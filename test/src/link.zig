const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const assert = std.debug.assert;
const io = std.io;
const fs = std.fs;
const macho = std.macho;
const mem = std.mem;
const fmt = std.fmt;
const print = std.debug.print;
const tmpDir = std.testing.tmpDir;
const testing = std.testing;
const enable_qemu: bool = build_options.enable_qemu;
const enable_wasmtime: bool = build_options.enable_wasmtime;
const enable_wine: bool = build_options.enable_wine;
const enable_darling: bool = build_options.enable_darling;
const enable_rosetta: bool = build_options.enable_rosetta;
const glibc_runtimes_dir: ?[]const u8 = build_options.glibc_runtimes_dir;

const Allocator = mem.Allocator;
const CrossTarget = std.zig.CrossTarget;

test {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try @import("test_cases").addCases(&ctx);

    try ctx.run();
}

pub const TestContext = struct {
    cases: std.ArrayList(Case),

    pub const File = struct {
        bytes: []const u8,
        basename: []const u8,
    };

    pub const CSource = struct {
        file: File,
        flags: []const []const u8,
    };

    pub const ExpectedOut = struct {
        stdout: []const u8 = &[0]u8{},
        stderr: []const u8 = &[0]u8{},
    };

    pub const InspectQuery = union(enum) {
        load_command: struct {
            cmd: macho.LC,
            grep: []const u8,
        },
    };

    pub const Artifact = struct {
        tag: Tag,
        basename: []const u8,
        zig_source: ?File = null,
        c_sources: std.ArrayList(CSource),
        queries: std.ArrayList(InspectQuery),
        link_objects: std.ArrayList(*const Artifact),
        link_flags: std.ArrayList([]const u8),

        const Tag = enum {
            exe,
            shared,
            archive,
        };

        pub fn deinit(self: *Artifact) void {
            self.c_sources.deinit();
            self.queries.deinit();
            self.link_objects.deinit();
            self.link_flags.deinit();
        }

        pub fn addZigSource(self: *Artifact, basename: []const u8, bytes: []const u8) void {
            assert(self.zig_source == null);
            self.zig_source = .{
                .basename = basename,
                .bytes = bytes,
            };
        }

        pub fn addCSource(
            self: *Artifact,
            basename: []const u8,
            bytes: []const u8,
            flags: []const []const u8,
        ) !void {
            try self.c_sources.append(.{
                .file = .{
                    .basename = basename,
                    .bytes = bytes,
                },
                .flags = flags,
            });
        }

        pub fn installName(self: Artifact, allocator: Allocator, target: std.Target) error{OutOfMemory}![]const u8 {
            return switch (self.tag) {
                .exe => fmt.allocPrint(allocator, "{s}{s}", .{ self.basename, target.exeFileExt() }),
                .shared => fmt.allocPrint(allocator, "{s}{s}{s}", .{
                    target.libPrefix(),
                    self.basename,
                    target.dynamicLibSuffix(),
                }),
                .archive => fmt.allocPrint(allocator, "{s}{s}{s}", .{
                    target.libPrefix(),
                    self.basename,
                    target.staticLibSuffix(),
                }),
            };
        }

        pub fn addLinkFlags(self: *Artifact, flags: []const []const u8) !void {
            try self.link_flags.appendSlice(flags);
        }

        pub fn addQuery(self: *Artifact, query: InspectQuery) !void {
            try self.queries.append(query);
        }

        pub fn linkArchive(self: *Artifact, archive: *const Artifact) !void {
            try self.link_objects.append(archive);
        }

        pub fn linkShared(self: *Artifact, shared: *const Artifact) !void {
            try self.link_objects.append(shared);
            try self.link_flags.append("-rpath");
            try self.link_flags.append(".");
        }
    };

    pub const Case = struct {
        name: []const u8,
        target: CrossTarget,
        artifacts: std.ArrayList(Artifact),
        expected_out: ExpectedOut = .{},

        pub fn deinit(self: *Case) void {
            for (self.artifacts.items) |*artifact| {
                artifact.deinit();
            }
            self.artifacts.deinit();
        }

        pub fn createExe(self: *Case, basename: []const u8) !*Artifact {
            return self.createArtifact(basename, .exe);
        }

        pub fn createShared(self: *Case, basename: []const u8) !*Artifact {
            return self.createArtifact(basename, .shared);
        }

        fn createArtifact(self: *Case, basename: []const u8, tag: Artifact.Tag) !*Artifact {
            const index = self.artifacts.items.len;
            try self.artifacts.append(Artifact{
                .tag = tag,
                .basename = basename,
                .c_sources = std.ArrayList(CSource).init(testing.allocator),
                .queries = std.ArrayList(InspectQuery).init(testing.allocator),
                .link_objects = std.ArrayList(*const Artifact).init(testing.allocator),
                .link_flags = std.ArrayList([]const u8).init(testing.allocator),
            });
            return &self.artifacts.items[index];
        }

        pub fn expectStdOut(self: *Case, stdout: []const u8) void {
            self.expected_out.stdout = stdout;
        }

        pub fn expectStdErr(self: *Case, stderr: []const u8) void {
            self.expected_out.stderr = stderr;
        }
    };

    fn init() TestContext {
        var cases = std.ArrayList(Case).init(testing.allocator);
        return .{ .cases = cases };
    }

    fn deinit(self: *TestContext) void {
        for (self.cases.items) |*case| {
            case.deinit();
        }
        self.cases.deinit();
    }

    pub fn createCase(self: *TestContext, name: []const u8, target: CrossTarget) !*Case {
        const index = self.cases.items.len;
        try self.cases.append(Case{
            .name = name,
            .target = target,
            .artifacts = std.ArrayList(Artifact).init(testing.allocator),
        });
        return &self.cases.items[index];
    }

    fn run(self: *TestContext) !void {
        const host = try std.zig.system.NativeTargetInfo.detect(testing.allocator, .{});

        var progress = std.Progress{};
        const root_node = try progress.start("linker", self.cases.items.len);
        defer root_node.end();

        var fail_count: usize = 0;

        for (self.cases.items) |case| {
            var prg_node = root_node.start(case.name, 1);
            prg_node.activate();
            defer prg_node.end();

            runOneCase(testing.allocator, case, host) catch |err| {
                fail_count += 1;
                print("test '{s}' failed: {s}\n\n", .{ case.name, @errorName(err) });
            };
        }

        if (fail_count > 0) {
            print("{d} tests failed\n", .{fail_count});
            return error.TestFailed;
        }
    }

    fn runOneCase(allocator: Allocator, case: Case, host: std.zig.system.NativeTargetInfo) !void {
        const target_info = try std.zig.system.NativeTargetInfo.detect(allocator, case.target);
        const target = target_info.target;

        var arena_allocator = std.heap.ArenaAllocator.init(allocator);
        defer arena_allocator.deinit();
        const arena = arena_allocator.allocator();

        var tmp = tmpDir(.{});
        // defer tmp.cleanup();

        var cache_dir = try tmp.dir.makeOpenPath("zig-cache", .{});
        defer cache_dir.close();

        const cwd = try fs.path.join(
            arena,
            &[_][]const u8{ ".", "zig-cache", "tmp", &tmp.sub_path },
        );

        var exe_artifact: ?Artifact = null;
        for (case.artifacts.items) |artifact| {
            try buildArtifact(arena, artifact, target, tmp.dir, cwd);
            try inspectArtifact(arena, artifact, tmp.dir, target);

            if (artifact.tag == .exe) {
                if (exe_artifact) |exe| {
                    print("\nMultiple exe artifacts defined: {s} and {s}\n", .{
                        exe.basename,
                        artifact.basename,
                    });
                    return error.MultipleExeArtifacts;
                }
                exe_artifact = artifact;
            }
        }

        if (exe_artifact) |exe| {
            const exe_path = try fs.path.join(arena, &.{ ".", exe.basename });
            var run_argv = std.ArrayList([]const u8).init(arena);
            switch (host.getExternalExecutor(target_info, .{})) {
                .native => {},
                .rosetta => if (!enable_rosetta) return,
                .qemu => |bin_name| if (enable_qemu) {
                    const need_cross_glibc = case.target.isGnuLibC();
                    const glibc_dir_arg = if (need_cross_glibc)
                        glibc_runtimes_dir
                    else
                        null;

                    try run_argv.append(bin_name);
                    if (glibc_dir_arg) |dir| {
                        // TODO look into making this a call to `linuxTriple`. This
                        // needs the directory to be called "i686" rather than
                        // "i386" which is why we do it manually here.
                        const fmt_str = "{s}" ++ fs.path.sep_str ++ "{s}-{s}-{s}";
                        const cpu_arch = case.target.getCpuArch();
                        const os_tag = case.target.getOsTag();
                        const abi = case.target.getAbi();
                        const cpu_arch_name: []const u8 = if (cpu_arch == .i386)
                            "i686"
                        else
                            @tagName(cpu_arch);
                        const full_dir = try fmt.allocPrint(arena, fmt_str, .{
                            dir, cpu_arch_name, @tagName(os_tag), @tagName(abi),
                        });

                        try run_argv.append("-L");
                        try run_argv.append(full_dir);
                    }
                } else {
                    return;
                },
                .darling => |bin_name| if (enable_darling) {
                    try run_argv.append(bin_name);
                    try run_argv.append("shell");
                } else {
                    return;
                },
                .wasmtime => |bin_name| if (enable_wasmtime) {
                    try run_argv.append(bin_name);
                    try run_argv.append("--dir=.");
                } else {
                    return;
                },
                .wine => |bin_name| if (enable_wine) {
                    try run_argv.append(bin_name);
                } else {
                    return;
                },
                else => return,
            }
            try run_argv.append(exe_path);

            const exec_result = std.ChildProcess.exec(.{
                .allocator = arena,
                .argv = run_argv.items,
                .cwd_dir = tmp.dir,
                .cwd = cwd,
            }) catch |err| {
                print("\nThe following command failed with {s}:\n", .{@errorName(err)});
                dumpArgs(run_argv.items);
                return error.ChildProcessExecution;
            };
            switch (exec_result.term) {
                .Exited => |code| {
                    if (code != 0) {
                        print("\n{s}\n{s}: execution exited with code {d}:\n", .{
                            exec_result.stderr, case.name, code,
                        });
                        dumpArgs(run_argv.items);
                        return error.ChildProcessExecution;
                    }
                },
                else => {
                    print("\n{s}\n{s}: execution crashed:\n", .{
                        exec_result.stderr, case.name,
                    });
                    dumpArgs(run_argv.items);
                    return error.ChildProcessExecution;
                },
            }
            try testing.expectEqualStrings(case.expected_out.stdout, exec_result.stdout);
            try testing.expectEqualStrings(case.expected_out.stderr, exec_result.stderr);
        }
    }

    fn buildArtifact(
        arena: Allocator,
        artifact: Artifact,
        target: std.Target,
        cwd_dir: fs.Dir,
        cwd: []const u8,
    ) !void {
        var zig_args = std.ArrayList([]const u8).init(arena);
        try zig_args.append(testing.zig_exe_path);

        switch (artifact.tag) {
            .exe => try zig_args.append("build-exe"),
            .shared => {
                try zig_args.append("build-lib");
                try zig_args.append("-dynamic");
            },
            .archive => try zig_args.append("build-lib"),
        }

        if (artifact.zig_source) |zig_source| {
            try cwd_dir.writeFile(zig_source.basename, zig_source.bytes);
            try zig_args.append(zig_source.basename);
        }

        if (artifact.c_sources.items.len > 0) {
            for (artifact.c_sources.items) |source| {
                try cwd_dir.writeFile(source.file.basename, source.file.bytes);
                try zig_args.append("-cflags");
                try zig_args.appendSlice(source.flags);
                try zig_args.append("--");
                try zig_args.append(source.file.basename);
            }
        }

        try zig_args.append("--name");
        try zig_args.append(artifact.basename);

        const target_triple = try target.zigTriple(arena);
        try zig_args.append("-target");
        try zig_args.append(target_triple);

        try zig_args.ensureUnusedCapacity(artifact.link_objects.items.len);
        for (artifact.link_objects.items) |other| {
            const install_name = try other.installName(arena, target);
            zig_args.appendAssumeCapacity(install_name);
        }

        try zig_args.appendSlice(artifact.link_flags.items);

        // dumpArgs(zig_args.items);

        const result = try std.ChildProcess.exec(.{
            .allocator = arena,
            .argv = zig_args.items,
            .cwd_dir = cwd_dir,
            .cwd = cwd,
        });
        if (result.stdout.len != 0) {
            print("unexpected stdout: {s}", .{result.stdout});
        }
        if (result.stderr.len != 0) {
            print("unexpected stderr: {s}", .{result.stderr});
        }
        if (result.term != .Exited or result.term.Exited != 0) {
            print("{s}", .{result.stderr});
            dumpArgs(zig_args.items);
            return error.CompileError;
        }
    }

    fn inspectArtifact(arena: Allocator, artifact: Artifact, cwd_dir: fs.Dir, target: std.Target) !void {
        const install_name = try artifact.installName(arena, target);
        const file = try cwd_dir.openFile(install_name, .{});
        defer file.close();

        // TODO mmap the file, but remember to handle Windows as the host too.
        // The test should be possible to perform on ANY OS!
        const reader = file.reader();
        const header = try reader.readStruct(macho.mach_header_64);
        assert(header.filetype == macho.MH_EXECUTE or header.filetype == macho.MH_DYLIB);

        var load_commands = std.ArrayList(macho.LoadCommand).init(arena);
        try load_commands.ensureTotalCapacity(header.ncmds);

        var ncmd: u16 = 0;
        while (ncmd < header.ncmds) : (ncmd += 1) {
            var cmd = try macho.LoadCommand.read(arena, reader);
            load_commands.appendAssumeCapacity(cmd);
        }

        var lc_dump_cache = std.AutoHashMap(u16, []const u8).init(arena);

        for (artifact.queries.items) |query| {
            switch (query) {
                .load_command => |lc| {
                    for (load_commands.items) |given_lc, i| {
                        if (lc.cmd != given_lc.cmd()) continue;
                        const lc_dump = lc_dump_cache.get(@intCast(u16, i)) orelse blk: {
                            const lc_dump = try dumpLoadCommand(arena, given_lc);
                            try lc_dump_cache.putNoClobber(@intCast(u16, i), lc_dump);
                            break :blk lc_dump;
                        };
                        if (mem.indexOf(u8, lc_dump, lc.grep)) |_| {
                            break;
                        }
                    } else {
                        print(
                            \\
                            \\======== Expected to find this load command: ========
                            \\{} with data
                            \\  {s}
                            \\
                        , .{ lc.cmd, fmt.fmtSliceEscapeLower(lc.grep) });
                        return error.TestFailed;
                    }
                },
            }
        }
    }
};

fn dumpArgs(argv: []const []const u8) void {
    for (argv) |arg| {
        print("{s} ", .{arg});
    }
    print("\n", .{});
}

fn dumpLoadCommand(allocator: Allocator, lc: macho.LoadCommand) ![]const u8 {
    switch (lc.cmd()) {
        .RPATH => {
            const rpath = lc.rpath;
            const fmt_slice = try fmt.allocPrint(allocator,
                \\cmd LC_RPATH
                \\cmdsize {d}
                \\path {s}
            , .{ lc.cmdsize(), rpath.data });
            return fmt_slice;
        },
        else => return error.TODODumpMoreLoadCommands,
    }
}
