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

    pub const InspectQuery = union(enum) {
        load_command: struct {
            cmd: u32,
            data: []const u8,
        },
    };

    pub const Case = struct {
        name: []const u8,
        target: CrossTarget,
        zig_source: ?File = null,
        c_sources: std.ArrayList(CSource),
        queries: std.ArrayList(InspectQuery),
        link_flags: []const []const u8 = &[0][]u8{},
        expected_out: ExpectedOut = .{},

        const ExpectedOut = struct {
            stdout: []const u8 = &[0]u8{},
            stderr: []const u8 = &[0]u8{},
        };

        const CSource = struct {
            file: File,
            flags: []const []const u8,
        };

        pub fn deinit(self: *Case) void {
            self.c_sources.deinit();
            self.queries.deinit();
        }

        pub fn addZigSource(self: *Case, basename: []const u8, bytes: []const u8) void {
            assert(self.zig_source == null);
            self.zig_source = .{
                .basename = basename,
                .bytes = bytes,
            };
        }

        pub fn addCSource(
            self: *Case,
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

        pub fn setLinkFlags(self: *Case, flags: []const []const u8) void {
            self.link_flags = flags;
        }

        pub fn expectStdOut(self: *Case, stdout: []const u8) void {
            self.expected_out.stdout = stdout;
        }

        pub fn expectStdErr(self: *Case, stderr: []const u8) void {
            self.expected_out.stderr = stderr;
        }

        pub fn expectInBinary(self: *Case, query: InspectQuery) !void {
            try self.queries.append(query);
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
            .c_sources = std.ArrayList(Case.CSource).init(testing.allocator),
            .queries = std.ArrayList(InspectQuery).init(testing.allocator),
        });
        return &self.cases.items[index];
    }

    fn run(self: *TestContext) !void {
        const host = try std.zig.system.NativeTargetInfo.detect(testing.allocator, .{});

        var fail_count: usize = 0;

        for (self.cases.items) |case| {
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
        _ = host;
        const target_info = try std.zig.system.NativeTargetInfo.detect(allocator, case.target);
        const target = target_info.target;

        var arena_allocator = std.heap.ArenaAllocator.init(allocator);
        defer arena_allocator.deinit();
        const arena = arena_allocator.allocator();

        const target_triple = try target.zigTriple(arena);
        _ = target_triple;

        var tmp = tmpDir(.{});
        // defer tmp.cleanup();

        var cache_dir = try tmp.dir.makeOpenPath("zig-cache", .{});
        defer cache_dir.close();

        const cwd = try fs.path.join(
            arena,
            &[_][]const u8{ ".", "zig-cache", "tmp", &tmp.sub_path },
        );

        if (case.zig_source) |zig_source| {
            try tmp.dir.writeFile(zig_source.basename, zig_source.bytes);
        }

        for (case.c_sources.items) |source| {
            try tmp.dir.writeFile(source.file.basename, source.file.bytes);
        }

        var zig_args = std.ArrayList([]const u8).init(arena);
        try zig_args.append(testing.zig_exe_path);

        try zig_args.append("build-exe");

        if (case.zig_source) |zig_source| {
            try zig_args.append(zig_source.basename);
        }

        for (case.c_sources.items) |source| {
            try zig_args.append(source.file.basename);
        }

        try zig_args.append("--name");
        try zig_args.append("test");

        try zig_args.append("-target");
        try zig_args.append(target_triple);

        try zig_args.appendSlice(case.link_flags);

        // try zig_args.append("-cflags");
        // for (case.c_sources.items) |source| {
        //     try zig_args.appendSlice(source.flags);
        // }

        dumpArgs(zig_args.items);

        const result = try std.ChildProcess.exec(.{
            .allocator = arena,
            .argv = zig_args.items,
            .cwd_dir = tmp.dir,
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

        var run_argv = std.ArrayList([]const u8).init(arena);

        switch (host.getExternalExecutor(target_info, .{})) {
            .native => {
                try run_argv.append("test");
            },
            .rosetta => if (enable_rosetta) {
                try run_argv.append("test");
            } else {
                return;
            },
            .qemu => |bin_name| if (enable_qemu) {
                const need_cross_glibc = case.target.isGnuLibC() and case.c_sources.items.len > 0;
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
                try run_argv.append("test");
            } else {
                return;
            },
            .wasmtime => |bin_name| if (enable_wasmtime) {
                try run_argv.append(bin_name);
                try run_argv.append("--dir=.");
                try run_argv.append("test");
            } else {
                return;
            },
            .wine => |bin_name| if (enable_wine) {
                try run_argv.append(bin_name);
                try run_argv.append("test");
            } else {
                return;
            },
            else => return,
        }

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
};

// const MachoParseAndInspectStep = struct {
//     pub const base_id = .custom;

//     step: build.Step,
//     builder: *build.Builder,
//     macho_file: *build.LibExeObjStep,
//     load_commands: ArrayList(macho.LoadCommand),

//     pub fn init(builder: *build.Builder, macho_file: *build.LibExeObjStep) MachoParseAndInspectStep {
//         return MachoParseAndInspectStep{
//             .builder = builder,
//             .step = build.Step.init(.custom, builder.fmt("MachoParseAndInspect {s}", .{
//                 macho_file.getOutputSource().getDisplayName(),
//             }), builder.allocator, make),
//             .macho_file = macho_file,
//             .load_commands = ArrayList(macho.LoadCommand).init(builder.allocator),
//         };
//     }

//     pub fn addExpectedLoadCommand(self: *MachoParseAndInspectStep, lc: macho.LoadCommand) void {
//         self.load_commands.append(lc) catch unreachable;
//     }

//     fn make(step: *build.Step) !void {
//         const self = @fieldParentPtr(MachoParseAndInspectStep, "step", step);
//         const executable_path = self.macho_file.installed_path orelse
//             self.macho_file.getOutputSource().getPath(self.builder);

//         const file = try fs.cwd().openFile(executable_path, .{});
//         defer file.close();

//         // TODO mmap the file, but remember to handle Windows as the host too.
//         // The test should be possible to perform on ANY OS!
//         const reader = file.reader();
//         const header = try reader.readStruct(macho.mach_header_64);
//         assert(header.filetype == macho.MH_EXECUTE or header.filetype == macho.MH_DYLIB);

//         var load_commands = ArrayList(macho.LoadCommand).init(self.builder.allocator);
//         try load_commands.ensureTotalCapacity(header.ncmds);

//         var i: u16 = 0;
//         while (i < header.ncmds) : (i += 1) {
//             var cmd = try macho.LoadCommand.read(self.builder.allocator, reader);
//             load_commands.appendAssumeCapacity(cmd);
//         }

//         outer: for (self.load_commands.items) |exp_lc| {
//             for (load_commands.items) |given_lc| {
//                 if (exp_lc.eql(given_lc)) continue :outer;
//             }

//             std.debug.print(
//                 \\
//                 \\======== Expected to find this load command: ========
//                 \\{}
//                 \\
//             , .{exp_lc});
//             return error.TestFailed;
//         }
//     }
// };

fn dumpArgs(argv: []const []const u8) void {
    for (argv) |arg| {
        print("{s} ", .{arg});
    }
    print("\n", .{});
}
