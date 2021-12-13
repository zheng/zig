const std = @import("std");
const builtin = @import("builtin");
const macho = std.macho;
const TestContext = @import("src/link.zig").TestContext;
const CrossTarget = std.zig.CrossTarget;

const macos_x86_64 = CrossTarget{
    .cpu_arch = .x86_64,
    .os_tag = .macos,
};
const macos_aarch64 = CrossTarget{
    .cpu_arch = .aarch64,
    .os_tag = .macos,
};
const all_targets = &[_]CrossTarget{
    macos_x86_64,
    macos_aarch64,
};

pub fn addCases(ctx: *TestContext) !void {
    for (all_targets) |target| {
        {
            const case = try ctx.createCase("hello world in C", target);
            const exe = try case.createExe("hello");
            try exe.addCSource("main.c",
                \\#include <stdio.h>
                \\
                \\int main(int argc, char* argv[]) {
                \\    fprintf(stdout, "Hello, World!\n");
                \\    return 0;
                \\}
            , &.{});
            case.expectStdOut("Hello, World!\n");
        }

        {
            const case = try ctx.createCase("dylib", target);
            const dylib = try case.createShared("a");
            try dylib.addCSource("a.c",
                \\#include <stdio.h>
                \\char world[] = "world";
                \\
                \\char *hello() {
                \\  return "Hello";
                \\}
            , &.{});
            const exe = try case.createExe("main");
            try exe.addCSource("main.c",
                \\#include <stdio.h>
                \\
                \\char *hello();
                \\extern char world[];
                \\
                \\int main() {
                \\  printf("%s %s", hello(), world);
                \\}
            , &.{});
            try exe.linkShared(dylib);
            case.expectStdOut("Hello world");
        }

        {
            const case = try ctx.createCase("tls", target);
            const dylib = try case.createShared("a");
            try dylib.addCSource("a.c",
                \\_Thread_local int a;
            , &.{});
            const exe = try case.createExe("main");
            try exe.addCSource("b.c",
                \\#include <stdio.h>
                \\
                \\extern _Thread_local int a;
                \\
                \\int main() {
                \\  printf("%d", a);
                \\}
            , &.{});
            try exe.linkShared(dylib);
            case.expectStdOut("0");
        }

        {
            const case = try ctx.createCase("tls between in Zig and C", target);
            const exe = try case.createExe("main");
            exe.addZigSource("main.zig",
                \\const std = @import("std");
                \\
                \\threadlocal var globl: usize = 0;
                \\extern threadlocal var other: u32;
                \\
                \\pub fn main() void {
                \\    std.debug.print("{d}, {d}\n", .{globl, other});
                \\    globl += other;
                \\    other -= 1;
                \\    std.debug.print("{d}, {d}\n", .{globl, other});
                \\}
            );
            try exe.addCSource("a.c",
                \\_Thread_local int other = 10;
            , &.{});
            case.expectStdErr(
                \\0, 10
                \\10, 9
                \\
            );
        }

        {
            const case = try ctx.createCase("rpaths in exe", target);
            const exe = try case.createExe("main");
            try exe.addCSource("main.c",
                \\int main() {}
            , &.{});
            try exe.addLinkFlags(&.{ "-rpath", "foo", "-rpath", "@bar" });
            try exe.addQuery(.{
                .load_command = .{ .cmd = .RPATH, .grep = "path foo" },
            });
            try exe.addQuery(.{
                .load_command = .{ .cmd = .RPATH, .grep = "path @bar" },
            });
        }

        {
            const case = try ctx.createCase("common symbols", target);
            const exe = try case.createExe("main");
            exe.addZigSource("main.zig",
                \\const std = @import("std");
                \\
                \\extern fn common_defined_externally() c_int;
                \\extern fn incr_i() void;
                \\extern fn add_to_i_and_j(x: c_int) c_int;
                \\
                \\pub fn main() void {
                \\   std.debug.print("{d}\n", .{common_defined_externally()});
                \\   incr_i();
                \\   std.debug.print("{d}\n", .{add_to_i_and_j(2)});
                \\}
            );
            try exe.addCSource("a.c",
                \\int i;
                \\int j;
                \\
                \\int add_to_i_and_j(int x) {
                \\  return x + i + j;
                \\}
            , &.{"-fcommon"});
            try exe.addCSource("b.c",
                \\long i;
                \\int j = 2;
                \\int k;
                \\
                \\void incr_i() {
                \\  i++;
                \\}
            , &.{"-fcommon"});
            try exe.addCSource("c.c",
                \\extern int k;
                \\
                \\int common_defined_externally() {
                \\  return k;
                \\}
            , &.{"-fcommon"});
            case.expectStdErr(
                \\0
                \\5
                \\
            );
        }

        {
            const case = try ctx.createCase("common symbols alignment", target);
            const exe = try case.createExe("main");
            try exe.addCSource("a.c",
                \\int foo;
                \\__attribute__((aligned(4096))) int bar;
            , &.{"-fcommon"});
            try exe.addCSource("b.c",
                \\#include <stdio.h>
                \\#include <stdint.h>
                \\
                \\extern int foo;
                \\extern int bar;
                \\
                \\int main() {
                \\  printf("%lu %lu", (uintptr_t)&foo % 4, (uintptr_t)&bar % 4096);
                \\}
            , &.{});
            case.expectStdOut("0 0");
        }
    }
}
