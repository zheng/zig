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
            var case = try ctx.createCase("hello world in C", target);
            try case.addCSource("test.c",
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
            var case = try ctx.createCase("TLS support in Zig and C", target);
            case.addZigSource("main.zig",
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
            try case.addCSource("a.c",
                \\_Thread_local int other = 10;
            , &.{});
            case.expectStdErr(
                \\0, 10
                \\10, 9
                \\
            );
        }

        {
            var case = try ctx.createCase("rpaths in binary", target);
            try case.addCSource("main.c",
                \\int main() {}
            , &.{});
            case.setLinkFlags(&.{ "-rpath", "foo", "-rpath", "@bar" });
            try case.expectInBinary(.{
                .load_command = .{ .cmd = .RPATH, .grep = "path foo" },
            });
            try case.expectInBinary(.{
                .load_command = .{ .cmd = .RPATH, .grep = "path @bar" },
            });
        }

        {
            var case = try ctx.createCase("common symbols (tentative definitions)", target);
            case.addZigSource("main.zig",
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
            try case.addCSource("a.c",
                \\int i;
                \\int j;
                \\
                \\int add_to_i_and_j(int x) {
                \\  return x + i + j;
                \\}
            , &.{"-fcommon"});
            try case.addCSource("b.c",
                \\long i;
                \\int j = 2;
                \\int k;
                \\
                \\void incr_i() {
                \\  i++;
                \\}
            , &.{"-fcommon"});
            try case.addCSource("c.c",
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
    }
}
