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
            case.setLinkFlags(&.{ "-rpath", "foo" });
            try case.expectInBinary(.{
                .load_command = .{ .cmd = macho.LC_RPATH, .data = "foo" },
            });
        }
    }
}
