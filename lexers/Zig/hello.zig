const std = @import("std");

pub fn main() !void {
    // If this program is run without stdout attached, exit with an error.
    const stdout_file = std.io.getStdOut();
    // If this program encounters pipe failure when printing to stdout, exit
    // with an error.
    try stdout_file.writeAll("Hello, world!\n");
}
