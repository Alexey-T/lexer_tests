const std = @import("std");

test "fixed" {
    const x: f32 align(128) = 12.34;
    std.testing.expect(@ptrToInt(&x) % 128 == 0);
}
