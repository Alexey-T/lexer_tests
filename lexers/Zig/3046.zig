const std = @import("std");
const expect = std.testing.expect;

const SomeStruct = struct {
    field: i32,
};

fn couldFail() anyerror!i32 {
    return 1;
}

var some_struct: SomeStruct = undefined;

test "fixed" {
    some_struct = SomeStruct{
        .field = couldFail() catch |_| @as(i32, 0),
    };
    expect(some_struct.field == 1);
}
