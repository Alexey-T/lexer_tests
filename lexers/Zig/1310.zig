const std = @import("std");
const expect = std.testing.expect;

pub const VM = ?[*]const struct_InvocationTable_;
pub const struct_InvocationTable_ = extern struct {
    GetVM: ?fn (?[*]VM) callconv(.C) c_int,
};

pub const struct_VM_ = extern struct {
    functions: ?[*]const struct_InvocationTable_,
};

//excised output from stdlib.h etc

pub const InvocationTable_ = struct_InvocationTable_;
pub const VM_ = struct_VM_;

fn agent_callback(_vm: [*]VM, options: [*]u8) callconv(.C) i32 {
    return 11;
}

test "fixed" {
    expect(agent_callback(undefined, undefined) == 11);
}
