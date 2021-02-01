const expect = @import("std").testing.expect;

const PrefixOp = union(enum) {
    Return,
    AddrOf: Value,
};

const Value = struct {
    align_expr: ?u32,
};

test "optional if after an if in a switch prong of a switch with 2 prongs in an else" {
    foo(false, true);
}

fn foo(a: bool, b: bool) void {
    var prefix_op = PrefixOp{
        .AddrOf = Value{ .align_expr = 1234 },
    };
    if (a) {} else {
        switch (prefix_op) {
            PrefixOp.AddrOf => |addr_of_info| {
                if (b) {}
                if (addr_of_info.align_expr) |align_expr| {
                    expect(align_expr == 1234);
                }
            },
            PrefixOp.Return => {},
        }
    }
}
