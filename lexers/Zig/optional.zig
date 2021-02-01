const std = @import("std");
const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;

pub const EmptyStruct = struct {};

test "optional pointer to size zero struct" {
    var e = EmptyStruct{};
    var o: ?*EmptyStruct = &e;
    expect(o != null);
}

test "equality compare nullable pointers" {
    testNullPtrsEql();
    comptime testNullPtrsEql();
}

fn testNullPtrsEql() void {
    var number: i32 = 1234;

    var x: ?*i32 = null;
    var y: ?*i32 = null;
    expect(x == y);
    y = &number;
    expect(x != y);
    expect(x != &number);
    expect(&number != x);
    x = &number;
    expect(x == y);
    expect(x == &number);
    expect(&number == x);
}

test "address of unwrap optional" {
    const S = struct {
        const Foo = struct {
            a: i32,
        };

        var global: ?Foo = null;

        pub fn getFoo() anyerror!*Foo {
            return &global.?;
        }
    };
    S.global = S.Foo{ .a = 1234 };
    const foo = S.getFoo() catch unreachable;
    expect(foo.a == 1234);
}

test "equality compare optional with non-optional" {
    test_cmp_optional_non_optional();
    comptime test_cmp_optional_non_optional();
}

fn test_cmp_optional_non_optional() void {
    var ten: i32 = 10;
    var opt_ten: ?i32 = 10;
    var five: i32 = 5;
    var int_n: ?i32 = null;

    expect(int_n != ten);
    expect(opt_ten == ten);
    expect(opt_ten != five);

    // test evaluation is always lexical
    // ensure that the optional isn't always computed before the non-optional
    var mutable_state: i32 = 0;
    _ = blk1: {
        mutable_state += 1;
        break :blk1 @as(?f64, 10.0);
    } != blk2: {
        expect(mutable_state == 1);
        break :blk2 @as(f64, 5.0);
    };
    _ = blk1: {
        mutable_state += 1;
        break :blk1 @as(f64, 10.0);
    } != blk2: {
        expect(mutable_state == 2);
        break :blk2 @as(?f64, 5.0);
    };
}

test "passing an optional integer as a parameter" {
    const S = struct {
        fn entry() bool {
            var x: i32 = 1234;
            return foo(x);
        }

        fn foo(x: ?i32) bool {
            return x.? == 1234;
        }
    };
    expect(S.entry());
    comptime expect(S.entry());
}

test "unwrap function call with optional pointer return value" {
    const S = struct {
        fn entry() void {
            expect(foo().?.* == 1234);
            expect(bar() == null);
        }
        const global: i32 = 1234;
        fn foo() ?*const i32 {
            return &global;
        }
        fn bar() ?*i32 {
            return null;
        }
    };
    S.entry();
    comptime S.entry();
}

test "nested orelse" {
    const S = struct {
        fn entry() void {
            expect(func() == null);
        }
        fn maybe() ?Foo {
            return null;
        }
        fn func() ?Foo {
            const x = maybe() orelse
                maybe() orelse
                return null;
            unreachable;
        }
        const Foo = struct {
            field: i32,
        };
    };
    S.entry();
    comptime S.entry();
}

test "self-referential struct through a slice of optional" {
    const S = struct {
        const Node = struct {
            children: []?Node,
            data: ?u8,

            fn new() Node {
                return Node{
                    .children = undefined,
                    .data = null,
                };
            }
        };
    };

    var n = S.Node.new();
    expect(n.data == null);
}

test "assigning to an unwrapped optional field in an inline loop" {
    comptime var maybe_pos_arg: ?comptime_int = null;
    inline for ("ab") |x| {
        maybe_pos_arg = 0;
        if (maybe_pos_arg.? != 0) {
            @compileError("bad");
        }
        maybe_pos_arg.? = 10;
    }
}

test "coerce an anon struct literal to optional struct" {
    const S = struct {
        const Struct = struct {
            field: u32,
        };
        export fn doTheTest() void {
            var maybe_dims: ?Struct = null;
            maybe_dims = .{ .field = 1 };
            expect(maybe_dims.?.field == 1);
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "optional with void type" {
    const Foo = struct {
        x: ?void,
    };
    var x = Foo{ .x = null };
    expect(x.x == null);
}

test "0-bit child type coerced to optional return ptr result location" {
    const S = struct {
        fn doTheTest() void {
            var y = Foo{};
            var z = y.thing();
            expect(z != null);
        }

        const Foo = struct {
            pub const Bar = struct {
                field: *Foo,
            };

            pub fn thing(self: *Foo) ?Bar {
                return Bar{ .field = self };
            }
        };
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "0-bit child type coerced to optional" {
    const S = struct {
        fn doTheTest() void {
            var it: Foo = .{
                .list = undefined,
            };
            expect(it.foo() != null);
        }

        const Empty = struct {};
        const Foo = struct {
            list: [10]Empty,

            fn foo(self: *Foo) ?*Empty {
                const data = &self.list[0];
                return data;
            }
        };
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "array of optional unaligned types" {
    const Enum = enum { one, two, three };

    const SomeUnion = union(enum) {
        Num: Enum,
        Other: u32,
    };

    const values = [_]?SomeUnion{
        SomeUnion{ .Num = .one },
        SomeUnion{ .Num = .two },
        SomeUnion{ .Num = .three },
        SomeUnion{ .Num = .one },
        SomeUnion{ .Num = .two },
        SomeUnion{ .Num = .three },
    };

    // The index must be a runtime value
    var i: usize = 0;
    expectEqual(Enum.one, values[i].?.Num);
    i += 1;
    expectEqual(Enum.two, values[i].?.Num);
    i += 1;
    expectEqual(Enum.three, values[i].?.Num);
    i += 1;
    expectEqual(Enum.one, values[i].?.Num);
    i += 1;
    expectEqual(Enum.two, values[i].?.Num);
    i += 1;
    expectEqual(Enum.three, values[i].?.Num);
}
