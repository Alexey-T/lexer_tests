const std = @import("std");
const builtin = std.builtin;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;
const maxInt = std.math.maxInt;
const StructWithNoFields = struct {
    fn add(a: i32, b: i32) i32 {
        return a + b;
    }
};
const empty_global_instance = StructWithNoFields{};

top_level_field: i32,

test "top level fields" {
    var instance = @This(){
        .top_level_field = 1234,
    };
    instance.top_level_field += 1;
    expectEqual(@as(i32, 1235), instance.top_level_field);
}

test "call struct static method" {
    const result = StructWithNoFields.add(3, 4);
    expect(result == 7);
}

test "return empty struct instance" {
    _ = returnEmptyStructInstance();
}
fn returnEmptyStructInstance() StructWithNoFields {
    return empty_global_instance;
}

const should_be_11 = StructWithNoFields.add(5, 6);

test "invoke static method in global scope" {
    expect(should_be_11 == 11);
}

test "void struct fields" {
    const foo = VoidStructFieldsFoo{
        .a = void{},
        .b = 1,
        .c = void{},
    };
    expect(foo.b == 1);
    expect(@sizeOf(VoidStructFieldsFoo) == 4);
}
const VoidStructFieldsFoo = struct {
    a: void,
    b: i32,
    c: void,
};

test "structs" {
    var foo: StructFoo = undefined;
    @memset(@ptrCast([*]u8, &foo), 0, @sizeOf(StructFoo));
    foo.a += 1;
    foo.b = foo.a == 1;
    testFoo(foo);
    testMutation(&foo);
    expect(foo.c == 100);
}
const StructFoo = struct {
    a: i32,
    b: bool,
    c: f32,
};
fn testFoo(foo: StructFoo) void {
    expect(foo.b);
}
fn testMutation(foo: *StructFoo) void {
    foo.c = 100;
}

const Node = struct {
    val: Val,
    next: *Node,
};

const Val = struct {
    x: i32,
};

test "struct point to self" {
    var root: Node = undefined;
    root.val.x = 1;

    var node: Node = undefined;
    node.next = &root;
    node.val.x = 2;

    root.next = &node;

    expect(node.next.next.next.val.x == 1);
}

test "struct byval assign" {
    var foo1: StructFoo = undefined;
    var foo2: StructFoo = undefined;

    foo1.a = 1234;
    foo2.a = 0;
    expect(foo2.a == 0);
    foo2 = foo1;
    expect(foo2.a == 1234);
}

fn structInitializer() void {
    const val = Val{ .x = 42 };
    expect(val.x == 42);
}

test "fn call of struct field" {
    const Foo = struct {
        ptr: fn () i32,
    };
    const S = struct {
        fn aFunc() i32 {
            return 13;
        }

        fn callStructField(foo: Foo) i32 {
            return foo.ptr();
        }
    };

    expect(S.callStructField(Foo{ .ptr = S.aFunc }) == 13);
}

test "store member function in variable" {
    const instance = MemberFnTestFoo{ .x = 1234 };
    const memberFn = MemberFnTestFoo.member;
    const result = memberFn(instance);
    expect(result == 1234);
}
const MemberFnTestFoo = struct {
    x: i32,
    fn member(foo: MemberFnTestFoo) i32 {
        return foo.x;
    }
};

test "call member function directly" {
    const instance = MemberFnTestFoo{ .x = 1234 };
    const result = MemberFnTestFoo.member(instance);
    expect(result == 1234);
}

test "member functions" {
    const r = MemberFnRand{ .seed = 1234 };
    expect(r.getSeed() == 1234);
}
const MemberFnRand = struct {
    seed: u32,
    pub fn getSeed(r: *const MemberFnRand) u32 {
        return r.seed;
    }
};

test "return struct byval from function" {
    const bar = makeBar(1234, 5678);
    expect(bar.y == 5678);
}
const Bar = struct {
    x: i32,
    y: i32,
};
fn makeBar(x: i32, y: i32) Bar {
    return Bar{
        .x = x,
        .y = y,
    };
}

test "empty struct method call" {
    const es = EmptyStruct{};
    expect(es.method() == 1234);
}
const EmptyStruct = struct {
    fn method(es: *const EmptyStruct) i32 {
        return 1234;
    }
};

test "return empty struct from fn" {
    _ = testReturnEmptyStructFromFn();
}
const EmptyStruct2 = struct {};
fn testReturnEmptyStructFromFn() EmptyStruct2 {
    return EmptyStruct2{};
}

test "pass slice of empty struct to fn" {
    expect(testPassSliceOfEmptyStructToFn(&[_]EmptyStruct2{EmptyStruct2{}}) == 1);
}
fn testPassSliceOfEmptyStructToFn(slice: []const EmptyStruct2) usize {
    return slice.len;
}

const APackedStruct = packed struct {
    x: u8,
    y: u8,
};

test "packed struct" {
    var foo = APackedStruct{
        .x = 1,
        .y = 2,
    };
    foo.y += 1;
    const four = foo.x + foo.y;
    expect(four == 4);
}

const BitField1 = packed struct {
    a: u3,
    b: u3,
    c: u2,
};

const bit_field_1 = BitField1{
    .a = 1,
    .b = 2,
    .c = 3,
};

test "bit field access" {
    var data = bit_field_1;
    expect(getA(&data) == 1);
    expect(getB(&data) == 2);
    expect(getC(&data) == 3);
    comptime expect(@sizeOf(BitField1) == 1);

    data.b += 1;
    expect(data.b == 3);

    data.a += 1;
    expect(data.a == 2);
    expect(data.b == 3);
}

fn getA(data: *const BitField1) u3 {
    return data.a;
}

fn getB(data: *const BitField1) u3 {
    return data.b;
}

fn getC(data: *const BitField1) u2 {
    return data.c;
}

const Foo24Bits = packed struct {
    field: u24,
};
const Foo96Bits = packed struct {
    a: u24,
    b: u24,
    c: u24,
    d: u24,
};

test "packed struct 24bits" {
    comptime {
        expect(@sizeOf(Foo24Bits) == 4);
        if (@sizeOf(usize) == 4) {
            expect(@sizeOf(Foo96Bits) == 12);
        } else {
            expect(@sizeOf(Foo96Bits) == 16);
        }
    }

    var value = Foo96Bits{
        .a = 0,
        .b = 0,
        .c = 0,
        .d = 0,
    };
    value.a += 1;
    expect(value.a == 1);
    expect(value.b == 0);
    expect(value.c == 0);
    expect(value.d == 0);

    value.b += 1;
    expect(value.a == 1);
    expect(value.b == 1);
    expect(value.c == 0);
    expect(value.d == 0);

    value.c += 1;
    expect(value.a == 1);
    expect(value.b == 1);
    expect(value.c == 1);
    expect(value.d == 0);

    value.d += 1;
    expect(value.a == 1);
    expect(value.b == 1);
    expect(value.c == 1);
    expect(value.d == 1);
}

const Foo32Bits = packed struct {
    field: u24,
    pad: u8,
};

const FooArray24Bits = packed struct {
    a: u16,
    b: [2]Foo32Bits,
    c: u16,
};

// TODO revisit this test when doing https://github.com/ziglang/zig/issues/1512
test "packed array 24bits" {
    comptime {
        expect(@sizeOf([9]Foo32Bits) == 9 * 4);
        expect(@sizeOf(FooArray24Bits) == 2 + 2 * 4 + 2);
    }

    var bytes = [_]u8{0} ** (@sizeOf(FooArray24Bits) + 1);
    bytes[bytes.len - 1] = 0xaa;
    const ptr = &std.mem.bytesAsSlice(FooArray24Bits, bytes[0 .. bytes.len - 1])[0];
    expect(ptr.a == 0);
    expect(ptr.b[0].field == 0);
    expect(ptr.b[1].field == 0);
    expect(ptr.c == 0);

    ptr.a = maxInt(u16);
    expect(ptr.a == maxInt(u16));
    expect(ptr.b[0].field == 0);
    expect(ptr.b[1].field == 0);
    expect(ptr.c == 0);

    ptr.b[0].field = maxInt(u24);
    expect(ptr.a == maxInt(u16));
    expect(ptr.b[0].field == maxInt(u24));
    expect(ptr.b[1].field == 0);
    expect(ptr.c == 0);

    ptr.b[1].field = maxInt(u24);
    expect(ptr.a == maxInt(u16));
    expect(ptr.b[0].field == maxInt(u24));
    expect(ptr.b[1].field == maxInt(u24));
    expect(ptr.c == 0);

    ptr.c = maxInt(u16);
    expect(ptr.a == maxInt(u16));
    expect(ptr.b[0].field == maxInt(u24));
    expect(ptr.b[1].field == maxInt(u24));
    expect(ptr.c == maxInt(u16));

    expect(bytes[bytes.len - 1] == 0xaa);
}

const FooStructAligned = packed struct {
    a: u8,
    b: u8,
};

const FooArrayOfAligned = packed struct {
    a: [2]FooStructAligned,
};

test "aligned array of packed struct" {
    comptime {
        expect(@sizeOf(FooStructAligned) == 2);
        expect(@sizeOf(FooArrayOfAligned) == 2 * 2);
    }

    var bytes = [_]u8{0xbb} ** @sizeOf(FooArrayOfAligned);
    const ptr = &std.mem.bytesAsSlice(FooArrayOfAligned, bytes[0..])[0];

    expect(ptr.a[0].a == 0xbb);
    expect(ptr.a[0].b == 0xbb);
    expect(ptr.a[1].a == 0xbb);
    expect(ptr.a[1].b == 0xbb);
}

test "runtime struct initialization of bitfield" {
    const s1 = Nibbles{
        .x = x1,
        .y = x1,
    };
    const s2 = Nibbles{
        .x = @intCast(u4, x2),
        .y = @intCast(u4, x2),
    };

    expect(s1.x == x1);
    expect(s1.y == x1);
    expect(s2.x == @intCast(u4, x2));
    expect(s2.y == @intCast(u4, x2));
}

var x1 = @as(u4, 1);
var x2 = @as(u8, 2);

const Nibbles = packed struct {
    x: u4,
    y: u4,
};

const Bitfields = packed struct {
    f1: u16,
    f2: u16,
    f3: u8,
    f4: u8,
    f5: u4,
    f6: u4,
    f7: u8,
};

test "native bit field understands endianness" {
    var all: u64 = if (builtin.endian != .Little)
        0x1111222233445677
    else
        0x7765443322221111;
    var bytes: [8]u8 = undefined;
    @memcpy(&bytes, @ptrCast([*]u8, &all), 8);
    var bitfields = @ptrCast(*Bitfields, &bytes).*;

    expect(bitfields.f1 == 0x1111);
    expect(bitfields.f2 == 0x2222);
    expect(bitfields.f3 == 0x33);
    expect(bitfields.f4 == 0x44);
    expect(bitfields.f5 == 0x5);
    expect(bitfields.f6 == 0x6);
    expect(bitfields.f7 == 0x77);
}

test "align 1 field before self referential align 8 field as slice return type" {
    const result = alloc(Expr);
    expect(result.len == 0);
}

const Expr = union(enum) {
    Literal: u8,
    Question: *Expr,
};

fn alloc(comptime T: type) []T {
    return &[_]T{};
}

test "call method with mutable reference to struct with no fields" {
    const S = struct {
        fn doC(s: *const @This()) bool {
            return true;
        }
        fn do(s: *@This()) bool {
            return true;
        }
    };

    var s = S{};
    expect(S.doC(&s));
    expect(s.doC());
    expect(S.do(&s));
    expect(s.do());
}

test "implicit cast packed struct field to const ptr" {
    const LevelUpMove = packed struct {
        move_id: u9,
        level: u7,

        fn toInt(value: u7) u7 {
            return value;
        }
    };

    var lup: LevelUpMove = undefined;
    lup.level = 12;
    const res = LevelUpMove.toInt(lup.level);
    expect(res == 12);
}

test "pointer to packed struct member in a stack variable" {
    const S = packed struct {
        a: u2,
        b: u2,
    };

    var s = S{ .a = 2, .b = 0 };
    var b_ptr = &s.b;
    expect(s.b == 0);
    b_ptr.* = 2;
    expect(s.b == 2);
}

test "non-byte-aligned array inside packed struct" {
    const Foo = packed struct {
        a: bool,
        b: [0x16]u8,
    };
    const S = struct {
        fn bar(slice: []const u8) void {
            expectEqualSlices(u8, slice, "abcdefghijklmnopqurstu");
        }
        fn doTheTest() void {
            var foo = Foo{
                .a = true,
                .b = "abcdefghijklmnopqurstu".*,
            };
            const value = foo.b;
            bar(&value);
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "packed struct with u0 field access" {
    const S = packed struct {
        f0: u0,
    };
    var s = S{ .f0 = 0 };
    comptime expect(s.f0 == 0);
}

const S0 = struct {
    bar: S1,

    pub const S1 = struct {
        value: u8,
    };

    fn init() @This() {
        return S0{ .bar = S1{ .value = 123 } };
    }
};

var g_foo: S0 = S0.init();

test "access to global struct fields" {
    g_foo.bar.value = 42;
    expect(g_foo.bar.value == 42);
}

test "packed struct with fp fields" {
    const S = packed struct {
        data: [3]f32,

        pub fn frob(self: *@This()) void {
            self.data[0] += self.data[1] + self.data[2];
            self.data[1] += self.data[0] + self.data[2];
            self.data[2] += self.data[0] + self.data[1];
        }
    };

    var s: S = undefined;
    s.data[0] = 1.0;
    s.data[1] = 2.0;
    s.data[2] = 3.0;
    s.frob();
    expectEqual(@as(f32, 6.0), s.data[0]);
    expectEqual(@as(f32, 11.0), s.data[1]);
    expectEqual(@as(f32, 20.0), s.data[2]);
}

test "use within struct scope" {
    const S = struct {
        usingnamespace struct {
            pub fn inner() i32 {
                return 42;
            }
        };
    };
    expectEqual(@as(i32, 42), S.inner());
}

test "default struct initialization fields" {
    const S = struct {
        a: i32 = 1234,
        b: i32,
    };
    const x = S{
        .b = 5,
    };
    if (x.a + x.b != 1239) {
        @compileError("it should be comptime known");
    }
    var five: i32 = 5;
    const y = S{
        .b = five,
    };
    expectEqual(1239, x.a + x.b);
}

test "fn with C calling convention returns struct by value" {
    const S = struct {
        fn entry() void {
            var x = makeBar(10);
            expectEqual(@as(i32, 10), x.handle);
        }

        const ExternBar = extern struct {
            handle: i32,
        };

        fn makeBar(t: i32) callconv(.C) ExternBar {
            return ExternBar{
                .handle = t,
            };
        }
    };
    S.entry();
    comptime S.entry();
}

test "for loop over pointers to struct, getting field from struct pointer" {
    const S = struct {
        const Foo = struct {
            name: []const u8,
        };

        var ok = true;

        fn eql(a: []const u8) bool {
            return true;
        }

        const ArrayList = struct {
            fn toSlice(self: *ArrayList) []*Foo {
                return @as([*]*Foo, undefined)[0..0];
            }
        };

        fn doTheTest() void {
            var objects: ArrayList = undefined;

            for (objects.toSlice()) |obj| {
                if (eql(obj.name)) {
                    ok = false;
                }
            }

            expect(ok);
        }
    };
    S.doTheTest();
}

test "zero-bit field in packed struct" {
    const S = packed struct {
        x: u10,
        y: void,
    };
    var x: S = undefined;
}

test "struct field init with catch" {
    const S = struct {
        fn doTheTest() void {
            var x: anyerror!isize = 1;
            var req = Foo{
                .field = x catch undefined,
            };
            expect(req.field == 1);
        }

        pub const Foo = extern struct {
            field: isize,
        };
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "packed struct with non-ABI-aligned field" {
    const S = packed struct {
        x: u9,
        y: u183,
    };
    var s: S = undefined;
    s.x = 1;
    s.y = 42;
    expect(s.x == 1);
    expect(s.y == 42);
}

test "non-packed struct with u128 entry in union" {
    const U = union(enum) {
        Num: u128,
        Void,
    };

    const S = struct {
        f1: U,
        f2: U,
    };

    var sx: S = undefined;
    var s = &sx;
    std.testing.expect(@ptrToInt(&s.f2) - @ptrToInt(&s.f1) == @byteOffsetOf(S, "f2"));
    var v2 = U{ .Num = 123 };
    s.f2 = v2;
    std.testing.expect(s.f2.Num == 123);
}

test "packed struct field passed to generic function" {
    const S = struct {
        const P = packed struct {
            b: u5,
            g: u5,
            r: u5,
            a: u1,
        };

        fn genericReadPackedField(ptr: anytype) u5 {
            return ptr.*;
        }
    };

    var p: S.P = undefined;
    p.b = 29;
    var loaded = S.genericReadPackedField(&p.b);
    expect(loaded == 29);
}

test "anonymous struct literal syntax" {
    const S = struct {
        const Point = struct {
            x: i32,
            y: i32,
        };

        fn doTheTest() void {
            var p: Point = .{
                .x = 1,
                .y = 2,
            };
            expect(p.x == 1);
            expect(p.y == 2);
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "fully anonymous struct" {
    const S = struct {
        fn doTheTest() void {
            dump(.{
                .int = @as(u32, 1234),
                .float = @as(f64, 12.34),
                .b = true,
                .s = "hi",
            });
        }
        fn dump(args: anytype) void {
            expect(args.int == 1234);
            expect(args.float == 12.34);
            expect(args.b);
            expect(args.s[0] == 'h');
            expect(args.s[1] == 'i');
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "fully anonymous list literal" {
    const S = struct {
        fn doTheTest() void {
            dump(.{ @as(u32, 1234), @as(f64, 12.34), true, "hi" });
        }
        fn dump(args: anytype) void {
            expect(args.@"0" == 1234);
            expect(args.@"1" == 12.34);
            expect(args.@"2");
            expect(args.@"3"[0] == 'h');
            expect(args.@"3"[1] == 'i');
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "anonymous struct literal assigned to variable" {
    var vec = .{ @as(i32, 22), @as(i32, 55), @as(i32, 99) };
    expect(vec.@"0" == 22);
    expect(vec.@"1" == 55);
    expect(vec.@"2" == 99);
}

test "struct with var field" {
    const Point = struct {
        x: anytype,
        y: anytype,
    };
    const pt = Point{
        .x = 1,
        .y = 2,
    };
    expect(pt.x == 1);
    expect(pt.y == 2);
}

test "comptime struct field" {
    const T = struct {
        a: i32,
        comptime b: i32 = 1234,
    };

    var foo: T = undefined;
    comptime expect(foo.b == 1234);
}

test "anon struct literal field value initialized with fn call" {
    const S = struct {
        fn doTheTest() void {
            var x = .{foo()};
            expectEqualSlices(u8, x[0], "hi");
        }
        fn foo() []const u8 {
            return "hi";
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "self-referencing struct via array member" {
    const T = struct {
        children: [1]*@This(),
    };
    var x: T = undefined;
    x = T{ .children = .{&x} };
    expect(x.children[0] == &x);
}

test "struct with union field" {
    const Value = struct {
        ref: u32 = 2,
        kind: union(enum) {
            None: usize,
            Bool: bool,
        },
    };

    var True = Value{
        .kind = .{ .Bool = true },
    };
    expectEqual(@as(u32, 2), True.ref);
    expectEqual(true, True.kind.Bool);
}

test "type coercion of anon struct literal to struct" {
    const S = struct {
        const S2 = struct {
            A: u32,
            B: []const u8,
            C: void,
            D: Foo = .{},
        };

        const Foo = struct {
            field: i32 = 1234,
        };

        fn doTheTest() void {
            var y: u32 = 42;
            const t0 = .{ .A = 123, .B = "foo", .C = {} };
            const t1 = .{ .A = y, .B = "foo", .C = {} };
            const y0: S2 = t0;
            var y1: S2 = t1;
            expect(y0.A == 123);
            expect(std.mem.eql(u8, y0.B, "foo"));
            expect(y0.C == {});
            expect(y0.D.field == 1234);
            expect(y1.A == y);
            expect(std.mem.eql(u8, y1.B, "foo"));
            expect(y1.C == {});
            expect(y1.D.field == 1234);
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "type coercion of pointer to anon struct literal to pointer to struct" {
    const S = struct {
        const S2 = struct {
            A: u32,
            B: []const u8,
            C: void,
            D: Foo = .{},
        };

        const Foo = struct {
            field: i32 = 1234,
        };

        fn doTheTest() void {
            var y: u32 = 42;
            const t0 = &.{ .A = 123, .B = "foo", .C = {} };
            const t1 = &.{ .A = y, .B = "foo", .C = {} };
            const y0: *const S2 = t0;
            var y1: *const S2 = t1;
            expect(y0.A == 123);
            expect(std.mem.eql(u8, y0.B, "foo"));
            expect(y0.C == {});
            expect(y0.D.field == 1234);
            expect(y1.A == y);
            expect(std.mem.eql(u8, y1.B, "foo"));
            expect(y1.C == {});
            expect(y1.D.field == 1234);
        }
    };
    S.doTheTest();
    comptime S.doTheTest();
}

test "packed struct with undefined initializers" {
    const S = struct {
        const P = packed struct {
            a: u3,
            _a: u3 = undefined,
            b: u3,
            _b: u3 = undefined,
            c: u3,
            _c: u3 = undefined,
        };

        fn doTheTest() void {
            var p: P = undefined;
            p = P{ .a = 2, .b = 4, .c = 6 };
            // Make sure the compiler doesn't touch the unprefixed fields.
            expectEqual(@as(u3, 2), p.a);
            expectEqual(@as(u3, 4), p.b);
            expectEqual(@as(u3, 6), p.c);
        }
    };

    S.doTheTest();
    comptime S.doTheTest();
}
