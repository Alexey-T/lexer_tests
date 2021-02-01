const expect = @import("std").testing.expect;

test "bitCast to array" {
    comptime testBitCastArray();
    testBitCastArray();
}

fn testBitCastArray() void {
    expect(extractOne64(0x0123456789abcdef0123456789abcdef) == 0x0123456789abcdef);
}

fn extractOne64(a: u128) u64 {
    const x = @bitCast([2]u64, a);
    return x[1];
}
