const expect = @import("std").testing.expect;

test "Peer type resolution with string literals and unknown length u8 pointers" {
    expect(@TypeOf("", "a", @as([*:0]const u8, "")) == [*:0]const u8);
    expect(@TypeOf(@as([*:0]const u8, "baz"), "foo", "bar") == [*:0]const u8);
}
