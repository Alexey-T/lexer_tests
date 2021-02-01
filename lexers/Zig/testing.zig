// SPDX-License-Identifier: MIT
// Copyright (c) 2015-2021 Zig Contributors
// This file is part of [zig](https://ziglang.org/), which is MIT licensed.
// The MIT license requires this copyright notice to be included in all copies
// and substantial portions of the software.
const std = @import("std.zig");
const math = std.math;
const print = std.debug.print;

pub const FailingAllocator = @import("testing/failing_allocator.zig").FailingAllocator;

/// This should only be used in temporary test programs.
pub const allocator = &allocator_instance.allocator;
pub var allocator_instance = std.heap.GeneralPurposeAllocator(.{}){};

pub const failing_allocator = &failing_allocator_instance.allocator;
pub var failing_allocator_instance = FailingAllocator.init(&base_allocator_instance.allocator, 0);

pub var base_allocator_instance = std.heap.FixedBufferAllocator.init("");

/// TODO https://github.com/ziglang/zig/issues/5738
pub var log_level = std.log.Level.warn;

/// This is available to any test that wants to execute Zig in a child process.
/// It will be the same executable that is running `zig test`.
pub var zig_exe_path: []const u8 = undefined;

/// This function is intended to be used only in tests. It prints diagnostics to stderr
/// and then aborts when actual_error_union is not expected_error.
pub fn expectError(expected_error: anyerror, actual_error_union: anytype) void {
    if (actual_error_union) |actual_payload| {
        std.debug.panic("expected error.{s}, found {}", .{ @errorName(expected_error), actual_payload });
    } else |actual_error| {
        if (expected_error != actual_error) {
            std.debug.panic("expected error.{s}, found error.{s}", .{
                @errorName(expected_error),
                @errorName(actual_error),
            });
        }
    }
}

/// This function is intended to be used only in tests. When the two values are not
/// equal, prints diagnostics to stderr to show exactly how they are not equal,
/// then aborts.
/// `actual` is casted to the type of `expected`.
pub fn expectEqual(expected: anytype, actual: @TypeOf(expected)) void {
    switch (@typeInfo(@TypeOf(actual))) {
        .NoReturn,
        .BoundFn,
        .Opaque,
        .Frame,
        .AnyFrame,
        => @compileError("value of type " ++ @typeName(@TypeOf(actual)) ++ " encountered"),

        .Undefined,
        .Null,
        .Void,
        => return,

        .Type => {
            if (actual != expected) {
                std.debug.panic("expected type {s}, found type {s}", .{ @typeName(expected), @typeName(actual) });
            }
        },

        .Bool,
        .Int,
        .Float,
        .ComptimeFloat,
        .ComptimeInt,
        .EnumLiteral,
        .Enum,
        .Fn,
        .ErrorSet,
        => {
            if (actual != expected) {
                std.debug.panic("expected {}, found {}", .{ expected, actual });
            }
        },

        .Pointer => |pointer| {
            switch (pointer.size) {
                .One, .Many, .C => {
                    if (actual != expected) {
                        std.debug.panic("expected {*}, found {*}", .{ expected, actual });
                    }
                },
                .Slice => {
                    if (actual.ptr != expected.ptr) {
                        std.debug.panic("expected slice ptr {}, found {}", .{ expected.ptr, actual.ptr });
                    }
                    if (actual.len != expected.len) {
                        std.debug.panic("expected slice len {}, found {}", .{ expected.len, actual.len });
                    }
                },
            }
        },

        .Array => |array| expectEqualSlices(array.child, &expected, &actual),

        .Vector => |vectorType| {
            var i: usize = 0;
            while (i < vectorType.len) : (i += 1) {
                if (!std.meta.eql(expected[i], actual[i])) {
                    std.debug.panic("index {} incorrect. expected {}, found {}", .{ i, expected[i], actual[i] });
                }
            }
        },

        .Struct => |structType| {
            inline for (structType.fields) |field| {
                expectEqual(@field(expected, field.name), @field(actual, field.name));
            }
        },

        .Union => |union_info| {
            if (union_info.tag_type == null) {
                @compileError("Unable to compare untagged union values");
            }

            const TagType = @TagType(@TypeOf(expected));

            const expectedTag = @as(TagType, expected);
            const actualTag = @as(TagType, actual);

            expectEqual(expectedTag, actualTag);

            // we only reach this loop if the tags are equal
            inline for (std.meta.fields(@TypeOf(actual))) |fld| {
                if (std.mem.eql(u8, fld.name, @tagName(actualTag))) {
                    expectEqual(@field(expected, fld.name), @field(actual, fld.name));
                    return;
                }
            }

            // we iterate over *all* union fields
            // => we should never get here as the loop above is
            //    including all possible values.
            unreachable;
        },

        .Optional => {
            if (expected) |expected_payload| {
                if (actual) |actual_payload| {
                    expectEqual(expected_payload, actual_payload);
                } else {
                    std.debug.panic("expected {}, found null", .{expected_payload});
                }
            } else {
                if (actual) |actual_payload| {
                    std.debug.panic("expected null, found {}", .{actual_payload});
                }
            }
        },

        .ErrorUnion => {
            if (expected) |expected_payload| {
                if (actual) |actual_payload| {
                    expectEqual(expected_payload, actual_payload);
                } else |actual_err| {
                    std.debug.panic("expected {}, found {}", .{ expected_payload, actual_err });
                }
            } else |expected_err| {
                if (actual) |actual_payload| {
                    std.debug.panic("expected {}, found {}", .{ expected_err, actual_payload });
                } else |actual_err| {
                    expectEqual(expected_err, actual_err);
                }
            }
        },
    }
}

test "expectEqual.union(enum)" {
    const T = union(enum) {
        a: i32,
        b: f32,
    };

    const a10 = T{ .a = 10 };
    const a20 = T{ .a = 20 };

    expectEqual(a10, a10);
}

/// This function is intended to be used only in tests. When the formatted result of the template
/// and its arguments does not equal the expected text, it prints diagnostics to stderr to show how
/// they are not equal, then returns an error.
pub fn expectFmt(expected: []const u8, comptime template: []const u8, args: anytype) !void {
    const result = try std.fmt.allocPrint(allocator, template, args);
    defer allocator.free(result);
    if (std.mem.eql(u8, result, expected)) return;

    print("\n====== expected this output: =========\n", .{});
    print("{s}", .{expected});
    print("\n======== instead found this: =========\n", .{});
    print("{s}", .{result});
    print("\n======================================\n", .{});
    return error.TestFailed;
}

/// This function is intended to be used only in tests. When the actual value is not
/// within the margin of the expected value,
/// prints diagnostics to stderr to show exactly how they are not equal, then aborts.
/// The types must be floating point
pub fn expectWithinMargin(expected: anytype, actual: @TypeOf(expected), margin: @TypeOf(expected)) void {
    std.debug.assert(margin >= 0.0);

    switch (@typeInfo(@TypeOf(actual))) {
        .Float,
        .ComptimeFloat,
        => {
            if (@fabs(expected - actual) > margin) {
                std.debug.panic("actual {}, not within margin {} of expected {}", .{ actual, margin, expected });
            }
        },
        else => @compileError("Unable to compare non floating point values"),
    }
}

test "expectWithinMargin" {
    inline for ([_]type{ f16, f32, f64, f128 }) |T| {
        const pos_x: T = 12.0;
        const pos_y: T = 12.06;
        const neg_x: T = -12.0;
        const neg_y: T = -12.06;

        expectWithinMargin(pos_x, pos_y, 0.1);
        expectWithinMargin(neg_x, neg_y, 0.1);
    }
}

/// This function is intended to be used only in tests. When the actual value is not
/// within the epsilon of the expected value,
/// prints diagnostics to stderr to show exactly how they are not equal, then aborts.
/// The types must be floating point
pub fn expectWithinEpsilon(expected: anytype, actual: @TypeOf(expected), epsilon: @TypeOf(expected)) void {
    std.debug.assert(epsilon >= 0.0 and epsilon <= 1.0);

    // Relative epsilon test.
    const margin = math.max(math.fabs(expected), math.fabs(actual)) * epsilon;
    switch (@typeInfo(@TypeOf(actual))) {
        .Float,
        .ComptimeFloat,
        => {
            if (@fabs(expected - actual) > margin) {
                std.debug.panic("actual {}, not within epsilon {}, of expected {}", .{ actual, epsilon, expected });
            }
        },
        else => @compileError("Unable to compare non floating point values"),
    }
}

test "expectWithinEpsilon" {
    inline for ([_]type{ f16, f32, f64, f128 }) |T| {
        const pos_x: T = 12.0;
        const pos_y: T = 13.2;
        const neg_x: T = -12.0;
        const neg_y: T = -13.2;

        expectWithinEpsilon(pos_x, pos_y, 0.1);
        expectWithinEpsilon(neg_x, neg_y, 0.1);
    }
}

/// This function is intended to be used only in tests. When the two slices are not
/// equal, prints diagnostics to stderr to show exactly how they are not equal,
/// then aborts.
/// If your inputs are UTF-8 encoded strings, consider calling `expectEqualStrings` instead.
pub fn expectEqualSlices(comptime T: type, expected: []const T, actual: []const T) void {
    // TODO better printing of the difference
    // If the arrays are small enough we could print the whole thing
    // If the child type is u8 and no weird bytes, we could print it as strings
    // Even for the length difference, it would be useful to see the values of the slices probably.
    if (expected.len != actual.len) {
        std.debug.panic("slice lengths differ. expected {d}, found {d}", .{ expected.len, actual.len });
    }
    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        if (!std.meta.eql(expected[i], actual[i])) {
            std.debug.panic("index {} incorrect. expected {}, found {}", .{ i, expected[i], actual[i] });
        }
    }
}

/// This function is intended to be used only in tests. When `ok` is false, the test fails.
/// A message is printed to stderr and then abort is called.
pub fn expect(ok: bool) void {
    if (!ok) @panic("test failure");
}

pub const TmpDir = struct {
    dir: std.fs.Dir,
    parent_dir: std.fs.Dir,
    sub_path: [sub_path_len]u8,

    const random_bytes_count = 12;
    const sub_path_len = std.base64.Base64Encoder.calcSize(random_bytes_count);

    pub fn cleanup(self: *TmpDir) void {
        self.dir.close();
        self.parent_dir.deleteTree(&self.sub_path) catch {};
        self.parent_dir.close();
        self.* = undefined;
    }
};

fn getCwdOrWasiPreopen() std.fs.Dir {
    if (@import("builtin").os.tag == .wasi) {
        var preopens = std.fs.wasi.PreopenList.init(allocator);
        defer preopens.deinit();
        preopens.populate() catch
            @panic("unable to make tmp dir for testing: unable to populate preopens");
        const preopen = preopens.find(std.fs.wasi.PreopenType{ .Dir = "." }) orelse
            @panic("unable to make tmp dir for testing: didn't find '.' in the preopens");

        return std.fs.Dir{ .fd = preopen.fd };
    } else {
        return std.fs.cwd();
    }
}

pub fn tmpDir(opts: std.fs.Dir.OpenDirOptions) TmpDir {
    var random_bytes: [TmpDir.random_bytes_count]u8 = undefined;
    std.crypto.random.bytes(&random_bytes);
    var sub_path: [TmpDir.sub_path_len]u8 = undefined;
    _ = std.fs.base64_encoder.encode(&sub_path, &random_bytes);

    var cwd = getCwdOrWasiPreopen();
    var cache_dir = cwd.makeOpenPath("zig-cache", .{}) catch
        @panic("unable to make tmp dir for testing: unable to make and open zig-cache dir");
    defer cache_dir.close();
    var parent_dir = cache_dir.makeOpenPath("tmp", .{}) catch
        @panic("unable to make tmp dir for testing: unable to make and open zig-cache/tmp dir");
    var dir = parent_dir.makeOpenPath(&sub_path, opts) catch
        @panic("unable to make tmp dir for testing: unable to make and open the tmp dir");

    return .{
        .dir = dir,
        .parent_dir = parent_dir,
        .sub_path = sub_path,
    };
}

test "expectEqual nested array" {
    const a = [2][2]f32{
        [_]f32{ 1.0, 0.0 },
        [_]f32{ 0.0, 1.0 },
    };

    const b = [2][2]f32{
        [_]f32{ 1.0, 0.0 },
        [_]f32{ 0.0, 1.0 },
    };

    expectEqual(a, b);
}

test "expectEqual vector" {
    var a = @splat(4, @as(u32, 4));
    var b = @splat(4, @as(u32, 4));

    expectEqual(a, b);
}

pub fn expectEqualStrings(expected: []const u8, actual: []const u8) void {
    if (std.mem.indexOfDiff(u8, actual, expected)) |diff_index| {
        print("\n====== expected this output: =========\n", .{});
        printWithVisibleNewlines(expected);
        print("\n======== instead found this: =========\n", .{});
        printWithVisibleNewlines(actual);
        print("\n======================================\n", .{});

        var diff_line_number: usize = 1;
        for (expected[0..diff_index]) |value| {
            if (value == '\n') diff_line_number += 1;
        }
        print("First difference occurs on line {d}:\n", .{diff_line_number});

        print("expected:\n", .{});
        printIndicatorLine(expected, diff_index);

        print("found:\n", .{});
        printIndicatorLine(actual, diff_index);

        @panic("test failure");
    }
}

pub fn expectStringEndsWith(actual: []const u8, expected_ends_with: []const u8) void {
    if (std.mem.endsWith(u8, actual, expected_ends_with))
        return;

    const shortened_actual = if (actual.len >= expected_ends_with.len)
        actual[0..expected_ends_with.len]
    else
        actual;

    print("\n====== expected to end with: =========\n", .{});
    printWithVisibleNewlines(expected_ends_with);
    print("\n====== instead ended with: ===========\n", .{});
    printWithVisibleNewlines(shortened_actual);
    print("\n========= full output: ==============\n", .{});
    printWithVisibleNewlines(actual);
    print("\n======================================\n", .{});

    @panic("test failure");
}

fn printIndicatorLine(source: []const u8, indicator_index: usize) void {
    const line_begin_index = if (std.mem.lastIndexOfScalar(u8, source[0..indicator_index], '\n')) |line_begin|
        line_begin + 1
    else
        0;
    const line_end_index = if (std.mem.indexOfScalar(u8, source[indicator_index..], '\n')) |line_end|
        (indicator_index + line_end)
    else
        source.len;

    printLine(source[line_begin_index..line_end_index]);
    {
        var i: usize = line_begin_index;
        while (i < indicator_index) : (i += 1)
            print(" ", .{});
    }
    print("^\n", .{});
}

fn printWithVisibleNewlines(source: []const u8) void {
    var i: usize = 0;
    while (std.mem.indexOf(u8, source[i..], "\n")) |nl| : (i += nl + 1) {
        printLine(source[i .. i + nl]);
    }
    print("{s}␃\n", .{source[i..]}); // End of Text symbol (ETX)
}

fn printLine(line: []const u8) void {
    if (line.len != 0) switch (line[line.len - 1]) {
        ' ', '\t' => print("{s}⏎\n", .{line}), // Carriage return symbol,
        else => {},
    };
    print("{s}\n", .{line});
}

test "" {
    expectEqualStrings("foo", "foo");
}

/// Given a type, reference all the declarations inside, so that the semantic analyzer sees them.
pub fn refAllDecls(comptime T: type) void {
    if (!@import("builtin").is_test) return;
    inline for (std.meta.declarations(T)) |decl| {
        _ = decl;
    }
}
