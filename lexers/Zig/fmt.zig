// SPDX-License-Identifier: MIT
// Copyright (c) 2015-2021 Zig Contributors
// This file is part of [zig](https://ziglang.org/), which is MIT licensed.
// The MIT license requires this copyright notice to be included in all copies
// and substantial portions of the software.
const std = @import("std.zig");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const unicode = std.unicode;
const meta = std.meta;
const builtin = @import("builtin");
const errol = @import("fmt/errol.zig");
const lossyCast = std.math.lossyCast;
const expectFmt = std.testing.expectFmt;

pub const default_max_depth = 3;

pub const Alignment = enum {
    Left,
    Center,
    Right,
};

pub const FormatOptions = struct {
    precision: ?usize = null,
    width: ?usize = null,
    alignment: Alignment = .Right,
    fill: u8 = ' ',
};

/// Renders fmt string with args, calling output with slices of bytes.
/// If `output` returns an error, the error is returned from `format` and
/// `output` is not called again.
///
/// The format string must be comptime known and may contain placeholders following
/// this format:
/// `{[position][specifier]:[fill][alignment][width].[precision]}`
///
/// Each word between `[` and `]` is a parameter you have to replace with something:
///
/// - *position* is the index of the argument that should be inserted
/// - *specifier* is a type-dependent formatting option that determines how a type should formatted (see below)
/// - *fill* is a single character which is used to pad the formatted text
/// - *alignment* is one of the three characters `<`, `^` or `>`. they define if the text is *left*, *center*, or *right* aligned
/// - *width* is the total width of the field in characters
/// - *precision* specifies how many decimals a formatted number should have
///
/// Note that most of the parameters are optional and may be omitted. Also you can leave out separators like `:` and `.` when
/// all parameters after the separator are omitted.
/// Only exception is the *fill* parameter. If *fill* is required, one has to specify *alignment* as well, as otherwise
/// the digits after `:` is interpreted as *width*, not *fill*.
///
/// The *specifier* has several options for types:
/// - `x` and `X`:
///   - format the non-numeric value as a string of bytes in hexadecimal notation ("binary dump") in either lower case or upper case
///   - output numeric value in hexadecimal notation
/// - `s`:
///   - for pointer-to-many and C pointers of u8, print as a C-string using zero-termination
///   - for slices of u8, print the entire slice as a string without zero-termination
/// - `z`: escape the string with @"" syntax if it is not a valid Zig identifier.
/// - `Z`: print the string escaping non-printable characters using Zig escape sequences.
/// - `B` and `Bi`: output a memory size in either metric (1000) or power-of-two (1024) based notation. works for both float and integer values.
/// - `e` and `E`: if printing a string, escape non-printable characters
/// - `e`: output floating point value in scientific notation
/// - `d`: output numeric value in decimal notation
/// - `b`: output integer value in binary notation
/// - `o`: output integer value in octal notation
/// - `c`: output integer as an ASCII character. Integer type must have 8 bits at max.
/// - `u`: output integer as an UTF-8 sequence. Integer type must have 21 bits at max.
/// - `*`: output the address of the value instead of the value itself.
///
/// If a formatted user type contains a function of the type
/// ```
/// pub fn format(value: ?, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void
/// ```
/// with `?` being the type formatted, this function will be called instead of the default implementation.
/// This allows user types to be formatted in a logical manner instead of dumping all fields of the type.
///
/// A user type may be a `struct`, `vector`, `union` or `enum` type.
///
/// To print literal curly braces, escape them by writing them twice, e.g. `{{` or `}}`.
pub fn format(
    writer: anytype,
    comptime fmt: []const u8,
    args: anytype,
) !void {
    const ArgSetType = u32;

    const ArgsType = @TypeOf(args);
    // XXX: meta.trait.is(.Struct)(ArgsType) doesn't seem to work...
    if (@typeInfo(ArgsType) != .Struct) {
        @compileError("Expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields_info = meta.fields(ArgsType);
    if (fields_info.len > @typeInfo(ArgSetType).Int.bits) {
        @compileError("32 arguments max are supported per format call");
    }

    comptime var arg_state: struct {
        next_arg: usize = 0,
        used_args: usize = 0,
        args_len: usize = fields_info.len,

        fn hasUnusedArgs(comptime self: *@This()) bool {
            return @popCount(ArgSetType, self.used_args) != self.args_len;
        }

        fn nextArg(comptime self: *@This(), comptime arg_index: ?usize) comptime_int {
            const next_index = arg_index orelse init: {
                const arg = self.next_arg;
                self.next_arg += 1;
                break :init arg;
            };

            if (next_index >= self.args_len) {
                @compileError("Too few arguments");
            }

            // Mark this argument as used
            self.used_args |= 1 << next_index;

            return next_index;
        }
    } = .{};

    comptime var parser: struct {
        buf: []const u8 = undefined,
        pos: comptime_int = 0,

        // Returns a decimal number or null if the current character is not a
        // digit
        fn number(comptime self: *@This()) ?usize {
            var r: ?usize = null;

            while (self.pos < self.buf.len) : (self.pos += 1) {
                switch (self.buf[self.pos]) {
                    '0'...'9' => {
                        if (r == null) r = 0;
                        r.? *= 10;
                        r.? += self.buf[self.pos] - '0';
                    },
                    else => break,
                }
            }

            return r;
        }

        // Returns a substring of the input starting from the current position
        // and ending where `ch` is found or until the end if not found
        fn until(comptime self: *@This(), comptime ch: u8) []const u8 {
            const start = self.pos;

            if (start >= self.buf.len)
                return &[_]u8{};

            while (self.pos < self.buf.len) : (self.pos += 1) {
                if (self.buf[self.pos] == ch) break;
            }
            return self.buf[start..self.pos];
        }

        // Returns one character, if available
        fn char(comptime self: *@This()) ?u8 {
            if (self.pos < self.buf.len) {
                const ch = self.buf[self.pos];
                self.pos += 1;
                return ch;
            }
            return null;
        }

        fn maybe(comptime self: *@This(), comptime val: u8) bool {
            if (self.pos < self.buf.len and self.buf[self.pos] == val) {
                self.pos += 1;
                return true;
            }
            return false;
        }

        // Returns the n-th next character or null if that's past the end
        fn peek(comptime self: *@This(), comptime n: usize) ?u8 {
            return if (self.pos + n < self.buf.len) self.buf[self.pos + n] else null;
        }
    } = .{};

    var options: FormatOptions = .{};

    @setEvalBranchQuota(2000000);

    comptime var i = 0;
    inline while (i < fmt.len) {
        comptime const start_index = i;

        inline while (i < fmt.len) : (i += 1) {
            switch (fmt[i]) {
                '{', '}' => break,
                else => {},
            }
        }

        comptime var end_index = i;
        comptime var unescape_brace = false;

        // Handle {{ and }}, those are un-escaped as single braces
        if (i + 1 < fmt.len and fmt[i + 1] == fmt[i]) {
            unescape_brace = true;
            // Make the first brace part of the literal...
            end_index += 1;
            // ...and skip both
            i += 2;
        }

        // Write out the literal
        if (start_index != end_index) {
            try writer.writeAll(fmt[start_index..end_index]);
        }

        // We've already skipped the other brace, restart the loop
        if (unescape_brace) continue;

        if (i >= fmt.len) break;

        if (fmt[i] == '}') {
            @compileError("Missing opening {");
        }

        // Get past the {
        comptime assert(fmt[i] == '{');
        i += 1;

        comptime const fmt_begin = i;
        // Find the closing brace
        inline while (i < fmt.len and fmt[i] != '}') : (i += 1) {}
        comptime const fmt_end = i;

        if (i >= fmt.len) {
            @compileError("Missing closing }");
        }

        // Get past the }
        comptime assert(fmt[i] == '}');
        i += 1;

        options = .{};

        // Parse the format fragment between braces
        parser.buf = fmt[fmt_begin..fmt_end];
        parser.pos = 0;

        // Parse the positional argument number
        comptime const opt_pos_arg = init: {
            if (comptime parser.maybe('[')) {
                comptime const arg_name = parser.until(']');

                if (!comptime parser.maybe(']')) {
                    @compileError("Expected closing ]");
                }

                break :init comptime meta.fieldIndex(ArgsType, arg_name) orelse
                    @compileError("No argument with name '" ++ arg_name ++ "'");
            } else {
                break :init comptime parser.number();
            }
        };

        // Parse the format specifier
        comptime const specifier_arg = comptime parser.until(':');

        // Skip the colon, if present
        if (comptime parser.char()) |ch| {
            if (ch != ':') {
                @compileError("Expected : or }, found '" ++ [1]u8{ch} ++ "'");
            }
        }

        // Parse the fill character
        // The fill parameter requires the alignment parameter to be specified
        // too
        if (comptime parser.peek(1)) |ch| {
            if (comptime mem.indexOfScalar(u8, "<^>", ch) != null) {
                options.fill = comptime parser.char().?;
            }
        }

        // Parse the alignment parameter
        if (comptime parser.peek(0)) |ch| {
            switch (ch) {
                '<' => {
                    options.alignment = .Left;
                    _ = comptime parser.char();
                },
                '^' => {
                    options.alignment = .Center;
                    _ = comptime parser.char();
                },
                '>' => {
                    options.alignment = .Right;
                    _ = comptime parser.char();
                },
                else => {},
            }
        }

        // Parse the width parameter
        options.width = init: {
            if (comptime parser.maybe('[')) {
                comptime const arg_name = parser.until(']');

                if (!comptime parser.maybe(']')) {
                    @compileError("Expected closing ]");
                }

                comptime const index = meta.fieldIndex(ArgsType, arg_name) orelse
                    @compileError("No argument with name '" ++ arg_name ++ "'");
                const arg_index = comptime arg_state.nextArg(index);

                break :init @field(args, fields_info[arg_index].name);
            } else {
                break :init comptime parser.number();
            }
        };

        // Skip the dot, if present
        if (comptime parser.char()) |ch| {
            if (ch != '.') {
                @compileError("Expected . or }, found '" ++ [1]u8{ch} ++ "'");
            }
        }

        // Parse the precision parameter
        options.precision = init: {
            if (comptime parser.maybe('[')) {
                comptime const arg_name = parser.until(']');

                if (!comptime parser.maybe(']')) {
                    @compileError("Expected closing ]");
                }

                comptime const arg_i = meta.fieldIndex(ArgsType, arg_name) orelse
                    @compileError("No argument with name '" ++ arg_name ++ "'");
                const arg_to_use = comptime arg_state.nextArg(arg_i);

                break :init @field(args, fields_info[arg_to_use].name);
            } else {
                break :init comptime parser.number();
            }
        };

        if (comptime parser.char()) |ch| {
            @compileError("Extraneous trailing character '" ++ [1]u8{ch} ++ "'");
        }

        const arg_to_print = comptime arg_state.nextArg(opt_pos_arg);
        try formatType(
            @field(args, fields_info[arg_to_print].name),
            specifier_arg,
            options,
            writer,
            default_max_depth,
        );
    }

    if (comptime arg_state.hasUnusedArgs()) {
        @compileError("Unused arguments");
    }
}

pub fn formatAddress(value: anytype, options: FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
    const T = @TypeOf(value);

    switch (@typeInfo(T)) {
        .Pointer => |info| {
            try writer.writeAll(@typeName(info.child) ++ "@");
            if (info.size == .Slice)
                try formatInt(@ptrToInt(value.ptr), 16, false, FormatOptions{}, writer)
            else
                try formatInt(@ptrToInt(value), 16, false, FormatOptions{}, writer);
            return;
        },
        .Optional => |info| {
            if (@typeInfo(info.child) == .Pointer) {
                try writer.writeAll(@typeName(info.child) ++ "@");
                try formatInt(@ptrToInt(value), 16, false, FormatOptions{}, writer);
                return;
            }
        },
        .Array => |info| {
            try writer.writeAll(@typeName(info.child) ++ "@");
            try formatInt(@ptrToInt(value), 16, false, FormatOptions{}, writer);
            return;
        },
        else => {},
    }

    @compileError("Cannot format non-pointer type " ++ @typeName(T) ++ " with * specifier");
}

pub fn formatType(
    value: anytype,
    comptime fmt: []const u8,
    options: FormatOptions,
    writer: anytype,
    max_depth: usize,
) @TypeOf(writer).Error!void {
    if (comptime std.mem.eql(u8, fmt, "*")) {
        return formatAddress(value, options, writer);
    }

    const T = @TypeOf(value);
    if (comptime std.meta.trait.hasFn("format")(T)) {
        return try value.format(fmt, options, writer);
    }

    switch (@typeInfo(T)) {
        .ComptimeInt, .Int, .ComptimeFloat, .Float => {
            return formatValue(value, fmt, options, writer);
        },
        .Void => {
            return formatBuf("void", options, writer);
        },
        .Bool => {
            return formatBuf(if (value) "true" else "false", options, writer);
        },
        .Optional => {
            if (value) |payload| {
                return formatType(payload, fmt, options, writer, max_depth);
            } else {
                return formatBuf("null", options, writer);
            }
        },
        .ErrorUnion => {
            if (value) |payload| {
                return formatType(payload, fmt, options, writer, max_depth);
            } else |err| {
                return formatType(err, fmt, options, writer, max_depth);
            }
        },
        .ErrorSet => {
            try writer.writeAll("error.");
            return writer.writeAll(@errorName(value));
        },
        .Enum => |enumInfo| {
            try writer.writeAll(@typeName(T));
            if (enumInfo.is_exhaustive) {
                try writer.writeAll(".");
                try writer.writeAll(@tagName(value));
                return;
            }

            // Use @tagName only if value is one of known fields
            @setEvalBranchQuota(3 * enumInfo.fields.len);
            inline for (enumInfo.fields) |enumField| {
                if (@enumToInt(value) == enumField.value) {
                    try writer.writeAll(".");
                    try writer.writeAll(@tagName(value));
                    return;
                }
            }

            try writer.writeAll("(");
            try formatType(@enumToInt(value), fmt, options, writer, max_depth);
            try writer.writeAll(")");
        },
        .Union => |info| {
            try writer.writeAll(@typeName(T));
            if (max_depth == 0) {
                return writer.writeAll("{ ... }");
            }
            if (info.tag_type) |UnionTagType| {
                try writer.writeAll("{ .");
                try writer.writeAll(@tagName(@as(UnionTagType, value)));
                try writer.writeAll(" = ");
                inline for (info.fields) |u_field| {
                    if (value == @field(UnionTagType, u_field.name)) {
                        try formatType(@field(value, u_field.name), fmt, options, writer, max_depth - 1);
                    }
                }
                try writer.writeAll(" }");
            } else {
                try format(writer, "@{x}", .{@ptrToInt(&value)});
            }
        },
        .Struct => |info| {
            try writer.writeAll(@typeName(T));
            if (max_depth == 0) {
                return writer.writeAll("{ ... }");
            }
            try writer.writeAll("{");
            inline for (info.fields) |f, i| {
                if (i == 0) {
                    try writer.writeAll(" .");
                } else {
                    try writer.writeAll(", .");
                }
                try writer.writeAll(f.name);
                try writer.writeAll(" = ");
                try formatType(@field(value, f.name), fmt, options, writer, max_depth - 1);
            }
            try writer.writeAll(" }");
        },
        .Pointer => |ptr_info| switch (ptr_info.size) {
            .One => switch (@typeInfo(ptr_info.child)) {
                .Array => |info| {
                    if (info.child == u8) {
                        if (fmt.len > 0 and comptime mem.indexOfScalar(u8, "sxXeEzZ", fmt[0]) != null) {
                            return formatText(value, fmt, options, writer);
                        }
                    }
                    return format(writer, "{s}@{x}", .{ @typeName(ptr_info.child), @ptrToInt(value) });
                },
                .Enum, .Union, .Struct => {
                    return formatType(value.*, fmt, options, writer, max_depth);
                },
                else => return format(writer, "{s}@{x}", .{ @typeName(ptr_info.child), @ptrToInt(value) }),
            },
            .Many, .C => {
                if (ptr_info.sentinel) |sentinel| {
                    return formatType(mem.span(value), fmt, options, writer, max_depth);
                }
                if (ptr_info.child == u8) {
                    if (fmt.len > 0 and comptime mem.indexOfScalar(u8, "sxXeEzZ", fmt[0]) != null) {
                        return formatText(mem.span(value), fmt, options, writer);
                    }
                }
                return format(writer, "{s}@{x}", .{ @typeName(ptr_info.child), @ptrToInt(value) });
            },
            .Slice => {
                if (max_depth == 0) {
                    return writer.writeAll("{ ... }");
                }
                if (ptr_info.child == u8) {
                    if (fmt.len > 0 and comptime mem.indexOfScalar(u8, "sxXeEzZ", fmt[0]) != null) {
                        return formatText(value, fmt, options, writer);
                    }
                }
                try writer.writeAll("{ ");
                for (value) |elem, i| {
                    try formatType(elem, fmt, options, writer, max_depth - 1);
                    if (i != value.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(" }");
            },
        },
        .Array => |info| {
            if (max_depth == 0) {
                return writer.writeAll("{ ... }");
            }
            if (info.child == u8) {
                if (fmt.len > 0 and comptime mem.indexOfScalar(u8, "sxXeEzZ", fmt[0]) != null) {
                    return formatText(&value, fmt, options, writer);
                }
            }
            try writer.writeAll("{ ");
            for (value) |elem, i| {
                try formatType(elem, fmt, options, writer, max_depth - 1);
                if (i < value.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll(" }");
        },
        .Vector => |info| {
            try writer.writeAll("{ ");
            var i: usize = 0;
            while (i < info.len) : (i += 1) {
                try formatValue(value[i], fmt, options, writer);
                if (i < info.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll(" }");
        },
        .Fn => {
            return format(writer, "{s}@{x}", .{ @typeName(T), @ptrToInt(value) });
        },
        .Type => return formatBuf(@typeName(value), options, writer),
        .EnumLiteral => {
            const buffer = [_]u8{'.'} ++ @tagName(value);
            return formatBuf(buffer, options, writer);
        },
        .Null => return formatBuf("null", options, writer),
        else => @compileError("Unable to format type '" ++ @typeName(T) ++ "'"),
    }
}

fn formatValue(
    value: anytype,
    comptime fmt: []const u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    if (comptime std.mem.eql(u8, fmt, "B")) {
        return formatBytes(value, options, 1000, writer);
    } else if (comptime std.mem.eql(u8, fmt, "Bi")) {
        return formatBytes(value, options, 1024, writer);
    }

    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .Float, .ComptimeFloat => return formatFloatValue(value, fmt, options, writer),
        .Int, .ComptimeInt => return formatIntValue(value, fmt, options, writer),
        .Bool => return formatBuf(if (value) "true" else "false", options, writer),
        else => comptime unreachable,
    }
}

pub fn formatIntValue(
    value: anytype,
    comptime fmt: []const u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    comptime var radix = 10;
    comptime var uppercase = false;

    const int_value = if (@TypeOf(value) == comptime_int) blk: {
        const Int = math.IntFittingRange(value, value);
        break :blk @as(Int, value);
    } else
        value;

    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "d")) {
        radix = 10;
        uppercase = false;
    } else if (comptime std.mem.eql(u8, fmt, "c")) {
        if (@typeInfo(@TypeOf(int_value)).Int.bits <= 8) {
            return formatAsciiChar(@as(u8, int_value), options, writer);
        } else {
            @compileError("Cannot print integer that is larger than 8 bits as a ascii");
        }
    } else if (comptime std.mem.eql(u8, fmt, "Z")) {
        if (@typeInfo(@TypeOf(int_value)).Int.bits <= 8) {
            const c: u8 = int_value;
            return formatZigEscapes(@as(*const [1]u8, &c), options, writer);
        } else {
            @compileError("Cannot escape character with more than 8 bits");
        }
    } else if (comptime std.mem.eql(u8, fmt, "u")) {
        if (@typeInfo(@TypeOf(int_value)).Int.bits <= 21) {
            return formatUnicodeCodepoint(@as(u21, int_value), options, writer);
        } else {
            @compileError("Cannot print integer that is larger than 21 bits as an UTF-8 sequence");
        }
    } else if (comptime std.mem.eql(u8, fmt, "b")) {
        radix = 2;
        uppercase = false;
    } else if (comptime std.mem.eql(u8, fmt, "x")) {
        radix = 16;
        uppercase = false;
    } else if (comptime std.mem.eql(u8, fmt, "X")) {
        radix = 16;
        uppercase = true;
    } else if (comptime std.mem.eql(u8, fmt, "o")) {
        radix = 8;
        uppercase = false;
    } else {
        @compileError("Unknown format string: '" ++ fmt ++ "'");
    }

    return formatInt(int_value, radix, uppercase, options, writer);
}

fn formatFloatValue(
    value: anytype,
    comptime fmt: []const u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    // this buffer should be enough to display all decimal places of a decimal f64 number.
    var buf: [512]u8 = undefined;
    var buf_stream = std.io.fixedBufferStream(&buf);

    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "e")) {
        formatFloatScientific(value, options, buf_stream.writer()) catch |err| switch (err) {
            error.NoSpaceLeft => unreachable,
            else => |e| return e,
        };
    } else if (comptime std.mem.eql(u8, fmt, "d")) {
        formatFloatDecimal(value, options, buf_stream.writer()) catch |err| switch (err) {
            error.NoSpaceLeft => unreachable,
            else => |e| return e,
        };
    } else {
        @compileError("Unknown format string: '" ++ fmt ++ "'");
    }

    return formatBuf(buf_stream.getWritten(), options, writer);
}

pub fn formatText(
    bytes: []const u8,
    comptime fmt: []const u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    if (comptime std.mem.eql(u8, fmt, "s")) {
        return formatBuf(bytes, options, writer);
    } else if (comptime (std.mem.eql(u8, fmt, "x") or std.mem.eql(u8, fmt, "X"))) {
        for (bytes) |c| {
            try formatInt(c, 16, fmt[0] == 'X', FormatOptions{ .width = 2, .fill = '0' }, writer);
        }
        return;
    } else if (comptime (std.mem.eql(u8, fmt, "e") or std.mem.eql(u8, fmt, "E"))) {
        for (bytes) |c| {
            if (std.ascii.isPrint(c)) {
                try writer.writeByte(c);
            } else {
                try writer.writeAll("\\x");
                try formatInt(c, 16, fmt[0] == 'E', FormatOptions{ .width = 2, .fill = '0' }, writer);
            }
        }
        return;
    } else if (comptime std.mem.eql(u8, fmt, "z")) {
        @compileError("specifier 'z' has been deprecated, wrap your argument in std.zig.fmtId instead");
    } else if (comptime std.mem.eql(u8, fmt, "Z")) {
        @compileError("specifier 'Z' has been deprecated, wrap your argument in std.zig.fmtEscapes instead");
    } else {
        @compileError("Unknown format string: '" ++ fmt ++ "'");
    }
}

pub fn formatAsciiChar(
    c: u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    return writer.writeAll(@as(*const [1]u8, &c));
}

pub fn formatUnicodeCodepoint(
    c: u21,
    options: FormatOptions,
    writer: anytype,
) !void {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(c, &buf) catch |err| switch (err) {
        error.Utf8CannotEncodeSurrogateHalf, error.CodepointTooLarge => {
            // In case of error output the replacement char U+FFFD
            return formatBuf(&[_]u8{ 0xef, 0xbf, 0xbd }, options, writer);
        },
    };
    return formatBuf(buf[0..len], options, writer);
}

pub fn formatBuf(
    buf: []const u8,
    options: FormatOptions,
    writer: anytype,
) !void {
    if (options.width) |min_width| {
        // In case of error assume the buffer content is ASCII-encoded
        const width = unicode.utf8CountCodepoints(buf) catch |_| buf.len;
        const padding = if (width < min_width) min_width - width else 0;

        if (padding == 0)
            return writer.writeAll(buf);

        switch (options.alignment) {
            .Left => {
                try writer.writeAll(buf);
                try writer.writeByteNTimes(options.fill, padding);
            },
            .Center => {
                const left_padding = padding / 2;
                const right_padding = (padding + 1) / 2;
                try writer.writeByteNTimes(options.fill, left_padding);
                try writer.writeAll(buf);
                try writer.writeByteNTimes(options.fill, right_padding);
            },
            .Right => {
                try writer.writeByteNTimes(options.fill, padding);
                try writer.writeAll(buf);
            },
        }
    } else {
        // Fast path, avoid counting the number of codepoints
        try writer.writeAll(buf);
    }
}

/// Print a float in scientific notation to the specified precision. Null uses full precision.
/// It should be the case that every full precision, printed value can be re-parsed back to the
/// same type unambiguously.
pub fn formatFloatScientific(
    value: anytype,
    options: FormatOptions,
    writer: anytype,
) !void {
    var x = @floatCast(f64, value);

    // Errol doesn't handle these special cases.
    if (math.signbit(x)) {
        try writer.writeAll("-");
        x = -x;
    }

    if (math.isNan(x)) {
        return writer.writeAll("nan");
    }
    if (math.isPositiveInf(x)) {
        return writer.writeAll("inf");
    }
    if (x == 0.0) {
        try writer.writeAll("0");

        if (options.precision) |precision| {
            if (precision != 0) {
                try writer.writeAll(".");
                var i: usize = 0;
                while (i < precision) : (i += 1) {
                    try writer.writeAll("0");
                }
            }
        } else {
            try writer.writeAll(".0");
        }

        try writer.writeAll("e+00");
        return;
    }

    var buffer: [32]u8 = undefined;
    var float_decimal = errol.errol3(x, buffer[0..]);

    if (options.precision) |precision| {
        errol.roundToPrecision(&float_decimal, precision, errol.RoundMode.Scientific);

        try writer.writeAll(float_decimal.digits[0..1]);

        // {e0} case prints no `.`
        if (precision != 0) {
            try writer.writeAll(".");

            var printed: usize = 0;
            if (float_decimal.digits.len > 1) {
                const num_digits = math.min(float_decimal.digits.len, precision + 1);
                try writer.writeAll(float_decimal.digits[1..num_digits]);
                printed += num_digits - 1;
            }

            while (printed < precision) : (printed += 1) {
                try writer.writeAll("0");
            }
        }
    } else {
        try writer.writeAll(float_decimal.digits[0..1]);
        try writer.writeAll(".");
        if (float_decimal.digits.len > 1) {
            const num_digits = if (@TypeOf(value) == f32) math.min(@as(usize, 9), float_decimal.digits.len) else float_decimal.digits.len;

            try writer.writeAll(float_decimal.digits[1..num_digits]);
        } else {
            try writer.writeAll("0");
        }
    }

    try writer.writeAll("e");
    const exp = float_decimal.exp - 1;

    if (exp >= 0) {
        try writer.writeAll("+");
        if (exp > -10 and exp < 10) {
            try writer.writeAll("0");
        }
        try formatInt(exp, 10, false, FormatOptions{ .width = 0 }, writer);
    } else {
        try writer.writeAll("-");
        if (exp > -10 and exp < 10) {
            try writer.writeAll("0");
        }
        try formatInt(-exp, 10, false, FormatOptions{ .width = 0 }, writer);
    }
}

/// Print a float of the format x.yyyyy where the number of y is specified by the precision argument.
/// By default floats are printed at full precision (no rounding).
pub fn formatFloatDecimal(
    value: anytype,
    options: FormatOptions,
    writer: anytype,
) !void {
    var x = @as(f64, value);

    // Errol doesn't handle these special cases.
    if (math.signbit(x)) {
        try writer.writeAll("-");
        x = -x;
    }

    if (math.isNan(x)) {
        return writer.writeAll("nan");
    }
    if (math.isPositiveInf(x)) {
        return writer.writeAll("inf");
    }
    if (x == 0.0) {
        try writer.writeAll("0");

        if (options.precision) |precision| {
            if (precision != 0) {
                try writer.writeAll(".");
                var i: usize = 0;
                while (i < precision) : (i += 1) {
                    try writer.writeAll("0");
                }
            } else {
                try writer.writeAll(".0");
            }
        }

        return;
    }

    // non-special case, use errol3
    var buffer: [32]u8 = undefined;
    var float_decimal = errol.errol3(x, buffer[0..]);

    if (options.precision) |precision| {
        errol.roundToPrecision(&float_decimal, precision, errol.RoundMode.Decimal);

        // exp < 0 means the leading is always 0 as errol result is normalized.
        var num_digits_whole = if (float_decimal.exp > 0) @intCast(usize, float_decimal.exp) else 0;

        // the actual slice into the buffer, we may need to zero-pad between num_digits_whole and this.
        var num_digits_whole_no_pad = math.min(num_digits_whole, float_decimal.digits.len);

        if (num_digits_whole > 0) {
            // We may have to zero pad, for instance 1e4 requires zero padding.
            try writer.writeAll(float_decimal.digits[0..num_digits_whole_no_pad]);

            var i = num_digits_whole_no_pad;
            while (i < num_digits_whole) : (i += 1) {
                try writer.writeAll("0");
            }
        } else {
            try writer.writeAll("0");
        }

        // {.0} special case doesn't want a trailing '.'
        if (precision == 0) {
            return;
        }

        try writer.writeAll(".");

        // Keep track of fractional count printed for case where we pre-pad then post-pad with 0's.
        var printed: usize = 0;

        // Zero-fill until we reach significant digits or run out of precision.
        if (float_decimal.exp <= 0) {
            const zero_digit_count = @intCast(usize, -float_decimal.exp);
            const zeros_to_print = math.min(zero_digit_count, precision);

            var i: usize = 0;
            while (i < zeros_to_print) : (i += 1) {
                try writer.writeAll("0");
                printed += 1;
            }

            if (printed >= precision) {
                return;
            }
        }

        // Remaining fractional portion, zero-padding if insufficient.
        assert(precision >= printed);
        if (num_digits_whole_no_pad + precision - printed < float_decimal.digits.len) {
            try writer.writeAll(float_decimal.digits[num_digits_whole_no_pad .. num_digits_whole_no_pad + precision - printed]);
            return;
        } else {
            try writer.writeAll(float_decimal.digits[num_digits_whole_no_pad..]);
            printed += float_decimal.digits.len - num_digits_whole_no_pad;

            while (printed < precision) : (printed += 1) {
                try writer.writeAll("0");
            }
        }
    } else {
        // exp < 0 means the leading is always 0 as errol result is normalized.
        var num_digits_whole = if (float_decimal.exp > 0) @intCast(usize, float_decimal.exp) else 0;

        // the actual slice into the buffer, we may need to zero-pad between num_digits_whole and this.
        var num_digits_whole_no_pad = math.min(num_digits_whole, float_decimal.digits.len);

        if (num_digits_whole > 0) {
            // We may have to zero pad, for instance 1e4 requires zero padding.
            try writer.writeAll(float_decimal.digits[0..num_digits_whole_no_pad]);

            var i = num_digits_whole_no_pad;
            while (i < num_digits_whole) : (i += 1) {
                try writer.writeAll("0");
            }
        } else {
            try writer.writeAll("0");
        }

        // Omit `.` if no fractional portion
        if (float_decimal.exp >= 0 and num_digits_whole_no_pad == float_decimal.digits.len) {
            return;
        }

        try writer.writeAll(".");

        // Zero-fill until we reach significant digits or run out of precision.
        if (float_decimal.exp < 0) {
            const zero_digit_count = @intCast(usize, -float_decimal.exp);

            var i: usize = 0;
            while (i < zero_digit_count) : (i += 1) {
                try writer.writeAll("0");
            }
        }

        try writer.writeAll(float_decimal.digits[num_digits_whole_no_pad..]);
    }
}

pub fn formatBytes(
    value: anytype,
    options: FormatOptions,
    comptime radix: usize,
    writer: anytype,
) !void {
    if (value == 0) {
        return writer.writeAll("0B");
    }

    const is_float = comptime std.meta.trait.is(.Float)(@TypeOf(value));
    const mags_si = " kMGTPEZY";
    const mags_iec = " KMGTPEZY";

    const log2 = if (is_float) @floatToInt(usize, math.log2(value)) else math.log2(value);
    const magnitude = switch (radix) {
        1000 => math.min(log2 / comptime math.log2(1000), mags_si.len - 1),
        1024 => math.min(log2 / 10, mags_iec.len - 1),
        else => unreachable,
    };
    const new_value = lossyCast(f64, value) / math.pow(f64, lossyCast(f64, radix), lossyCast(f64, magnitude));
    const suffix = switch (radix) {
        1000 => mags_si[magnitude],
        1024 => mags_iec[magnitude],
        else => unreachable,
    };

    try formatFloatDecimal(new_value, options, writer);

    if (suffix == ' ') {
        return writer.writeAll("B");
    }

    const buf = switch (radix) {
        1000 => &[_]u8{ suffix, 'B' },
        1024 => &[_]u8{ suffix, 'i', 'B' },
        else => unreachable,
    };
    return writer.writeAll(buf);
}

pub fn formatInt(
    value: anytype,
    base: u8,
    uppercase: bool,
    options: FormatOptions,
    writer: anytype,
) !void {
    assert(base >= 2);

    const int_value = if (@TypeOf(value) == comptime_int) blk: {
        const Int = math.IntFittingRange(value, value);
        break :blk @as(Int, value);
    } else
        value;

    const value_info = @typeInfo(@TypeOf(int_value)).Int;

    // The type must have the same size as `base` or be wider in order for the
    // division to work
    const min_int_bits = comptime math.max(value_info.bits, 8);
    const MinInt = std.meta.Int(.unsigned, min_int_bits);

    const abs_value = math.absCast(int_value);
    // The worst case in terms of space needed is base 2, plus 1 for the sign
    var buf: [1 + math.max(value_info.bits, 1)]u8 = undefined;

    var a: MinInt = abs_value;
    var index: usize = buf.len;
    while (true) {
        const digit = a % base;
        index -= 1;
        buf[index] = digitToChar(@intCast(u8, digit), uppercase);
        a /= base;
        if (a == 0) break;
    }

    if (value_info.signedness == .signed) {
        if (value < 0) {
            // Negative integer
            index -= 1;
            buf[index] = '-';
        } else if (options.width == null or options.width.? == 0) {
            // Positive integer, omit the plus sign
        } else {
            // Positive integer
            index -= 1;
            buf[index] = '+';
        }
    }

    return formatBuf(buf[index..], options, writer);
}

pub fn formatIntBuf(out_buf: []u8, value: anytype, base: u8, uppercase: bool, options: FormatOptions) usize {
    var fbs = std.io.fixedBufferStream(out_buf);
    formatInt(value, base, uppercase, options, fbs.writer()) catch unreachable;
    return fbs.pos;
}

/// Formats a number of nanoseconds according to its magnitude:
///
/// - #ns
/// - [#y][#w][#d][#h][#m]#[.###][u|m]s
pub fn formatDuration(ns: u64, writer: anytype) !void {
    var ns_remaining = ns;
    inline for (.{
        .{ .ns = 365 * std.time.ns_per_day, .sep = 'y' },
        .{ .ns = std.time.ns_per_week, .sep = 'w' },
        .{ .ns = std.time.ns_per_day, .sep = 'd' },
        .{ .ns = std.time.ns_per_hour, .sep = 'h' },
        .{ .ns = std.time.ns_per_min, .sep = 'm' },
    }) |unit| {
        if (ns_remaining >= unit.ns) {
            const units = ns_remaining / unit.ns;
            try formatInt(units, 10, false, .{}, writer);
            try writer.writeByte(unit.sep);
            ns_remaining -= units * unit.ns;
            if (ns_remaining == 0) return;
        }
    }

    inline for (.{
        .{ .ns = std.time.ns_per_s, .sep = "s" },
        .{ .ns = std.time.ns_per_ms, .sep = "ms" },
        .{ .ns = std.time.ns_per_us, .sep = "us" },
    }) |unit| {
        const kunits = ns_remaining * 1000 / unit.ns;
        if (kunits >= 1000) {
            try formatInt(kunits / 1000, 10, false, .{}, writer);
            if (kunits > 1000) {
                // Write up to 3 decimal places
                const frac = kunits % 1000;
                var buf = [_]u8{ '.', 0, 0, 0 };
                _ = formatIntBuf(buf[1..], frac, 10, false, .{ .fill = '0', .width = 3 });
                var end: usize = 4;
                while (end > 1) : (end -= 1) {
                    if (buf[end - 1] != '0') break;
                }
                try writer.writeAll(buf[0..end]);
            }
            try writer.writeAll(unit.sep);
            return;
        }
    }

    try formatInt(ns, 10, false, .{}, writer);
    try writer.writeAll("ns");
    return;
}

test "formatDuration" {
    var buf: [24]u8 = undefined;
    inline for (.{
        .{ .s = "0ns", .d = 0 },
        .{ .s = "1ns", .d = 1 },
        .{ .s = "999ns", .d = std.time.ns_per_us - 1 },
        .{ .s = "1us", .d = std.time.ns_per_us },
        .{ .s = "1.45us", .d = 1450 },
        .{ .s = "1.5us", .d = 3 * std.time.ns_per_us / 2 },
        .{ .s = "999.999us", .d = std.time.ns_per_ms - 1 },
        .{ .s = "1ms", .d = std.time.ns_per_ms + 1 },
        .{ .s = "1.5ms", .d = 3 * std.time.ns_per_ms / 2 },
        .{ .s = "999.999ms", .d = std.time.ns_per_s - 1 },
        .{ .s = "1s", .d = std.time.ns_per_s },
        .{ .s = "59.999s", .d = std.time.ns_per_min - 1 },
        .{ .s = "1m", .d = std.time.ns_per_min },
        .{ .s = "1h", .d = std.time.ns_per_hour },
        .{ .s = "1d", .d = std.time.ns_per_day },
        .{ .s = "1w", .d = std.time.ns_per_week },
        .{ .s = "1y", .d = 365 * std.time.ns_per_day },
        .{ .s = "1y52w23h59m59.999s", .d = 730 * std.time.ns_per_day - 1 }, // 365d = 52w1d
        .{ .s = "1y1h1.001s", .d = 365 * std.time.ns_per_day + std.time.ns_per_hour + std.time.ns_per_s + std.time.ns_per_ms },
        .{ .s = "1y1h1s", .d = 365 * std.time.ns_per_day + std.time.ns_per_hour + std.time.ns_per_s + 999 * std.time.ns_per_us },
        .{ .s = "1y1h999.999us", .d = 365 * std.time.ns_per_day + std.time.ns_per_hour + std.time.ns_per_ms - 1 },
        .{ .s = "1y1h1ms", .d = 365 * std.time.ns_per_day + std.time.ns_per_hour + std.time.ns_per_ms },
        .{ .s = "1y1h1ms", .d = 365 * std.time.ns_per_day + std.time.ns_per_hour + std.time.ns_per_ms + 1 },
    }) |tc| {
        const slice = try bufPrint(&buf, "{}", .{duration(tc.d)});
        std.testing.expectEqualStrings(tc.s, slice);
    }
}

/// Wraps a `u64` to format with `formatDuration`.
const Duration = struct {
    ns: u64,

    pub fn format(self: Duration, comptime fmt: []const u8, options: FormatOptions, writer: anytype) !void {
        return formatDuration(self.ns, writer);
    }
};

/// Formats a number of nanoseconds according to its magnitude. See `formatDuration`.
pub fn duration(ns: u64) Duration {
    return Duration{ .ns = ns };
}

pub const ParseIntError = error{
    /// The result cannot fit in the type specified
    Overflow,

    /// The input was empty or had a byte that was not a digit
    InvalidCharacter,
};

/// Creates a Formatter type from a format function. Wrapping data in Formatter(func) causes
/// the data to be formatted using the given function `func`.  `func` must be of the following
/// form:
///
///     fn formatExample(
///         data: T,
///         comptime fmt: []const u8,
///         options: std.fmt.FormatOptions,
///         writer: anytype,
///     ) !void;
///
pub fn Formatter(comptime format_fn: anytype) type {
    const Data = @typeInfo(@TypeOf(format_fn)).Fn.args[0].arg_type.?;
    return struct {
        data: Data,
        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try format_fn(self.data, fmt, options, writer);
        }
    };
}

/// Parses the string `buf` as signed or unsigned representation in the
/// specified radix of an integral value of type `T`.
///
/// When `radix` is zero the string prefix is examined to detect the true radix:
///  * A prefix of "0b" implies radix=2,
///  * A prefix of "0o" implies radix=8,
///  * A prefix of "0x" implies radix=16,
///  * Otherwise radix=10 is assumed.
///
/// See also `parseUnsigned`.
pub fn parseInt(comptime T: type, buf: []const u8, radix: u8) ParseIntError!T {
    if (buf.len == 0) return error.InvalidCharacter;
    if (buf[0] == '+') return parseWithSign(T, buf[1..], radix, .Pos);
    if (buf[0] == '-') return parseWithSign(T, buf[1..], radix, .Neg);
    return parseWithSign(T, buf, radix, .Pos);
}

test "parseInt" {
    std.testing.expect((try parseInt(i32, "-10", 10)) == -10);
    std.testing.expect((try parseInt(i32, "+10", 10)) == 10);
    std.testing.expect((try parseInt(u32, "+10", 10)) == 10);
    std.testing.expectError(error.Overflow, parseInt(u32, "-10", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, " 10", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "10 ", 10));
    std.testing.expect((try parseInt(u8, "255", 10)) == 255);
    std.testing.expectError(error.Overflow, parseInt(u8, "256", 10));

    // +0 and -0 should work for unsigned
    std.testing.expect((try parseInt(u8, "-0", 10)) == 0);
    std.testing.expect((try parseInt(u8, "+0", 10)) == 0);

    // ensure minInt is parsed correctly
    std.testing.expect((try parseInt(i8, "-128", 10)) == math.minInt(i8));
    std.testing.expect((try parseInt(i43, "-4398046511104", 10)) == math.minInt(i43));

    // empty string or bare +- is invalid
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(i32, "", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "+", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(i32, "+", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "-", 10));
    std.testing.expectError(error.InvalidCharacter, parseInt(i32, "-", 10));

    // autodectect the radix
    std.testing.expect((try parseInt(i32, "111", 0)) == 111);
    std.testing.expect((try parseInt(i32, "+0b111", 0)) == 7);
    std.testing.expect((try parseInt(i32, "+0o111", 0)) == 73);
    std.testing.expect((try parseInt(i32, "+0x111", 0)) == 273);
    std.testing.expect((try parseInt(i32, "-0b111", 0)) == -7);
    std.testing.expect((try parseInt(i32, "-0o111", 0)) == -73);
    std.testing.expect((try parseInt(i32, "-0x111", 0)) == -273);

    // bare binary/octal/decimal prefix is invalid
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "0b", 0));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "0o", 0));
    std.testing.expectError(error.InvalidCharacter, parseInt(u32, "0x", 0));
}

fn parseWithSign(
    comptime T: type,
    buf: []const u8,
    radix: u8,
    comptime sign: enum { Pos, Neg },
) ParseIntError!T {
    if (buf.len == 0) return error.InvalidCharacter;

    var buf_radix = radix;
    var buf_start = buf;
    if (radix == 0) {
        // Treat is as a decimal number by default.
        buf_radix = 10;
        // Detect the radix by looking at buf prefix.
        if (buf.len > 2 and buf[0] == '0') {
            switch (buf[1]) {
                'b' => {
                    buf_radix = 2;
                    buf_start = buf[2..];
                },
                'o' => {
                    buf_radix = 8;
                    buf_start = buf[2..];
                },
                'x' => {
                    buf_radix = 16;
                    buf_start = buf[2..];
                },
                else => {},
            }
        }
    }

    const add = switch (sign) {
        .Pos => math.add,
        .Neg => math.sub,
    };

    var x: T = 0;

    for (buf_start) |c| {
        const digit = try charToDigit(c, buf_radix);

        if (x != 0) x = try math.mul(T, x, try math.cast(T, buf_radix));
        x = try add(T, x, try math.cast(T, digit));
    }

    return x;
}

/// Parses the string `buf` as  unsigned representation in the specified radix
/// of an integral value of type `T`.
///
/// When `radix` is zero the string prefix is examined to detect the true radix:
///  * A prefix of "0b" implies radix=2,
///  * A prefix of "0o" implies radix=8,
///  * A prefix of "0x" implies radix=16,
///  * Otherwise radix=10 is assumed.
///
/// See also `parseInt`.
pub fn parseUnsigned(comptime T: type, buf: []const u8, radix: u8) ParseIntError!T {
    return parseWithSign(T, buf, radix, .Pos);
}

test "parseUnsigned" {
    std.testing.expect((try parseUnsigned(u16, "050124", 10)) == 50124);
    std.testing.expect((try parseUnsigned(u16, "65535", 10)) == 65535);
    std.testing.expectError(error.Overflow, parseUnsigned(u16, "65536", 10));

    std.testing.expect((try parseUnsigned(u64, "0ffffffffffffffff", 16)) == 0xffffffffffffffff);
    std.testing.expectError(error.Overflow, parseUnsigned(u64, "10000000000000000", 16));

    std.testing.expect((try parseUnsigned(u32, "DeadBeef", 16)) == 0xDEADBEEF);

    std.testing.expect((try parseUnsigned(u7, "1", 10)) == 1);
    std.testing.expect((try parseUnsigned(u7, "1000", 2)) == 8);

    std.testing.expectError(error.InvalidCharacter, parseUnsigned(u32, "f", 10));
    std.testing.expectError(error.InvalidCharacter, parseUnsigned(u8, "109", 8));

    std.testing.expect((try parseUnsigned(u32, "NUMBER", 36)) == 1442151747);

    // these numbers should fit even though the radix itself doesn't fit in the destination type
    std.testing.expect((try parseUnsigned(u1, "0", 10)) == 0);
    std.testing.expect((try parseUnsigned(u1, "1", 10)) == 1);
    std.testing.expectError(error.Overflow, parseUnsigned(u1, "2", 10));
    std.testing.expect((try parseUnsigned(u1, "001", 16)) == 1);
    std.testing.expect((try parseUnsigned(u2, "3", 16)) == 3);
    std.testing.expectError(error.Overflow, parseUnsigned(u2, "4", 16));

    // parseUnsigned does not expect a sign
    std.testing.expectError(error.InvalidCharacter, parseUnsigned(u8, "+0", 10));
    std.testing.expectError(error.InvalidCharacter, parseUnsigned(u8, "-0", 10));

    // test empty string error
    std.testing.expectError(error.InvalidCharacter, parseUnsigned(u8, "", 10));
}

pub const parseFloat = @import("fmt/parse_float.zig").parseFloat;

test "parseFloat" {
    _ = @import("fmt/parse_float.zig");
}

pub fn charToDigit(c: u8, radix: u8) (error{InvalidCharacter}!u8) {
    const value = switch (c) {
        '0'...'9' => c - '0',
        'A'...'Z' => c - 'A' + 10,
        'a'...'z' => c - 'a' + 10,
        else => return error.InvalidCharacter,
    };

    if (value >= radix) return error.InvalidCharacter;

    return value;
}

pub fn digitToChar(digit: u8, uppercase: bool) u8 {
    return switch (digit) {
        0...9 => digit + '0',
        10...35 => digit + ((if (uppercase) @as(u8, 'A') else @as(u8, 'a')) - 10),
        else => unreachable,
    };
}

pub const BufPrintError = error{
    /// As much as possible was written to the buffer, but it was too small to fit all the printed bytes.
    NoSpaceLeft,
};
pub fn bufPrint(buf: []u8, comptime fmt: []const u8, args: anytype) BufPrintError![]u8 {
    var fbs = std.io.fixedBufferStream(buf);
    try format(fbs.writer(), fmt, args);
    return fbs.getWritten();
}

pub fn bufPrintZ(buf: []u8, comptime fmt: []const u8, args: anytype) BufPrintError![:0]u8 {
    const result = try bufPrint(buf, fmt ++ "\x00", args);
    return result[0 .. result.len - 1 :0];
}

/// Count the characters needed for format. Useful for preallocating memory
pub fn count(comptime fmt: []const u8, args: anytype) u64 {
    var counting_writer = std.io.countingWriter(std.io.null_writer);
    format(counting_writer.writer(), fmt, args) catch |err| switch (err) {};
    return counting_writer.bytes_written;
}

pub const AllocPrintError = error{OutOfMemory};

pub fn allocPrint(allocator: *mem.Allocator, comptime fmt: []const u8, args: anytype) AllocPrintError![]u8 {
    const size = math.cast(usize, count(fmt, args)) catch |err| switch (err) {
        // Output too long. Can't possibly allocate enough memory to display it.
        error.Overflow => return error.OutOfMemory,
    };
    const buf = try allocator.alloc(u8, size);
    return bufPrint(buf, fmt, args) catch |err| switch (err) {
        error.NoSpaceLeft => unreachable, // we just counted the size above
    };
}

/// Deprecated, use allocPrintZ
pub const allocPrint0 = allocPrintZ;

pub fn allocPrintZ(allocator: *mem.Allocator, comptime fmt: []const u8, args: anytype) AllocPrintError![:0]u8 {
    const result = try allocPrint(allocator, fmt ++ "\x00", args);
    return result[0 .. result.len - 1 :0];
}

test "bufPrintInt" {
    var buffer: [100]u8 = undefined;
    const buf = buffer[0..];

    std.testing.expectEqualSlices(u8, "-1", bufPrintIntToSlice(buf, @as(i1, -1), 10, false, FormatOptions{}));

    std.testing.expectEqualSlices(u8, "-101111000110000101001110", bufPrintIntToSlice(buf, @as(i32, -12345678), 2, false, FormatOptions{}));
    std.testing.expectEqualSlices(u8, "-12345678", bufPrintIntToSlice(buf, @as(i32, -12345678), 10, false, FormatOptions{}));
    std.testing.expectEqualSlices(u8, "-bc614e", bufPrintIntToSlice(buf, @as(i32, -12345678), 16, false, FormatOptions{}));
    std.testing.expectEqualSlices(u8, "-BC614E", bufPrintIntToSlice(buf, @as(i32, -12345678), 16, true, FormatOptions{}));

    std.testing.expectEqualSlices(u8, "12345678", bufPrintIntToSlice(buf, @as(u32, 12345678), 10, true, FormatOptions{}));

    std.testing.expectEqualSlices(u8, "   666", bufPrintIntToSlice(buf, @as(u32, 666), 10, false, FormatOptions{ .width = 6 }));
    std.testing.expectEqualSlices(u8, "  1234", bufPrintIntToSlice(buf, @as(u32, 0x1234), 16, false, FormatOptions{ .width = 6 }));
    std.testing.expectEqualSlices(u8, "1234", bufPrintIntToSlice(buf, @as(u32, 0x1234), 16, false, FormatOptions{ .width = 1 }));

    std.testing.expectEqualSlices(u8, "+42", bufPrintIntToSlice(buf, @as(i32, 42), 10, false, FormatOptions{ .width = 3 }));
    std.testing.expectEqualSlices(u8, "-42", bufPrintIntToSlice(buf, @as(i32, -42), 10, false, FormatOptions{ .width = 3 }));
}

pub fn bufPrintIntToSlice(buf: []u8, value: anytype, base: u8, uppercase: bool, options: FormatOptions) []u8 {
    return buf[0..formatIntBuf(buf, value, base, uppercase, options)];
}

pub fn comptimePrint(comptime fmt: []const u8, args: anytype) *const [count(fmt, args):0]u8 {
    comptime {
        var buf: [count(fmt, args):0]u8 = undefined;
        _ = bufPrint(&buf, fmt, args) catch unreachable;
        buf[buf.len] = 0;
        return &buf;
    }
}

test "comptimePrint" {
    @setEvalBranchQuota(2000);
    std.testing.expectEqual(*const [3:0]u8, @TypeOf(comptime comptimePrint("{}", .{100})));
    std.testing.expectEqualSlices(u8, "100", comptime comptimePrint("{}", .{100}));
}

test "parse u64 digit too big" {
    _ = parseUnsigned(u64, "123a", 10) catch |err| {
        if (err == error.InvalidCharacter) return;
        unreachable;
    };
    unreachable;
}

test "parse unsigned comptime" {
    comptime {
        std.testing.expect((try parseUnsigned(usize, "2", 10)) == 2);
    }
}

test "escaped braces" {
    try expectFmt("escaped: {{foo}}\n", "escaped: {{{{foo}}}}\n", .{});
    try expectFmt("escaped: {foo}\n", "escaped: {{foo}}\n", .{});
}

test "optional" {
    {
        const value: ?i32 = 1234;
        try expectFmt("optional: 1234\n", "optional: {}\n", .{value});
    }
    {
        const value: ?i32 = null;
        try expectFmt("optional: null\n", "optional: {}\n", .{value});
    }
    {
        const value = @intToPtr(?*i32, 0xf000d000);
        try expectFmt("optional: *i32@f000d000\n", "optional: {*}\n", .{value});
    }
}

test "error" {
    {
        const value: anyerror!i32 = 1234;
        try expectFmt("error union: 1234\n", "error union: {}\n", .{value});
    }
    {
        const value: anyerror!i32 = error.InvalidChar;
        try expectFmt("error union: error.InvalidChar\n", "error union: {}\n", .{value});
    }
}

test "int.small" {
    {
        const value: u3 = 0b101;
        try expectFmt("u3: 5\n", "u3: {}\n", .{value});
    }
}

test "int.specifier" {
    {
        const value: u8 = 'a';
        try expectFmt("u8: a\n", "u8: {c}\n", .{value});
    }
    {
        const value: u8 = 0b1100;
        try expectFmt("u8: 0b1100\n", "u8: 0b{b}\n", .{value});
    }
    {
        const value: u16 = 0o1234;
        try expectFmt("u16: 0o1234\n", "u16: 0o{o}\n", .{value});
    }
    {
        const value: u8 = 'a';
        try expectFmt("UTF-8: a\n", "UTF-8: {u}\n", .{value});
    }
    {
        const value: u21 = 0x1F310;
        try expectFmt("UTF-8: 🌐\n", "UTF-8: {u}\n", .{value});
    }
    {
        const value: u21 = 0xD800;
        try expectFmt("UTF-8: �\n", "UTF-8: {u}\n", .{value});
    }
    {
        const value: u21 = 0x110001;
        try expectFmt("UTF-8: �\n", "UTF-8: {u}\n", .{value});
    }
}

test "int.padded" {
    try expectFmt("u8: '   1'", "u8: '{:4}'", .{@as(u8, 1)});
    try expectFmt("u8: '1000'", "u8: '{:0<4}'", .{@as(u8, 1)});
    try expectFmt("u8: '0001'", "u8: '{:0>4}'", .{@as(u8, 1)});
    try expectFmt("u8: '0100'", "u8: '{:0^4}'", .{@as(u8, 1)});
    try expectFmt("i8: '-1  '", "i8: '{:<4}'", .{@as(i8, -1)});
    try expectFmt("i8: '  -1'", "i8: '{:>4}'", .{@as(i8, -1)});
    try expectFmt("i8: ' -1 '", "i8: '{:^4}'", .{@as(i8, -1)});
    try expectFmt("i16: '-1234'", "i16: '{:4}'", .{@as(i16, -1234)});
    try expectFmt("i16: '+1234'", "i16: '{:4}'", .{@as(i16, 1234)});
    try expectFmt("i16: '-12345'", "i16: '{:4}'", .{@as(i16, -12345)});
    try expectFmt("i16: '+12345'", "i16: '{:4}'", .{@as(i16, 12345)});
    try expectFmt("u16: '12345'", "u16: '{:4}'", .{@as(u16, 12345)});

    try expectFmt("UTF-8: 'ü   '", "UTF-8: '{u:<4}'", .{'ü'});
    try expectFmt("UTF-8: '   ü'", "UTF-8: '{u:>4}'", .{'ü'});
    try expectFmt("UTF-8: ' ü  '", "UTF-8: '{u:^4}'", .{'ü'});
}

test "buffer" {
    {
        var buf1: [32]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf1);
        try formatType(1234, "", FormatOptions{}, fbs.writer(), default_max_depth);
        std.testing.expect(mem.eql(u8, fbs.getWritten(), "1234"));

        fbs.reset();
        try formatType('a', "c", FormatOptions{}, fbs.writer(), default_max_depth);
        std.testing.expect(mem.eql(u8, fbs.getWritten(), "a"));

        fbs.reset();
        try formatType(0b1100, "b", FormatOptions{}, fbs.writer(), default_max_depth);
        std.testing.expect(mem.eql(u8, fbs.getWritten(), "1100"));
    }
}

test "array" {
    {
        const value: [3]u8 = "abc".*;
        try expectFmt("array: abc\n", "array: {s}\n", .{value});
        try expectFmt("array: abc\n", "array: {s}\n", .{&value});
        try expectFmt("array: { 97, 98, 99 }\n", "array: {d}\n", .{value});

        var buf: [100]u8 = undefined;
        try expectFmt(
            try bufPrint(buf[0..], "array: [3]u8@{x}\n", .{@ptrToInt(&value)}),
            "array: {*}\n",
            .{&value},
        );
    }
}

test "slice" {
    {
        const value: []const u8 = "abc";
        try expectFmt("slice: abc\n", "slice: {s}\n", .{value});
    }
    {
        var runtime_zero: usize = 0;
        const value = @intToPtr([*]align(1) const []const u8, 0xdeadbeef)[runtime_zero..runtime_zero];
        try expectFmt("slice: []const u8@deadbeef\n", "slice: {*}\n", .{value});
    }
    {
        const null_term_slice: [:0]const u8 = "\x00hello\x00";
        try expectFmt("buf: \x00hello\x00\n", "buf: {s}\n", .{null_term_slice});
    }

    try expectFmt("buf:  Test\n", "buf: {s:5}\n", .{"Test"});
    try expectFmt("buf: Test\n Other text", "buf: {s}\n Other text", .{"Test"});

    {
        var int_slice = [_]u32{ 1, 4096, 391891, 1111111111 };
        var runtime_zero: usize = 0;
        try expectFmt("int: { 1, 4096, 391891, 1111111111 }", "int: {}", .{int_slice[runtime_zero..]});
        try expectFmt("int: { 1, 4096, 391891, 1111111111 }", "int: {d}", .{int_slice[runtime_zero..]});
        try expectFmt("int: { 1, 1000, 5fad3, 423a35c7 }", "int: {x}", .{int_slice[runtime_zero..]});
        try expectFmt("int: { 00001, 01000, 5fad3, 423a35c7 }", "int: {x:0>5}", .{int_slice[runtime_zero..]});
    }
}

test "escape non-printable" {
    try expectFmt("abc", "{e}", .{"abc"});
    try expectFmt("ab\\xffc", "{e}", .{"ab\xffc"});
    try expectFmt("ab\\xFFc", "{E}", .{"ab\xffc"});
}

test "pointer" {
    {
        const value = @intToPtr(*align(1) i32, 0xdeadbeef);
        try expectFmt("pointer: i32@deadbeef\n", "pointer: {}\n", .{value});
        try expectFmt("pointer: i32@deadbeef\n", "pointer: {*}\n", .{value});
    }
    {
        const value = @intToPtr(fn () void, 0xdeadbeef);
        try expectFmt("pointer: fn() void@deadbeef\n", "pointer: {}\n", .{value});
    }
    {
        const value = @intToPtr(fn () void, 0xdeadbeef);
        try expectFmt("pointer: fn() void@deadbeef\n", "pointer: {}\n", .{value});
    }
}

test "cstr" {
    try expectFmt(
        "cstr: Test C\n",
        "cstr: {s}\n",
        .{@ptrCast([*c]const u8, "Test C")},
    );
    try expectFmt(
        "cstr:     Test C\n",
        "cstr: {s:10}\n",
        .{@ptrCast([*c]const u8, "Test C")},
    );
}

test "filesize" {
    try expectFmt("file size: 63MiB\n", "file size: {Bi}\n", .{@as(usize, 63 * 1024 * 1024)});
    try expectFmt("file size: 66.06MB\n", "file size: {B:.2}\n", .{@as(usize, 63 * 1024 * 1024)});
}

test "struct" {
    {
        const Struct = struct {
            field: u8,
        };
        const value = Struct{ .field = 42 };
        try expectFmt("struct: Struct{ .field = 42 }\n", "struct: {}\n", .{value});
        try expectFmt("struct: Struct{ .field = 42 }\n", "struct: {}\n", .{&value});
    }
    {
        const Struct = struct {
            a: u0,
            b: u1,
        };
        const value = Struct{ .a = 0, .b = 1 };
        try expectFmt("struct: Struct{ .a = 0, .b = 1 }\n", "struct: {}\n", .{value});
    }
}

test "enum" {
    const Enum = enum {
        One,
        Two,
    };
    const value = Enum.Two;
    try expectFmt("enum: Enum.Two\n", "enum: {}\n", .{value});
    try expectFmt("enum: Enum.Two\n", "enum: {}\n", .{&value});
    try expectFmt("enum: Enum.One\n", "enum: {x}\n", .{Enum.One});
    try expectFmt("enum: Enum.Two\n", "enum: {X}\n", .{Enum.Two});

    // test very large enum to verify ct branch quota is large enough
    try expectFmt("enum: Win32Error.INVALID_FUNCTION\n", "enum: {}\n", .{std.os.windows.Win32Error.INVALID_FUNCTION});
}

test "non-exhaustive enum" {
    const Enum = enum(u16) {
        One = 0x000f,
        Two = 0xbeef,
        _,
    };
    try expectFmt("enum: Enum.One\n", "enum: {}\n", .{Enum.One});
    try expectFmt("enum: Enum.Two\n", "enum: {}\n", .{Enum.Two});
    try expectFmt("enum: Enum(4660)\n", "enum: {}\n", .{@intToEnum(Enum, 0x1234)});
    try expectFmt("enum: Enum.One\n", "enum: {x}\n", .{Enum.One});
    try expectFmt("enum: Enum.Two\n", "enum: {x}\n", .{Enum.Two});
    try expectFmt("enum: Enum.Two\n", "enum: {X}\n", .{Enum.Two});
    try expectFmt("enum: Enum(1234)\n", "enum: {x}\n", .{@intToEnum(Enum, 0x1234)});
}

test "float.scientific" {
    try expectFmt("f32: 1.34000003e+00", "f32: {e}", .{@as(f32, 1.34)});
    try expectFmt("f32: 1.23400001e+01", "f32: {e}", .{@as(f32, 12.34)});
    try expectFmt("f64: -1.234e+11", "f64: {e}", .{@as(f64, -12.34e10)});
    try expectFmt("f64: 9.99996e-40", "f64: {e}", .{@as(f64, 9.999960e-40)});
}

test "float.scientific.precision" {
    try expectFmt("f64: 1.40971e-42", "f64: {e:.5}", .{@as(f64, 1.409706e-42)});
    try expectFmt("f64: 1.00000e-09", "f64: {e:.5}", .{@as(f64, @bitCast(f32, @as(u32, 814313563)))});
    try expectFmt("f64: 7.81250e-03", "f64: {e:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1006632960)))});
    // libc rounds 1.000005e+05 to 1.00000e+05 but zig does 1.00001e+05.
    // In fact, libc doesn't round a lot of 5 cases up when one past the precision point.
    try expectFmt("f64: 1.00001e+05", "f64: {e:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1203982400)))});
}

test "float.special" {
    try expectFmt("f64: nan", "f64: {}", .{math.nan_f64});
    // negative nan is not defined by IEE 754,
    // and ARM thus normalizes it to positive nan
    if (builtin.arch != builtin.Arch.arm) {
        try expectFmt("f64: -nan", "f64: {}", .{-math.nan_f64});
    }
    try expectFmt("f64: inf", "f64: {}", .{math.inf_f64});
    try expectFmt("f64: -inf", "f64: {}", .{-math.inf_f64});
}

test "float.decimal" {
    try expectFmt("f64: 152314000000000000000000000000", "f64: {d}", .{@as(f64, 1.52314e+29)});
    try expectFmt("f32: 0", "f32: {d}", .{@as(f32, 0.0)});
    try expectFmt("f32: 1.1", "f32: {d:.1}", .{@as(f32, 1.1234)});
    try expectFmt("f32: 1234.57", "f32: {d:.2}", .{@as(f32, 1234.567)});
    // -11.1234 is converted to f64 -11.12339... internally (errol3() function takes f64).
    // -11.12339... is rounded back up to -11.1234
    try expectFmt("f32: -11.1234", "f32: {d:.4}", .{@as(f32, -11.1234)});
    try expectFmt("f32: 91.12345", "f32: {d:.5}", .{@as(f32, 91.12345)});
    try expectFmt("f64: 91.1234567890", "f64: {d:.10}", .{@as(f64, 91.12345678901235)});
    try expectFmt("f64: 0.00000", "f64: {d:.5}", .{@as(f64, 0.0)});
    try expectFmt("f64: 6", "f64: {d:.0}", .{@as(f64, 5.700)});
    try expectFmt("f64: 10.0", "f64: {d:.1}", .{@as(f64, 9.999)});
    try expectFmt("f64: 1.000", "f64: {d:.3}", .{@as(f64, 1.0)});
    try expectFmt("f64: 0.00030000", "f64: {d:.8}", .{@as(f64, 0.0003)});
    try expectFmt("f64: 0.00000", "f64: {d:.5}", .{@as(f64, 1.40130e-45)});
    try expectFmt("f64: 0.00000", "f64: {d:.5}", .{@as(f64, 9.999960e-40)});
}

test "float.libc.sanity" {
    try expectFmt("f64: 0.00001", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 916964781)))});
    try expectFmt("f64: 0.00001", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 925353389)))});
    try expectFmt("f64: 0.10000", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1036831278)))});
    try expectFmt("f64: 1.00000", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1065353133)))});
    try expectFmt("f64: 10.00000", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1092616192)))});

    // libc differences
    //
    // This is 0.015625 exactly according to gdb. We thus round down,
    // however glibc rounds up for some reason. This occurs for all
    // floats of the form x.yyyy25 on a precision point.
    try expectFmt("f64: 0.01563", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1015021568)))});
    // errol3 rounds to ... 630 but libc rounds to ...632. Grisu3
    // also rounds to 630 so I'm inclined to believe libc is not
    // optimal here.
    try expectFmt("f64: 18014400656965630.00000", "f64: {d:.5}", .{@as(f64, @bitCast(f32, @as(u32, 1518338049)))});
}

test "custom" {
    const Vec2 = struct {
        const SelfType = @This();
        x: f32,
        y: f32,

        pub fn format(
            self: SelfType,
            comptime fmt: []const u8,
            options: FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "p")) {
                return std.fmt.format(writer, "({d:.3},{d:.3})", .{ self.x, self.y });
            } else if (comptime std.mem.eql(u8, fmt, "d")) {
                return std.fmt.format(writer, "{d:.3}x{d:.3}", .{ self.x, self.y });
            } else {
                @compileError("Unknown format character: '" ++ fmt ++ "'");
            }
        }
    };

    var buf1: [32]u8 = undefined;
    var value = Vec2{
        .x = 10.2,
        .y = 2.22,
    };
    try expectFmt("point: (10.200,2.220)\n", "point: {}\n", .{&value});
    try expectFmt("dim: 10.200x2.220\n", "dim: {d}\n", .{&value});

    // same thing but not passing a pointer
    try expectFmt("point: (10.200,2.220)\n", "point: {}\n", .{value});
    try expectFmt("dim: 10.200x2.220\n", "dim: {d}\n", .{value});
}

test "struct" {
    const S = struct {
        a: u32,
        b: anyerror,
    };

    const inst = S{
        .a = 456,
        .b = error.Unused,
    };

    try expectFmt("S{ .a = 456, .b = error.Unused }", "{}", .{inst});
}

test "union" {
    const TU = union(enum) {
        float: f32,
        int: u32,
    };

    const UU = union {
        float: f32,
        int: u32,
    };

    const EU = extern union {
        float: f32,
        int: u32,
    };

    const tu_inst = TU{ .int = 123 };
    const uu_inst = UU{ .int = 456 };
    const eu_inst = EU{ .float = 321.123 };

    try expectFmt("TU{ .int = 123 }", "{}", .{tu_inst});

    var buf: [100]u8 = undefined;
    const uu_result = try bufPrint(buf[0..], "{}", .{uu_inst});
    std.testing.expect(mem.eql(u8, uu_result[0..3], "UU@"));

    const eu_result = try bufPrint(buf[0..], "{}", .{eu_inst});
    std.testing.expect(mem.eql(u8, uu_result[0..3], "EU@"));
}

test "enum" {
    const E = enum {
        One,
        Two,
        Three,
    };

    const inst = E.Two;

    try expectFmt("E.Two", "{}", .{inst});
}

test "struct.self-referential" {
    const S = struct {
        const SelfType = @This();
        a: ?*SelfType,
    };

    var inst = S{
        .a = null,
    };
    inst.a = &inst;

    try expectFmt("S{ .a = S{ .a = S{ .a = S{ ... } } } }", "{}", .{inst});
}

test "struct.zero-size" {
    const A = struct {
        fn foo() void {}
    };
    const B = struct {
        a: A,
        c: i32,
    };

    const a = A{};
    const b = B{ .a = a, .c = 0 };

    try expectFmt("B{ .a = A{ }, .c = 0 }", "{}", .{b});
}

test "bytes.hex" {
    const some_bytes = "\xCA\xFE\xBA\xBE";
    try expectFmt("lowercase: cafebabe\n", "lowercase: {x}\n", .{some_bytes});
    try expectFmt("uppercase: CAFEBABE\n", "uppercase: {X}\n", .{some_bytes});
    //Test Slices
    try expectFmt("uppercase: CAFE\n", "uppercase: {X}\n", .{some_bytes[0..2]});
    try expectFmt("lowercase: babe\n", "lowercase: {x}\n", .{some_bytes[2..]});
    const bytes_with_zeros = "\x00\x0E\xBA\xBE";
    try expectFmt("lowercase: 000ebabe\n", "lowercase: {x}\n", .{bytes_with_zeros});
}

pub const trim = @compileError("deprecated; use std.mem.trim with std.ascii.spaces instead");
pub const isWhiteSpace = @compileError("deprecated; use std.ascii.isSpace instead");

pub fn hexToBytes(out: []u8, input: []const u8) !void {
    if (out.len * 2 < input.len)
        return error.InvalidLength;

    var in_i: usize = 0;
    while (in_i != input.len) : (in_i += 2) {
        const hi = try charToDigit(input[in_i], 16);
        const lo = try charToDigit(input[in_i + 1], 16);
        out[in_i / 2] = (hi << 4) | lo;
    }
}

test "hexToBytes" {
    const test_hex_str = "909A312BB12ED1F819B3521AC4C1E896F2160507FFC1C8381E3B07BB16BD1706";
    var pb: [32]u8 = undefined;
    try hexToBytes(pb[0..], test_hex_str);
    try expectFmt(test_hex_str, "{X}", .{pb});
}

test "formatIntValue with comptime_int" {
    const value: comptime_int = 123456789123456789;

    var buf: [20]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatIntValue(value, "", FormatOptions{}, fbs.writer());
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "123456789123456789"));
}

test "formatFloatValue with comptime_float" {
    const value: comptime_float = 1.0;

    var buf: [20]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatFloatValue(value, "", FormatOptions{}, fbs.writer());
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "1.0e+00"));

    try expectFmt("1.0e+00", "{}", .{value});
    try expectFmt("1.0e+00", "{}", .{1.0});
}

test "formatType max_depth" {
    const Vec2 = struct {
        const SelfType = @This();
        x: f32,
        y: f32,

        pub fn format(
            self: SelfType,
            comptime fmt: []const u8,
            options: FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len == 0) {
                return std.fmt.format(writer, "({d:.3},{d:.3})", .{ self.x, self.y });
            } else {
                @compileError("Unknown format string: '" ++ fmt ++ "'");
            }
        }
    };
    const E = enum {
        One,
        Two,
        Three,
    };
    const TU = union(enum) {
        const SelfType = @This();
        float: f32,
        int: u32,
        ptr: ?*SelfType,
    };
    const S = struct {
        const SelfType = @This();
        a: ?*SelfType,
        tu: TU,
        e: E,
        vec: Vec2,
    };

    var inst = S{
        .a = null,
        .tu = TU{ .ptr = null },
        .e = E.Two,
        .vec = Vec2{ .x = 10.2, .y = 2.22 },
    };
    inst.a = &inst;
    inst.tu.ptr = &inst.tu;

    var buf: [1000]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatType(inst, "", FormatOptions{}, fbs.writer(), 0);
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "S{ ... }"));

    fbs.reset();
    try formatType(inst, "", FormatOptions{}, fbs.writer(), 1);
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "S{ .a = S{ ... }, .tu = TU{ ... }, .e = E.Two, .vec = (10.200,2.220) }"));

    fbs.reset();
    try formatType(inst, "", FormatOptions{}, fbs.writer(), 2);
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "S{ .a = S{ .a = S{ ... }, .tu = TU{ ... }, .e = E.Two, .vec = (10.200,2.220) }, .tu = TU{ .ptr = TU{ ... } }, .e = E.Two, .vec = (10.200,2.220) }"));

    fbs.reset();
    try formatType(inst, "", FormatOptions{}, fbs.writer(), 3);
    std.testing.expect(mem.eql(u8, fbs.getWritten(), "S{ .a = S{ .a = S{ .a = S{ ... }, .tu = TU{ ... }, .e = E.Two, .vec = (10.200,2.220) }, .tu = TU{ .ptr = TU{ ... } }, .e = E.Two, .vec = (10.200,2.220) }, .tu = TU{ .ptr = TU{ .ptr = TU{ ... } } }, .e = E.Two, .vec = (10.200,2.220) }"));
}

test "positional" {
    try expectFmt("2 1 0", "{2} {1} {0}", .{ @as(usize, 0), @as(usize, 1), @as(usize, 2) });
    try expectFmt("2 1 0", "{2} {1} {}", .{ @as(usize, 0), @as(usize, 1), @as(usize, 2) });
    try expectFmt("0 0", "{0} {0}", .{@as(usize, 0)});
    try expectFmt("0 1", "{} {1}", .{ @as(usize, 0), @as(usize, 1) });
    try expectFmt("1 0 0 1", "{1} {} {0} {}", .{ @as(usize, 0), @as(usize, 1) });
}

test "positional with specifier" {
    try expectFmt("10.0", "{0d:.1}", .{@as(f64, 9.999)});
}

test "positional/alignment/width/precision" {
    try expectFmt("10.0", "{0d: >3.1}", .{@as(f64, 9.999)});
}

test "vector" {
    if (builtin.arch == .mipsel or builtin.arch == .mips) {
        // https://github.com/ziglang/zig/issues/3317
        return error.SkipZigTest;
    }
    if (builtin.arch == .riscv64) {
        // https://github.com/ziglang/zig/issues/4486
        return error.SkipZigTest;
    }
    if (builtin.arch == .wasm32) {
        // https://github.com/ziglang/zig/issues/5339
        return error.SkipZigTest;
    }

    const vbool: std.meta.Vector(4, bool) = [_]bool{ true, false, true, false };
    const vi64: std.meta.Vector(4, i64) = [_]i64{ -2, -1, 0, 1 };
    const vu64: std.meta.Vector(4, u64) = [_]u64{ 1000, 2000, 3000, 4000 };

    try expectFmt("{ true, false, true, false }", "{}", .{vbool});
    try expectFmt("{ -2, -1, 0, 1 }", "{}", .{vi64});
    try expectFmt("{    -2,    -1,    +0,    +1 }", "{d:5}", .{vi64});
    try expectFmt("{ 1000, 2000, 3000, 4000 }", "{}", .{vu64});
    try expectFmt("{ 3e8, 7d0, bb8, fa0 }", "{x}", .{vu64});
    try expectFmt("{ 1kB, 2kB, 3kB, 4kB }", "{B}", .{vu64});
    try expectFmt("{ 1000B, 1.953125KiB, 2.9296875KiB, 3.90625KiB }", "{Bi}", .{vu64});
}

test "enum-literal" {
    try expectFmt(".hello_world", "{s}", .{.hello_world});
}

test "padding" {
    try expectFmt("Simple", "{s}", .{"Simple"});
    try expectFmt("      true", "{:10}", .{true});
    try expectFmt("      true", "{:>10}", .{true});
    try expectFmt("======true", "{:=>10}", .{true});
    try expectFmt("true======", "{:=<10}", .{true});
    try expectFmt("   true   ", "{:^10}", .{true});
    try expectFmt("===true===", "{:=^10}", .{true});
    try expectFmt("           Minimum width", "{s:18} width", .{"Minimum"});
    try expectFmt("==================Filled", "{s:=>24}", .{"Filled"});
    try expectFmt("        Centered        ", "{s:^24}", .{"Centered"});
    try expectFmt("-", "{s:-^1}", .{""});
    try expectFmt("==crêpe===", "{s:=^10}", .{"crêpe"});
    try expectFmt("=====crêpe", "{s:=>10}", .{"crêpe"});
    try expectFmt("crêpe=====", "{s:=<10}", .{"crêpe"});
}

test "decimal float padding" {
    var number: f32 = 3.1415;
    try expectFmt("left-pad:   **3.141\n", "left-pad:   {d:*>7.3}\n", .{number});
    try expectFmt("center-pad: *3.141*\n", "center-pad: {d:*^7.3}\n", .{number});
    try expectFmt("right-pad:  3.141**\n", "right-pad:  {d:*<7.3}\n", .{number});
}

test "sci float padding" {
    var number: f32 = 3.1415;
    try expectFmt("left-pad:   **3.141e+00\n", "left-pad:   {e:*>11.3}\n", .{number});
    try expectFmt("center-pad: *3.141e+00*\n", "center-pad: {e:*^11.3}\n", .{number});
    try expectFmt("right-pad:  3.141e+00**\n", "right-pad:  {e:*<11.3}\n", .{number});
}

test "null" {
    const inst = null;
    try expectFmt("null", "{}", .{inst});
}

test "type" {
    try expectFmt("u8", "{}", .{u8});
    try expectFmt("?f32", "{}", .{?f32});
    try expectFmt("[]const u8", "{}", .{[]const u8});
}

test "named arguments" {
    try expectFmt("hello world!", "{s} world{c}", .{ "hello", '!' });
    try expectFmt("hello world!", "{[greeting]s} world{[punctuation]c}", .{ .punctuation = '!', .greeting = "hello" });
    try expectFmt("hello world!", "{[1]s} world{[0]c}", .{ '!', "hello" });
}

test "runtime width specifier" {
    var width: usize = 9;
    try expectFmt("~~hello~~", "{s:~^[1]}", .{ "hello", width });
    try expectFmt("~~hello~~", "{s:~^[width]}", .{ .string = "hello", .width = width });
}

test "runtime precision specifier" {
    var number: f32 = 3.1415;
    var precision: usize = 2;
    try expectFmt("3.14e+00", "{:1.[1]}", .{ number, precision });
    try expectFmt("3.14e+00", "{:1.[precision]}", .{ .number = number, .precision = precision });
}
