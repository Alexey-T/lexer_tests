const std = @import("std");
const Type = @import("type.zig").Type;
const log2 = std.math.log2;
const assert = std.debug.assert;
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;
const Target = std.Target;
const Allocator = std.mem.Allocator;
const Module = @import("Module.zig");
const ir = @import("ir.zig");

/// This is the raw data, with no bookkeeping, no memory awareness,
/// no de-duplication, and no type system awareness.
/// It's important for this type to be small.
/// This union takes advantage of the fact that the first page of memory
/// is unmapped, giving us 4096 possible enum tags that have no payload.
pub const Value = extern union {
    /// If the tag value is less than Tag.no_payload_count, then no pointer
    /// dereference is needed.
    tag_if_small_enough: usize,
    ptr_otherwise: *Payload,

    pub const Tag = enum {
        // The first section of this enum are tags that require no payload.
        u8_type,
        i8_type,
        u16_type,
        i16_type,
        u32_type,
        i32_type,
        u64_type,
        i64_type,
        usize_type,
        isize_type,
        c_short_type,
        c_ushort_type,
        c_int_type,
        c_uint_type,
        c_long_type,
        c_ulong_type,
        c_longlong_type,
        c_ulonglong_type,
        c_longdouble_type,
        f16_type,
        f32_type,
        f64_type,
        f128_type,
        c_void_type,
        bool_type,
        void_type,
        type_type,
        anyerror_type,
        comptime_int_type,
        comptime_float_type,
        noreturn_type,
        null_type,
        undefined_type,
        fn_noreturn_no_args_type,
        fn_void_no_args_type,
        fn_naked_noreturn_no_args_type,
        fn_ccc_void_no_args_type,
        single_const_pointer_to_comptime_int_type,
        const_slice_u8_type,
        enum_literal_type,
        anyframe_type,

        undef,
        zero,
        one,
        void_value,
        unreachable_value,
        empty_struct_value,
        empty_array,
        null_value,
        bool_true,
        bool_false, // See last_no_payload_tag below.
        // After this, the tag requires a payload.

        ty,
        int_type,
        int_u64,
        int_i64,
        int_big_positive,
        int_big_negative,
        function,
        extern_fn,
        variable,
        /// Represents a pointer to another immutable value.
        ref_val,
        /// Represents a pointer to a decl, not the value of the decl.
        decl_ref,
        elem_ptr,
        /// A slice of u8 whose memory is managed externally.
        bytes,
        /// This value is repeated some number of times. The amount of times to repeat
        /// is stored externally.
        repeated,
        float_16,
        float_32,
        float_64,
        float_128,
        enum_literal,
        error_set,
        @"error",
        /// This is a special value that tracks a set of types that have been stored
        /// to an inferred allocation. It does not support any of the normal value queries.
        inferred_alloc,

        pub const last_no_payload_tag = Tag.bool_false;
        pub const no_payload_count = @enumToInt(last_no_payload_tag) + 1;

        pub fn Type(comptime t: Tag) type {
            return switch (t) {
                .u8_type,
                .i8_type,
                .u16_type,
                .i16_type,
                .u32_type,
                .i32_type,
                .u64_type,
                .i64_type,
                .usize_type,
                .isize_type,
                .c_short_type,
                .c_ushort_type,
                .c_int_type,
                .c_uint_type,
                .c_long_type,
                .c_ulong_type,
                .c_longlong_type,
                .c_ulonglong_type,
                .c_longdouble_type,
                .f16_type,
                .f32_type,
                .f64_type,
                .f128_type,
                .c_void_type,
                .bool_type,
                .void_type,
                .type_type,
                .anyerror_type,
                .comptime_int_type,
                .comptime_float_type,
                .noreturn_type,
                .null_type,
                .undefined_type,
                .fn_noreturn_no_args_type,
                .fn_void_no_args_type,
                .fn_naked_noreturn_no_args_type,
                .fn_ccc_void_no_args_type,
                .single_const_pointer_to_comptime_int_type,
                .const_slice_u8_type,
                .enum_literal_type,
                .anyframe_type,
                .undef,
                .zero,
                .one,
                .void_value,
                .unreachable_value,
                .empty_struct_value,
                .empty_array,
                .null_value,
                .bool_true,
                .bool_false,
                => @compileError("Value Tag " ++ @tagName(t) ++ " has no payload"),

                .int_big_positive,
                .int_big_negative,
                => Payload.BigInt,

                .extern_fn,
                .decl_ref,
                => Payload.Decl,

                .ref_val,
                .repeated,
                => Payload.SubValue,

                .bytes,
                .enum_literal,
                => Payload.Bytes,

                .ty => Payload.Ty,
                .int_type => Payload.IntType,
                .int_u64 => Payload.U64,
                .int_i64 => Payload.I64,
                .function => Payload.Function,
                .variable => Payload.Variable,
                .elem_ptr => Payload.ElemPtr,
                .float_16 => Payload.Float_16,
                .float_32 => Payload.Float_32,
                .float_64 => Payload.Float_64,
                .float_128 => Payload.Float_128,
                .error_set => Payload.ErrorSet,
                .@"error" => Payload.Error,
                .inferred_alloc => Payload.InferredAlloc,
            };
        }

        pub fn create(comptime t: Tag, ally: *Allocator, data: Data(t)) error{OutOfMemory}!Value {
            const ptr = try ally.create(t.Type());
            ptr.* = .{
                .base = .{ .tag = t },
                .data = data,
            };
            return Value{ .ptr_otherwise = &ptr.base };
        }

        pub fn Data(comptime t: Tag) type {
            return std.meta.fieldInfo(t.Type(), .data).field_type;
        }
    };

    pub fn initTag(small_tag: Tag) Value {
        assert(@enumToInt(small_tag) < Tag.no_payload_count);
        return .{ .tag_if_small_enough = @enumToInt(small_tag) };
    }

    pub fn initPayload(payload: *Payload) Value {
        assert(@enumToInt(payload.tag) >= Tag.no_payload_count);
        return .{ .ptr_otherwise = payload };
    }

    pub fn tag(self: Value) Tag {
        if (self.tag_if_small_enough < Tag.no_payload_count) {
            return @intToEnum(Tag, @intCast(@TagType(Tag), self.tag_if_small_enough));
        } else {
            return self.ptr_otherwise.tag;
        }
    }

    /// Prefer `castTag` to this.
    pub fn cast(self: Value, comptime T: type) ?*T {
        if (@hasField(T, "base_tag")) {
            return base.castTag(T.base_tag);
        }
        if (self.tag_if_small_enough < Tag.no_payload_count) {
            return null;
        }
        inline for (@typeInfo(Tag).Enum.fields) |field| {
            if (field.value < Tag.no_payload_count)
                continue;
            const t = @intToEnum(Tag, field.value);
            if (self.ptr_otherwise.tag == t) {
                if (T == t.Type()) {
                    return @fieldParentPtr(T, "base", self.ptr_otherwise);
                }
                return null;
            }
        }
        unreachable;
    }

    pub fn castTag(self: Value, comptime t: Tag) ?*t.Type() {
        if (self.tag_if_small_enough < Tag.no_payload_count)
            return null;

        if (self.ptr_otherwise.tag == t)
            return @fieldParentPtr(t.Type(), "base", self.ptr_otherwise);

        return null;
    }

    pub fn copy(self: Value, allocator: *Allocator) error{OutOfMemory}!Value {
        if (self.tag_if_small_enough < Tag.no_payload_count) {
            return Value{ .tag_if_small_enough = self.tag_if_small_enough };
        } else switch (self.ptr_otherwise.tag) {
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .undef,
            .zero,
            .one,
            .void_value,
            .unreachable_value,
            .empty_array,
            .null_value,
            .bool_true,
            .bool_false,
            .empty_struct_value,
            => unreachable,

            .ty => {
                const payload = self.castTag(.ty).?;
                const new_payload = try allocator.create(Payload.Ty);
                new_payload.* = .{
                    .base = payload.base,
                    .data = try payload.data.copy(allocator),
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .int_type => return self.copyPayloadShallow(allocator, Payload.IntType),
            .int_u64 => return self.copyPayloadShallow(allocator, Payload.U64),
            .int_i64 => return self.copyPayloadShallow(allocator, Payload.I64),
            .int_big_positive, .int_big_negative => {
                const old_payload = self.cast(Payload.BigInt).?;
                const new_payload = try allocator.create(Payload.BigInt);
                new_payload.* = .{
                    .base = .{ .tag = self.ptr_otherwise.tag },
                    .data = try allocator.dupe(std.math.big.Limb, old_payload.data),
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .function => return self.copyPayloadShallow(allocator, Payload.Function),
            .extern_fn => return self.copyPayloadShallow(allocator, Payload.Decl),
            .variable => return self.copyPayloadShallow(allocator, Payload.Variable),
            .ref_val => {
                const payload = self.castTag(.ref_val).?;
                const new_payload = try allocator.create(Payload.SubValue);
                new_payload.* = .{
                    .base = payload.base,
                    .data = try payload.data.copy(allocator),
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .decl_ref => return self.copyPayloadShallow(allocator, Payload.Decl),
            .elem_ptr => {
                const payload = self.castTag(.elem_ptr).?;
                const new_payload = try allocator.create(Payload.ElemPtr);
                new_payload.* = .{
                    .base = payload.base,
                    .data = .{
                        .array_ptr = try payload.data.array_ptr.copy(allocator),
                        .index = payload.data.index,
                    },
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .bytes => return self.copyPayloadShallow(allocator, Payload.Bytes),
            .repeated => {
                const payload = self.castTag(.repeated).?;
                const new_payload = try allocator.create(Payload.SubValue);
                new_payload.* = .{
                    .base = payload.base,
                    .data = try payload.data.copy(allocator),
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .float_16 => return self.copyPayloadShallow(allocator, Payload.Float_16),
            .float_32 => return self.copyPayloadShallow(allocator, Payload.Float_32),
            .float_64 => return self.copyPayloadShallow(allocator, Payload.Float_64),
            .float_128 => return self.copyPayloadShallow(allocator, Payload.Float_128),
            .enum_literal => {
                const payload = self.castTag(.enum_literal).?;
                const new_payload = try allocator.create(Payload.Bytes);
                new_payload.* = .{
                    .base = payload.base,
                    .data = try allocator.dupe(u8, payload.data),
                };
                return Value{ .ptr_otherwise = &new_payload.base };
            },
            .@"error" => return self.copyPayloadShallow(allocator, Payload.Error),

            .error_set => return self.copyPayloadShallow(allocator, Payload.ErrorSet),

            .inferred_alloc => unreachable,
        }
    }

    fn copyPayloadShallow(self: Value, allocator: *Allocator, comptime T: type) error{OutOfMemory}!Value {
        const payload = self.cast(T).?;
        const new_payload = try allocator.create(T);
        new_payload.* = payload.*;
        return Value{ .ptr_otherwise = &new_payload.base };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        out_stream: anytype,
    ) !void {
        comptime assert(fmt.len == 0);
        var val = self;
        while (true) switch (val.tag()) {
            .u8_type => return out_stream.writeAll("u8"),
            .i8_type => return out_stream.writeAll("i8"),
            .u16_type => return out_stream.writeAll("u16"),
            .i16_type => return out_stream.writeAll("i16"),
            .u32_type => return out_stream.writeAll("u32"),
            .i32_type => return out_stream.writeAll("i32"),
            .u64_type => return out_stream.writeAll("u64"),
            .i64_type => return out_stream.writeAll("i64"),
            .isize_type => return out_stream.writeAll("isize"),
            .usize_type => return out_stream.writeAll("usize"),
            .c_short_type => return out_stream.writeAll("c_short"),
            .c_ushort_type => return out_stream.writeAll("c_ushort"),
            .c_int_type => return out_stream.writeAll("c_int"),
            .c_uint_type => return out_stream.writeAll("c_uint"),
            .c_long_type => return out_stream.writeAll("c_long"),
            .c_ulong_type => return out_stream.writeAll("c_ulong"),
            .c_longlong_type => return out_stream.writeAll("c_longlong"),
            .c_ulonglong_type => return out_stream.writeAll("c_ulonglong"),
            .c_longdouble_type => return out_stream.writeAll("c_longdouble"),
            .f16_type => return out_stream.writeAll("f16"),
            .f32_type => return out_stream.writeAll("f32"),
            .f64_type => return out_stream.writeAll("f64"),
            .f128_type => return out_stream.writeAll("f128"),
            .c_void_type => return out_stream.writeAll("c_void"),
            .bool_type => return out_stream.writeAll("bool"),
            .void_type => return out_stream.writeAll("void"),
            .type_type => return out_stream.writeAll("type"),
            .anyerror_type => return out_stream.writeAll("anyerror"),
            .comptime_int_type => return out_stream.writeAll("comptime_int"),
            .comptime_float_type => return out_stream.writeAll("comptime_float"),
            .noreturn_type => return out_stream.writeAll("noreturn"),
            .null_type => return out_stream.writeAll("@Type(.Null)"),
            .undefined_type => return out_stream.writeAll("@Type(.Undefined)"),
            .fn_noreturn_no_args_type => return out_stream.writeAll("fn() noreturn"),
            .fn_void_no_args_type => return out_stream.writeAll("fn() void"),
            .fn_naked_noreturn_no_args_type => return out_stream.writeAll("fn() callconv(.Naked) noreturn"),
            .fn_ccc_void_no_args_type => return out_stream.writeAll("fn() callconv(.C) void"),
            .single_const_pointer_to_comptime_int_type => return out_stream.writeAll("*const comptime_int"),
            .const_slice_u8_type => return out_stream.writeAll("[]const u8"),
            .enum_literal_type => return out_stream.writeAll("@Type(.EnumLiteral)"),
            .anyframe_type => return out_stream.writeAll("anyframe"),

            // TODO this should print `NAME{}`
            .empty_struct_value => return out_stream.writeAll("struct {}{}"),
            .null_value => return out_stream.writeAll("null"),
            .undef => return out_stream.writeAll("undefined"),
            .zero => return out_stream.writeAll("0"),
            .one => return out_stream.writeAll("1"),
            .void_value => return out_stream.writeAll("{}"),
            .unreachable_value => return out_stream.writeAll("unreachable"),
            .bool_true => return out_stream.writeAll("true"),
            .bool_false => return out_stream.writeAll("false"),
            .ty => return val.castTag(.ty).?.data.format("", options, out_stream),
            .int_type => {
                const int_type = val.castTag(.int_type).?.data;
                return out_stream.print("{s}{d}", .{
                    if (int_type.signed) "s" else "u",
                    int_type.bits,
                });
            },
            .int_u64 => return std.fmt.formatIntValue(val.castTag(.int_u64).?.data, "", options, out_stream),
            .int_i64 => return std.fmt.formatIntValue(val.castTag(.int_i64).?.data, "", options, out_stream),
            .int_big_positive => return out_stream.print("{}", .{val.castTag(.int_big_positive).?.asBigInt()}),
            .int_big_negative => return out_stream.print("{}", .{val.castTag(.int_big_negative).?.asBigInt()}),
            .function => return out_stream.writeAll("(function)"),
            .extern_fn => return out_stream.writeAll("(extern function)"),
            .variable => return out_stream.writeAll("(variable)"),
            .ref_val => {
                const ref_val = val.castTag(.ref_val).?.data;
                try out_stream.writeAll("&const ");
                val = ref_val;
            },
            .decl_ref => return out_stream.writeAll("(decl ref)"),
            .elem_ptr => {
                const elem_ptr = val.castTag(.elem_ptr).?.data;
                try out_stream.print("&[{}] ", .{elem_ptr.index});
                val = elem_ptr.array_ptr;
            },
            .empty_array => return out_stream.writeAll(".{}"),
            .enum_literal => return out_stream.print(".{}", .{std.zig.fmtId(self.castTag(.enum_literal).?.data)}),
            .bytes => return out_stream.print("\"{}\"", .{std.zig.fmtEscapes(self.castTag(.bytes).?.data)}),
            .repeated => {
                try out_stream.writeAll("(repeated) ");
                val = val.castTag(.repeated).?.data;
            },
            .float_16 => return out_stream.print("{}", .{val.castTag(.float_16).?.data}),
            .float_32 => return out_stream.print("{}", .{val.castTag(.float_32).?.data}),
            .float_64 => return out_stream.print("{}", .{val.castTag(.float_64).?.data}),
            .float_128 => return out_stream.print("{}", .{val.castTag(.float_128).?.data}),
            .error_set => {
                const error_set = val.castTag(.error_set).?.data;
                try out_stream.writeAll("error{");
                var it = error_set.fields.iterator();
                while (it.next()) |entry| {
                    try out_stream.print("{},", .{entry.value});
                }
                return out_stream.writeAll("}");
            },
            .@"error" => return out_stream.print("error.{s}", .{val.castTag(.@"error").?.data.name}),
            .inferred_alloc => return out_stream.writeAll("(inferred allocation value)"),
        };
    }

    /// Asserts that the value is representable as an array of bytes.
    /// Copies the value into a freshly allocated slice of memory, which is owned by the caller.
    pub fn toAllocatedBytes(self: Value, allocator: *Allocator) ![]u8 {
        if (self.castTag(.bytes)) |payload| {
            return std.mem.dupe(allocator, u8, payload.data);
        }
        if (self.castTag(.enum_literal)) |payload| {
            return std.mem.dupe(allocator, u8, payload.data);
        }
        if (self.castTag(.repeated)) |payload| {
            @panic("TODO implement toAllocatedBytes for this Value tag");
        }
        if (self.castTag(.decl_ref)) |payload| {
            const val = try payload.data.value();
            return val.toAllocatedBytes(allocator);
        }
        unreachable;
    }

    /// Asserts that the value is representable as a type.
    pub fn toType(self: Value, allocator: *Allocator) !Type {
        return switch (self.tag()) {
            .ty => self.castTag(.ty).?.data,
            .u8_type => Type.initTag(.u8),
            .i8_type => Type.initTag(.i8),
            .u16_type => Type.initTag(.u16),
            .i16_type => Type.initTag(.i16),
            .u32_type => Type.initTag(.u32),
            .i32_type => Type.initTag(.i32),
            .u64_type => Type.initTag(.u64),
            .i64_type => Type.initTag(.i64),
            .usize_type => Type.initTag(.usize),
            .isize_type => Type.initTag(.isize),
            .c_short_type => Type.initTag(.c_short),
            .c_ushort_type => Type.initTag(.c_ushort),
            .c_int_type => Type.initTag(.c_int),
            .c_uint_type => Type.initTag(.c_uint),
            .c_long_type => Type.initTag(.c_long),
            .c_ulong_type => Type.initTag(.c_ulong),
            .c_longlong_type => Type.initTag(.c_longlong),
            .c_ulonglong_type => Type.initTag(.c_ulonglong),
            .c_longdouble_type => Type.initTag(.c_longdouble),
            .f16_type => Type.initTag(.f16),
            .f32_type => Type.initTag(.f32),
            .f64_type => Type.initTag(.f64),
            .f128_type => Type.initTag(.f128),
            .c_void_type => Type.initTag(.c_void),
            .bool_type => Type.initTag(.bool),
            .void_type => Type.initTag(.void),
            .type_type => Type.initTag(.type),
            .anyerror_type => Type.initTag(.anyerror),
            .comptime_int_type => Type.initTag(.comptime_int),
            .comptime_float_type => Type.initTag(.comptime_float),
            .noreturn_type => Type.initTag(.noreturn),
            .null_type => Type.initTag(.@"null"),
            .undefined_type => Type.initTag(.@"undefined"),
            .fn_noreturn_no_args_type => Type.initTag(.fn_noreturn_no_args),
            .fn_void_no_args_type => Type.initTag(.fn_void_no_args),
            .fn_naked_noreturn_no_args_type => Type.initTag(.fn_naked_noreturn_no_args),
            .fn_ccc_void_no_args_type => Type.initTag(.fn_ccc_void_no_args),
            .single_const_pointer_to_comptime_int_type => Type.initTag(.single_const_pointer_to_comptime_int),
            .const_slice_u8_type => Type.initTag(.const_slice_u8),
            .enum_literal_type => Type.initTag(.enum_literal),
            .anyframe_type => Type.initTag(.@"anyframe"),

            .int_type => {
                const payload = self.castTag(.int_type).?.data;
                const new = try allocator.create(Type.Payload.Bits);
                new.* = .{
                    .base = .{
                        .tag = if (payload.signed) .int_signed else .int_unsigned,
                    },
                    .data = payload.bits,
                };
                return Type.initPayload(&new.base);
            },
            .error_set => {
                const payload = self.castTag(.error_set).?.data;
                return Type.Tag.error_set.create(allocator, payload.decl);
            },

            .undef,
            .zero,
            .one,
            .void_value,
            .unreachable_value,
            .empty_array,
            .bool_true,
            .bool_false,
            .null_value,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .enum_literal,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,
        };
    }

    /// Asserts the value is an integer.
    pub fn toBigInt(self: Value, space: *BigIntSpace) BigIntConst {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .undef => unreachable,

            .zero,
            .bool_false,
            => return BigIntMutable.init(&space.limbs, 0).toConst(),

            .one,
            .bool_true,
            => return BigIntMutable.init(&space.limbs, 1).toConst(),

            .int_u64 => return BigIntMutable.init(&space.limbs, self.castTag(.int_u64).?.data).toConst(),
            .int_i64 => return BigIntMutable.init(&space.limbs, self.castTag(.int_i64).?.data).toConst(),
            .int_big_positive => return self.castTag(.int_big_positive).?.asBigInt(),
            .int_big_negative => return self.castTag(.int_big_negative).?.asBigInt(),
        }
    }

    /// Asserts the value is an integer and it fits in a u64
    pub fn toUnsignedInt(self: Value) u64 {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .undef => unreachable,

            .zero,
            .bool_false,
            => return 0,

            .one,
            .bool_true,
            => return 1,

            .int_u64 => return self.castTag(.int_u64).?.data,
            .int_i64 => return @intCast(u64, self.castTag(.int_i64).?.data),
            .int_big_positive => return self.castTag(.int_big_positive).?.asBigInt().to(u64) catch unreachable,
            .int_big_negative => return self.castTag(.int_big_negative).?.asBigInt().to(u64) catch unreachable,
        }
    }

    /// Asserts the value is an integer and it fits in a i64
    pub fn toSignedInt(self: Value) i64 {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .undef => unreachable,

            .zero,
            .bool_false,
            => return 0,

            .one,
            .bool_true,
            => return 1,

            .int_u64 => return @intCast(i64, self.castTag(.int_u64).?.data),
            .int_i64 => return self.castTag(.int_i64).?.data,
            .int_big_positive => return self.castTag(.int_big_positive).?.asBigInt().to(i64) catch unreachable,
            .int_big_negative => return self.castTag(.int_big_negative).?.asBigInt().to(i64) catch unreachable,
        }
    }

    pub fn toBool(self: Value) bool {
        return switch (self.tag()) {
            .bool_true => true,
            .bool_false, .zero => false,
            else => unreachable,
        };
    }

    /// Asserts that the value is a float or an integer.
    pub fn toFloat(self: Value, comptime T: type) T {
        return switch (self.tag()) {
            .float_16 => @panic("TODO soft float"),
            .float_32 => @floatCast(T, self.castTag(.float_32).?.data),
            .float_64 => @floatCast(T, self.castTag(.float_64).?.data),
            .float_128 => @floatCast(T, self.castTag(.float_128).?.data),

            .zero => 0,
            .one => 1,
            .int_u64 => @intToFloat(T, self.castTag(.int_u64).?.data),
            .int_i64 => @intToFloat(T, self.castTag(.int_i64).?.data),

            .int_big_positive, .int_big_negative => @panic("big int to f128"),
            else => unreachable,
        };
    }

    /// Asserts the value is an integer and not undefined.
    /// Returns the number of bits the value requires to represent stored in twos complement form.
    pub fn intBitCountTwosComp(self: Value) usize {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .undef,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .zero,
            .bool_false,
            => return 0,

            .one,
            .bool_true,
            => return 1,

            .int_u64 => {
                const x = self.castTag(.int_u64).?.data;
                if (x == 0) return 0;
                return @intCast(usize, std.math.log2(x) + 1);
            },
            .int_i64 => {
                @panic("TODO implement i64 intBitCountTwosComp");
            },
            .int_big_positive => return self.castTag(.int_big_positive).?.asBigInt().bitCountTwosComp(),
            .int_big_negative => return self.castTag(.int_big_negative).?.asBigInt().bitCountTwosComp(),
        }
    }

    /// Asserts the value is an integer, and the destination type is ComptimeInt or Int.
    pub fn intFitsInType(self: Value, ty: Type, target: Target) bool {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .zero,
            .undef,
            .bool_false,
            => return true,

            .one,
            .bool_true,
            => {
                const info = ty.intInfo(target);
                return switch (info.signedness) {
                    .signed => info.bits >= 2,
                    .unsigned => info.bits >= 1,
                };
            },

            .int_u64 => switch (ty.zigTypeTag()) {
                .Int => {
                    const x = self.castTag(.int_u64).?.data;
                    if (x == 0) return true;
                    const info = ty.intInfo(target);
                    const needed_bits = std.math.log2(x) + 1 + @boolToInt(info.signedness == .signed);
                    return info.bits >= needed_bits;
                },
                .ComptimeInt => return true,
                else => unreachable,
            },
            .int_i64 => switch (ty.zigTypeTag()) {
                .Int => {
                    const x = self.castTag(.int_i64).?.data;
                    if (x == 0) return true;
                    const info = ty.intInfo(target);
                    if (info.signedness == .unsigned and x < 0)
                        return false;
                    @panic("TODO implement i64 intFitsInType");
                },
                .ComptimeInt => return true,
                else => unreachable,
            },
            .int_big_positive => switch (ty.zigTypeTag()) {
                .Int => {
                    const info = ty.intInfo(target);
                    return self.castTag(.int_big_positive).?.asBigInt().fitsInTwosComp(info.signedness, info.bits);
                },
                .ComptimeInt => return true,
                else => unreachable,
            },
            .int_big_negative => switch (ty.zigTypeTag()) {
                .Int => {
                    const info = ty.intInfo(target);
                    return self.castTag(.int_big_negative).?.asBigInt().fitsInTwosComp(info.signedness, info.bits);
                },
                .ComptimeInt => return true,
                else => unreachable,
            },
        }
    }

    /// Converts an integer or a float to a float.
    /// Returns `error.Overflow` if the value does not fit in the new type.
    pub fn floatCast(self: Value, allocator: *Allocator, ty: Type, target: Target) !Value {
        switch (ty.tag()) {
            .f16 => {
                @panic("TODO add __trunctfhf2 to compiler-rt");
                //const res = try Value.Tag.float_16.create(allocator, self.toFloat(f16));
                //if (!self.eql(res))
                //    return error.Overflow;
                //return res;
            },
            .f32 => {
                const res = try Value.Tag.float_32.create(allocator, self.toFloat(f32));
                if (!self.eql(res))
                    return error.Overflow;
                return res;
            },
            .f64 => {
                const res = try Value.Tag.float_64.create(allocator, self.toFloat(f64));
                if (!self.eql(res))
                    return error.Overflow;
                return res;
            },
            .f128, .comptime_float, .c_longdouble => {
                return Value.Tag.float_128.create(allocator, self.toFloat(f128));
            },
            else => unreachable,
        }
    }

    /// Asserts the value is a float
    pub fn floatHasFraction(self: Value) bool {
        return switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .bool_true,
            .bool_false,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .undef,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .empty_array,
            .void_value,
            .unreachable_value,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .zero,
            .one,
            => false,

            .float_16 => @rem(self.castTag(.float_16).?.data, 1) != 0,
            .float_32 => @rem(self.castTag(.float_32).?.data, 1) != 0,
            .float_64 => @rem(self.castTag(.float_64).?.data, 1) != 0,
            // .float_128 => @rem(self.castTag(.float_128).?.data, 1) != 0,
            .float_128 => @panic("TODO lld: error: undefined symbol: fmodl"),
        };
    }

    pub fn orderAgainstZero(lhs: Value) std.math.Order {
        return switch (lhs.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .undef,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .zero,
            .bool_false,
            => .eq,

            .one,
            .bool_true,
            => .gt,

            .int_u64 => std.math.order(lhs.castTag(.int_u64).?.data, 0),
            .int_i64 => std.math.order(lhs.castTag(.int_i64).?.data, 0),
            .int_big_positive => lhs.castTag(.int_big_positive).?.asBigInt().orderAgainstScalar(0),
            .int_big_negative => lhs.castTag(.int_big_negative).?.asBigInt().orderAgainstScalar(0),

            .float_16 => std.math.order(lhs.castTag(.float_16).?.data, 0),
            .float_32 => std.math.order(lhs.castTag(.float_32).?.data, 0),
            .float_64 => std.math.order(lhs.castTag(.float_64).?.data, 0),
            .float_128 => std.math.order(lhs.castTag(.float_128).?.data, 0),
        };
    }

    /// Asserts the value is comparable.
    pub fn order(lhs: Value, rhs: Value) std.math.Order {
        const lhs_tag = lhs.tag();
        const rhs_tag = rhs.tag();
        const lhs_is_zero = lhs_tag == .zero;
        const rhs_is_zero = rhs_tag == .zero;
        if (lhs_is_zero) return rhs.orderAgainstZero().invert();
        if (rhs_is_zero) return lhs.orderAgainstZero();

        const lhs_float = lhs.isFloat();
        const rhs_float = rhs.isFloat();
        if (lhs_float and rhs_float) {
            if (lhs_tag == rhs_tag) {
                return switch (lhs.tag()) {
                    .float_16 => return std.math.order(lhs.castTag(.float_16).?.data, rhs.castTag(.float_16).?.data),
                    .float_32 => return std.math.order(lhs.castTag(.float_32).?.data, rhs.castTag(.float_32).?.data),
                    .float_64 => return std.math.order(lhs.castTag(.float_64).?.data, rhs.castTag(.float_64).?.data),
                    .float_128 => return std.math.order(lhs.castTag(.float_128).?.data, rhs.castTag(.float_128).?.data),
                    else => unreachable,
                };
            }
        }
        if (lhs_float or rhs_float) {
            const lhs_f128 = lhs.toFloat(f128);
            const rhs_f128 = rhs.toFloat(f128);
            return std.math.order(lhs_f128, rhs_f128);
        }

        var lhs_bigint_space: BigIntSpace = undefined;
        var rhs_bigint_space: BigIntSpace = undefined;
        const lhs_bigint = lhs.toBigInt(&lhs_bigint_space);
        const rhs_bigint = rhs.toBigInt(&rhs_bigint_space);
        return lhs_bigint.order(rhs_bigint);
    }

    /// Asserts the value is comparable.
    pub fn compare(lhs: Value, op: std.math.CompareOperator, rhs: Value) bool {
        return order(lhs, rhs).compare(op);
    }

    /// Asserts the value is comparable.
    pub fn compareWithZero(lhs: Value, op: std.math.CompareOperator) bool {
        return orderAgainstZero(lhs).compare(op);
    }

    pub fn eql(a: Value, b: Value) bool {
        if (a.tag() == b.tag()) {
            if (a.tag() == .void_value or a.tag() == .null_value) {
                return true;
            } else if (a.tag() == .enum_literal) {
                const a_name = a.castTag(.enum_literal).?.data;
                const b_name = b.castTag(.enum_literal).?.data;
                return std.mem.eql(u8, a_name, b_name);
            }
        }
        if (a.isType() and b.isType()) {
            // 128 bytes should be enough to hold both types
            var buf: [128]u8 = undefined;
            var fib = std.heap.FixedBufferAllocator.init(&buf);
            const a_type = a.toType(&fib.allocator) catch unreachable;
            const b_type = b.toType(&fib.allocator) catch unreachable;
            return a_type.eql(b_type);
        }
        return compare(a, .eq, b);
    }

    pub fn hash(self: Value) u64 {
        var hasher = std.hash.Wyhash.init(0);

        switch (self.tag()) {
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .ty,
            => {
                // Directly return Type.hash, toType can only fail for .int_type and .error_set.
                var allocator = std.heap.FixedBufferAllocator.init(&[_]u8{});
                return (self.toType(&allocator.allocator) catch unreachable).hash();
            },
            .error_set => {
                // Payload.decl should be same for all instances of the type.
                const payload = self.castTag(.error_set).?.data;
                std.hash.autoHash(&hasher, payload.decl);
            },
            .int_type => {
                const payload = self.castTag(.int_type).?.data;
                var int_payload = Type.Payload.Bits{
                    .base = .{
                        .tag = if (payload.signed) .int_signed else .int_unsigned,
                    },
                    .data = payload.bits,
                };
                return Type.initPayload(&int_payload.base).hash();
            },

            .empty_struct_value,
            .empty_array,
            => {},

            .undef,
            .null_value,
            .void_value,
            .unreachable_value,
            => std.hash.autoHash(&hasher, self.tag()),

            .zero, .bool_false => std.hash.autoHash(&hasher, @as(u64, 0)),
            .one, .bool_true => std.hash.autoHash(&hasher, @as(u64, 1)),

            .float_16, .float_32, .float_64, .float_128 => {},
            .enum_literal => {
                const payload = self.castTag(.enum_literal).?;
                hasher.update(payload.data);
            },
            .bytes => {
                const payload = self.castTag(.bytes).?;
                hasher.update(payload.data);
            },
            .int_u64 => {
                const payload = self.castTag(.int_u64).?;
                std.hash.autoHash(&hasher, payload.data);
            },
            .int_i64 => {
                const payload = self.castTag(.int_i64).?;
                std.hash.autoHash(&hasher, payload.data);
            },
            .repeated => {
                const payload = self.castTag(.repeated).?;
                std.hash.autoHash(&hasher, payload.data.hash());
            },
            .ref_val => {
                const payload = self.castTag(.ref_val).?;
                std.hash.autoHash(&hasher, payload.data.hash());
            },
            .int_big_positive, .int_big_negative => {
                var space: BigIntSpace = undefined;
                const big = self.toBigInt(&space);
                if (big.limbs.len == 1) {
                    // handle like {u,i}64 to ensure same hash as with Int{i,u}64
                    if (big.positive) {
                        std.hash.autoHash(&hasher, @as(u64, big.limbs[0]));
                    } else {
                        std.hash.autoHash(&hasher, @as(u64, @bitCast(usize, -@bitCast(isize, big.limbs[0]))));
                    }
                } else {
                    std.hash.autoHash(&hasher, big.positive);
                    for (big.limbs) |limb| {
                        std.hash.autoHash(&hasher, limb);
                    }
                }
            },
            .elem_ptr => {
                const payload = self.castTag(.elem_ptr).?.data;
                std.hash.autoHash(&hasher, payload.array_ptr.hash());
                std.hash.autoHash(&hasher, payload.index);
            },
            .decl_ref => {
                const decl = self.castTag(.decl_ref).?.data;
                std.hash.autoHash(&hasher, decl);
            },
            .function => {
                const func = self.castTag(.function).?.data;
                std.hash.autoHash(&hasher, func);
            },
            .extern_fn => {
                const decl = self.castTag(.extern_fn).?.data;
                std.hash.autoHash(&hasher, decl);
            },
            .variable => {
                const variable = self.castTag(.variable).?.data;
                std.hash.autoHash(&hasher, variable);
            },
            .@"error" => {
                const payload = self.castTag(.@"error").?.data;
                hasher.update(payload.name);
                std.hash.autoHash(&hasher, payload.value);
            },

            .inferred_alloc => unreachable,
        }
        return hasher.final();
    }

    /// Asserts the value is a pointer and dereferences it.
    /// Returns error.AnalysisFail if the pointer points to a Decl that failed semantic analysis.
    pub fn pointerDeref(self: Value, allocator: *Allocator) error{ AnalysisFail, OutOfMemory }!Value {
        return switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .zero,
            .one,
            .bool_true,
            .bool_false,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .bytes,
            .undef,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .empty_array,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .ref_val => self.castTag(.ref_val).?.data,
            .decl_ref => self.castTag(.decl_ref).?.data.value(),
            .elem_ptr => {
                const elem_ptr = self.castTag(.elem_ptr).?.data;
                const array_val = try elem_ptr.array_ptr.pointerDeref(allocator);
                return array_val.elemValue(allocator, elem_ptr.index);
            },
        };
    }

    /// Asserts the value is a single-item pointer to an array, or an array,
    /// or an unknown-length pointer, and returns the element value at the index.
    pub fn elemValue(self: Value, allocator: *Allocator, index: usize) error{OutOfMemory}!Value {
        switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .zero,
            .one,
            .bool_true,
            .bool_false,
            .null_value,
            .function,
            .extern_fn,
            .variable,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .undef,
            .elem_ptr,
            .ref_val,
            .decl_ref,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .unreachable_value,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            .inferred_alloc,
            => unreachable,

            .empty_array => unreachable, // out of bounds array index

            .bytes => return Tag.int_u64.create(allocator, self.castTag(.bytes).?.data[index]),

            // No matter the index; all the elements are the same!
            .repeated => return self.castTag(.repeated).?.data,
        }
    }

    /// Returns a pointer to the element value at the index.
    pub fn elemPtr(self: Value, allocator: *Allocator, index: usize) !Value {
        if (self.castTag(.elem_ptr)) |elem_ptr| {
            return Tag.elem_ptr.create(allocator, .{
                .array_ptr = elem_ptr.data.array_ptr,
                .index = elem_ptr.data.index + index,
            });
        }

        return Tag.elem_ptr.create(allocator, .{
            .array_ptr = self,
            .index = index,
        });
    }

    pub fn isUndef(self: Value) bool {
        return self.tag() == .undef;
    }

    /// Valid for all types. Asserts the value is not undefined and not unreachable.
    pub fn isNull(self: Value) bool {
        return switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .zero,
            .one,
            .empty_array,
            .bool_true,
            .bool_false,
            .function,
            .extern_fn,
            .variable,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .enum_literal,
            .error_set,
            .@"error",
            .empty_struct_value,
            => false,

            .undef => unreachable,
            .unreachable_value => unreachable,
            .inferred_alloc => unreachable,
            .null_value => true,
        };
    }

    /// Valid for all types. Asserts the value is not undefined.
    pub fn isFloat(self: Value) bool {
        return switch (self.tag()) {
            .undef => unreachable,
            .inferred_alloc => unreachable,

            .float_16,
            .float_32,
            .float_64,
            .float_128,
            => true,
            else => false,
        };
    }

    /// Valid for all types. Asserts the value is not undefined.
    pub fn isType(self: Value) bool {
        return switch (self.tag()) {
            .ty,
            .int_type,
            .u8_type,
            .i8_type,
            .u16_type,
            .i16_type,
            .u32_type,
            .i32_type,
            .u64_type,
            .i64_type,
            .usize_type,
            .isize_type,
            .c_short_type,
            .c_ushort_type,
            .c_int_type,
            .c_uint_type,
            .c_long_type,
            .c_ulong_type,
            .c_longlong_type,
            .c_ulonglong_type,
            .c_longdouble_type,
            .f16_type,
            .f32_type,
            .f64_type,
            .f128_type,
            .c_void_type,
            .bool_type,
            .void_type,
            .type_type,
            .anyerror_type,
            .comptime_int_type,
            .comptime_float_type,
            .noreturn_type,
            .null_type,
            .undefined_type,
            .fn_noreturn_no_args_type,
            .fn_void_no_args_type,
            .fn_naked_noreturn_no_args_type,
            .fn_ccc_void_no_args_type,
            .single_const_pointer_to_comptime_int_type,
            .const_slice_u8_type,
            .enum_literal_type,
            .anyframe_type,
            .error_set,
            => true,

            .zero,
            .one,
            .empty_array,
            .bool_true,
            .bool_false,
            .function,
            .extern_fn,
            .variable,
            .int_u64,
            .int_i64,
            .int_big_positive,
            .int_big_negative,
            .ref_val,
            .decl_ref,
            .elem_ptr,
            .bytes,
            .repeated,
            .float_16,
            .float_32,
            .float_64,
            .float_128,
            .void_value,
            .enum_literal,
            .@"error",
            .empty_struct_value,
            .null_value,
            => false,

            .undef => unreachable,
            .unreachable_value => unreachable,
            .inferred_alloc => unreachable,
        };
    }

    /// This type is not copyable since it may contain pointers to its inner data.
    pub const Payload = struct {
        tag: Tag,

        pub const U64 = struct {
            base: Payload,
            data: u64,
        };

        pub const I64 = struct {
            base: Payload,
            data: i64,
        };

        pub const BigInt = struct {
            base: Payload,
            data: []const std.math.big.Limb,

            pub fn asBigInt(self: BigInt) BigIntConst {
                const positive = switch (self.base.tag) {
                    .int_big_positive => true,
                    .int_big_negative => false,
                    else => unreachable,
                };
                return BigIntConst{ .limbs = self.data, .positive = positive };
            }
        };

        pub const Function = struct {
            base: Payload,
            data: *Module.Fn,
        };

        pub const Decl = struct {
            base: Payload,
            data: *Module.Decl,
        };

        pub const Variable = struct {
            base: Payload,
            data: *Module.Var,
        };

        pub const SubValue = struct {
            base: Payload,
            data: Value,
        };

        pub const ElemPtr = struct {
            pub const base_tag = Tag.elem_ptr;

            base: Payload = Payload{ .tag = base_tag },
            data: struct {
                array_ptr: Value,
                index: usize,
            },
        };

        pub const Bytes = struct {
            base: Payload,
            data: []const u8,
        };

        pub const Ty = struct {
            base: Payload,
            data: Type,
        };

        pub const IntType = struct {
            pub const base_tag = Tag.int_type;

            base: Payload = Payload{ .tag = base_tag },
            data: struct {
                bits: u16,
                signed: bool,
            },
        };

        pub const Float_16 = struct {
            pub const base_tag = Tag.float_16;

            base: Payload = .{ .tag = base_tag },
            data: f16,
        };

        pub const Float_32 = struct {
            pub const base_tag = Tag.float_32;

            base: Payload = .{ .tag = base_tag },
            data: f32,
        };

        pub const Float_64 = struct {
            pub const base_tag = Tag.float_64;

            base: Payload = .{ .tag = base_tag },
            data: f64,
        };

        pub const Float_128 = struct {
            pub const base_tag = Tag.float_128;

            base: Payload = .{ .tag = base_tag },
            data: f128,
        };

        // TODO move to type.zig
        pub const ErrorSet = struct {
            pub const base_tag = Tag.error_set;

            base: Payload = .{ .tag = base_tag },
            data: struct {
                // TODO revisit this when we have the concept of the error tag type
                fields: std.StringHashMapUnmanaged(u16),
                decl: *Module.Decl,
            },
        };

        pub const Error = struct {
            base: Payload = .{ .tag = .@"error" },
            data: struct {
                // TODO revisit this when we have the concept of the error tag type
                /// `name` is owned by `Module` and will be valid for the entire
                /// duration of the compilation.
                name: []const u8,
                value: u16,
            },
        };

        pub const InferredAlloc = struct {
            pub const base_tag = Tag.inferred_alloc;

            base: Payload = .{ .tag = base_tag },
            data: struct {
                /// The value stored in the inferred allocation. This will go into
                /// peer type resolution. This is stored in a separate list so that
                /// the items are contiguous in memory and thus can be passed to
                /// `Module.resolvePeerTypes`.
                stored_inst_list: std.ArrayListUnmanaged(*ir.Inst) = .{},
            },
        };
    };

    /// Big enough to fit any non-BigInt value
    pub const BigIntSpace = struct {
        /// The +1 is headroom so that operations such as incrementing once or decrementing once
        /// are possible without using an allocator.
        limbs: [(@sizeOf(u64) / @sizeOf(std.math.big.Limb)) + 1]std.math.big.Limb,
    };
};

test "hash same value different representation" {
    const zero_1 = Value.initTag(.zero);
    var payload_1 = Value.Payload.U64{
        .base = .{ .tag = .int_u64 },
        .data = 0,
    };
    const zero_2 = Value.initPayload(&payload_1.base);
    std.testing.expectEqual(zero_1.hash(), zero_2.hash());

    var payload_2 = Value.Payload.I64{
        .base = .{ .tag = .int_i64 },
        .data = 0,
    };
    const zero_3 = Value.initPayload(&payload_2.base);
    std.testing.expectEqual(zero_2.hash(), zero_3.hash());

    var payload_3 = Value.Payload.BigInt{
        .base = .{ .tag = .int_big_negative },
        .data = &[_]std.math.big.Limb{0},
    };
    const zero_4 = Value.initPayload(&payload_3.base);
    std.testing.expectEqual(zero_3.hash(), zero_4.hash());
}
