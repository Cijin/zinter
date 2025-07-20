const std = @import("std");
const fmt = std.fmt;
const assert = std.debug.assert;
const mem = std.mem;

pub const INT = "integer";
pub const ARRAY = "array";
pub const FN_INSTR = "fn_instructions";
pub const STRING = "string";
pub const BOOL = "boolean";
pub const NULL = "null";
pub const NULL_OBJ = Object{ .null = .{} };
pub const TRUE = Object{ .boolean = .{ .value = true } };
pub const FALSE = Object{ .boolean = .{ .value = false } };

const ObjectError = error{
    InvalidOperation,
};

pub const Object = union(enum) {
    integer: Integer,
    fn_instrs: FnInstrs,
    string: String,
    boolean: Boolean,
    array: *Array,
    null: Null,

    pub fn typ(self: Object) []const u8 {
        switch (self) {
            inline else => |impl| return impl.typ(),
        }
    }

    pub fn not_equal(self: Object, compared_to: Object) bool {
        switch (self) {
            inline else => |impl| {
                assert(mem.eql(u8, self.typ(), compared_to.typ()));

                return !impl.equal(compared_to);
            },
        }
    }

    pub fn equal(self: Object, compared_to: Object) bool {
        switch (self) {
            inline else => |impl| {
                if (!mem.eql(u8, self.typ(), compared_to.typ())) {
                    return false;
                }

                return impl.equal(compared_to);
            },
        }
    }

    pub fn inspect(self: Object, allocator: mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.inspect(allocator),
        }
    }
};

pub const Integer = struct {
    value: i64,

    fn typ(_: Integer) []const u8 {
        return INT;
    }

    fn equal(self: Integer, compared_to: Object) bool {
        return self.value == compared_to.integer.value;
    }

    fn inspect(self: Integer, allocator: mem.Allocator) []const u8 {
        return fmt.allocPrint(allocator, "{d}", .{self.value}) catch unreachable;
    }

    fn add(self: Integer, to: Integer) ObjectError!Object {
        return Object{ .integer = .{ .value = self.value + to.value } };
    }
};

pub const FnInstrs = struct {
    value: []const u8,
    symbol_count: u32,

    fn typ(_: FnInstrs) []const u8 {
        return FN_INSTR;
    }

    fn equal(self: FnInstrs, compared_to: Object) bool {
        if (self.value.ptr == compared_to.fn_instrs.value.ptr and self.value.len == compared_to.fn_instrs.value.len) {
            return true;
        }

        if (self.value.len != compared_to.fn_instrs.value.len) {
            return false;
        }

        for (self.value, 0..) |instr, i| {
            if (instr != compared_to.fn_instrs.value[i]) {
                return false;
            }
        }

        return true;
    }

    fn inspect(self: FnInstrs, allocator: mem.Allocator) []const u8 {
        if (self.value.len == 0) {
            return "[]";
        }

        var fn_instrs: []const u8 = undefined;
        for (self.value, 0..) |instr, i| {
            if (i == 0) {
                fn_instrs = std.fmt.allocPrint(allocator, "[{d}", .{instr}) catch unreachable;
                continue;
            }

            fn_instrs = std.fmt.allocPrint(allocator, ", {d}", .{instr}) catch unreachable;
        }

        fn_instrs = std.fmt.allocPrint(allocator, "{s}]", .{fn_instrs}) catch unreachable;
        return fn_instrs;
    }

    fn add(_: FnInstrs, _: Integer) ObjectError!Object {
        return ObjectError.InvalidOperation;
    }
};

pub const Array = struct {
    value: []Object,

    fn typ(_: Array) []const u8 {
        return ARRAY;
    }

    fn equal(self: Array, compared_to: Object) bool {
        if (self.value.ptr == compared_to.array.value.ptr and self.value.len == compared_to.array.value.len) {
            return true;
        }

        if (self.value.len != compared_to.array.value.len) {
            return false;
        }

        for (self.value, 0..) |a, i| {
            if (!a.equal(compared_to.array.value[i])) {
                return false;
            }
        }
        return true;
    }

    fn inspect(self: Array, allocator: mem.Allocator) []const u8 {
        if (self.value.len == 0) {
            return "[]";
        }

        var aol: []const u8 = undefined;
        for (self.value, 0..) |a, i| {
            if (i == 0) {
                aol = std.fmt.allocPrint(allocator, "[{s}", .{a.inspect(allocator)}) catch unreachable;
                continue;
            }

            aol = std.fmt.allocPrint(allocator, ", {s}", .{a.inspect(allocator)}) catch unreachable;
        }

        aol = std.fmt.allocPrint(allocator, "{s}]", .{aol}) catch unreachable;
        return aol;
    }

    fn add(_: Array, _: Array) ObjectError!Object {
        return ObjectError.InvalidOperation;
    }
};

pub const String = struct {
    value: []const u8,

    fn typ(_: String) []const u8 {
        return STRING;
    }

    fn equal(self: String, compared_to: Object) bool {
        return mem.eql(u8, self.value, compared_to.string.value);
    }

    fn inspect(self: String, _: mem.Allocator) []const u8 {
        return self.value;
    }

    fn add(self: String, to: String) ObjectError!Object {
        return Object{ .string = .{ .value = self.value + to.value } };
    }
};

pub const Boolean = struct {
    value: bool,

    fn typ(_: Boolean) []const u8 {
        return BOOL;
    }

    fn equal(self: Boolean, compared_to: Object) bool {
        return self.value == compared_to.boolean.value;
    }

    fn inspect(self: Boolean, allocator: mem.Allocator) []const u8 {
        return fmt.allocPrint(allocator, "{}", .{self.value}) catch unreachable;
    }

    fn add(_: Boolean, _: Boolean) ObjectError!Object {
        return ObjectError.InvalidOperation;
    }
};

pub const Null = struct {
    fn typ(_: Null) []const u8 {
        return NULL;
    }

    fn equal(_: Null, _: Object) bool {
        return true;
    }

    fn inspect(_: Null, _: mem.Allocator) []const u8 {
        return NULL;
    }

    fn add(_: Null, _: Null) ObjectError!Object {
        return ObjectError.InvalidOperation;
    }
};
