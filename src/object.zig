const std = @import("std");
const fmt = std.fmt;
const assert = std.debug.assert;
const mem = std.mem;

pub const INT = "integer";
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
    string: String,
    boolean: Boolean,
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
                assert(mem.eql(u8, self.typ(), compared_to.typ()));

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

pub const String = struct {
    value: []const u8,

    fn typ(_: String) []const u8 {
        return INT;
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
