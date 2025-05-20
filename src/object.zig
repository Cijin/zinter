const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;

const INT = "integer";
const BOOL = "boolean";
const NULL = "null";

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null: Null,

    fn typ(self: Object) []const u8 {
        switch (self) {
            inline else => |impl| return impl.type(),
        }
    }

    fn inspect(self: Object, allocator: mem.Allocator) []const u8 {
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

    fn inspect(self: Integer, allocator: mem.Allocator) []const u8 {
        return fmt.allocPrint(allocator, "{d}", self.value);
    }
};

pub const Boolean = struct {
    value: bool,

    fn typ(_: Integer) []const u8 {
        return BOOL;
    }

    fn inspect(self: Integer, allocator: mem.Allocator) []const u8 {
        return fmt.allocPrint(allocator, "{}", self.value);
    }
};

pub const Null = struct {
    fn typ(_: Integer) []const u8 {
        return NULL;
    }

    fn inspect(_: Integer, _: mem.Allocator) []const u8 {
        return NULL;
    }
};
