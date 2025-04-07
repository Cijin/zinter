const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;

const token = @import("token.zig");

const Node = struct {
    ptr: *anyopaque,
    token_literal_fn: fn (ptr: *anyopaque, alloc: mem.Allocator) fmt.AllocPrintError![]const u8,

    fn token_literal(self: Node, alloc: mem.Allocator) fmt.AllocPrintError![]const u8 {
        return self.token_literal_fn(self.ptr, alloc);
    }
};

const Program = struct {
    statements: []const Statement,

    fn token_literal(ptr: *anyopaque, _: mem.Allocator) fmt.AllocPrintError![]const u8 {
        const self: *Program = @alignCast(@ptrCast(ptr));
        if (self.satements.len > 0) {
            return self.statements[0].token_literal();
        }

        return "";
    }

    fn node(self: *Program) Node {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const Statement = struct {
    fn token_literal(ptr: *anyopaque, alloc: mem.Allocator) fmt.AllocPrintError![]const u8 {
        const self: *Statement = @alignCast(@ptrCast(ptr));
        // Todo: implement this, not sure yet
        _ = self;
        _ = alloc;
    }

    fn node(self: *Statement) Node {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const Expression = struct {
    // Todo: this might need more than token, not sure yet
    token: token.Token,

    fn token_literal(ptr: *anyopaque, _: mem.Allocator) fmt.AllocPrintError![]const u8 {
        const self: *Expression = @alignCast(@ptrCast(ptr));
        return self.token.Literal;
    }

    fn node(self: *Expression) Node {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const Identifier = struct {
    token: token.Token,

    fn token_literal(ptr: *anyopaque, _: mem.Allocator) fmt.AllocPrintError![]const u8 {
        const self: *Identifier = @alignCast(@ptrCast(ptr));
        return self.token.literal;
    }

    fn expression(self: *Identifier) Expression {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    value: Expression,

    fn token_literal(ptr: *anyopaque, alloc: mem.Allocator) fmt.AllocPrintError![]const u8 {
        const self: *Identifier = @alignCast(@ptrCast(ptr));
        return fmt.allocPrint(alloc, "{s} {s} = {s}", .{
            self.token.literal,
            self.name.token_literal(ptr, alloc),
            self.value.token_literal(ptr, alloc),
        });
    }

    fn statement(self: *LetStatement) Statement {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};
