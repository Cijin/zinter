const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;

const token = @import("token.zig");

const Node = union(enum) {
    statement: Statement,
    expression: Expression,
    program: Program,

    fn token_literal(self: Node, alloc: mem.Allocator) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(alloc),
        }
    }
};

const Statement = union(enum) {
    let_statement: LetStatement,

    fn token_literal(self: Statement, alloc: mem.Allocator) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(alloc),
        }
    }
};

const Expression = union(enum) {
    identifier: Identifier,

    fn token_literal(self: Expression, alloc: mem.Allocator) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(alloc),
        }
    }
};

const Program = struct {
    statements: []const Statement,

    pub fn token_literal(self: Program, _: mem.Allocator) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal();
        }

        return "";
    }
};

const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    fn token_literal(self: LetStatement, alloc: mem.Allocator) []const u8 {
        // Todo: not sure about this one
        fmt.allocPrint(alloc, "{s} {s} = {s};", .{ self.token.literal, self.Identifier.token.literal, self.value.token_literal(alloc) });
    }
};

const Identifier = struct {
    token: token.Token,
    value: []const u8,

    fn token_literal(self: Identifier, _: mem.Allocator) []const u8 {
        return self.token.literal;
    }
};
