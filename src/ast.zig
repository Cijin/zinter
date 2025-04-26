const std = @import("std");
const mem = std.mem;

const token = @import("token.zig");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
    program: Program,

    fn token_literal(self: Node) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(),
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,

    pub fn token_literal(self: Statement) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(),
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    prefix_expression: *PrefixExpression,
    nil_expression: NilExpression,

    pub fn token_literal(self: Expression, allocator: mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(allocator),
        }
    }
};

pub const Program = struct {
    statements: []const Statement,

    pub fn token_literal(self: Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal();
        }

        return "";
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    pub fn token_literal(self: LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    return_value: Expression,

    pub fn token_literal(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn token_literal(self: Identifier, _: mem.Allocator) []const u8 {
        return self.token.literal;
    }
};

pub const Integer = struct {
    token: token.Token,
    value: i64,

    pub fn token_literal(self: Integer, _: mem.Allocator) []const u8 {
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: Expression,

    pub fn token_literal(self: PrefixExpression, allocator: mem.Allocator) []const u8 {
        // Todo: handle or return error
        const prefix_expression_literal = std.fmt.allocPrint(allocator, "{s}{s}", .{ self.operator, self.right.token_literal(allocator) }) catch unreachable;
        return prefix_expression_literal;
    }
};

pub const NilExpression = struct {
    token: token.Token,

    pub fn token_literal(_: NilExpression, _: mem.Allocator) []const u8 {
        return "you messed up, son";
    }
};
