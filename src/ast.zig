const std = @import("std");
const mem = std.mem;

const token = @import("token.zig");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
    program: Program,

    fn token_literal(self: Node) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(),
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,

    fn token_literal(self: Statement) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(),
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,

    fn token_literal(self: Expression) []const u8 {
        switch (self) {
            null => unreachable,
            inline else => |impl| return impl.token_literal(),
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

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn token_literal(self: Identifier) []const u8 {
        return self.token.literal;
    }
};
