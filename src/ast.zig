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
    block_statement: BlockStatement,

    pub fn token_literal(self: Statement, allocator: mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(allocator),
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    boolean: Boolean,
    prefix_expression: *PrefixExpression,
    infix_expression: *InfixExpression,
    if_expression: *IfExpression,
    nil_expression: NilExpression,

    pub fn token_literal(self: Expression, allocator: mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(allocator),
        }
    }
};

pub const Program = struct {
    statements: []const Statement,

    pub fn token_literal(self: Program, allocator: mem.Allocator) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal(allocator);
        }

        return "";
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    pub fn token_literal(self: LetStatement, allocator: mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s} {s} {s}", .{
            self.token.literal,
            self.name.token_literal(allocator),
            self.value.token_literal(allocator),
        }) catch unreachable;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    return_value: Expression,

    pub fn token_literal(self: ReturnStatement, allocator: mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s} {s}", .{
            self.token.literal,
            self.return_value.token_literal(allocator),
        }) catch unreachable;
    }
};

pub const BlockStatement = struct {
    token: token.Token,
    statements: []Statement,

    pub fn token_literal(self: BlockStatement, allocator: mem.Allocator) []const u8 {
        var block_statement_literal: []u8 = "";
        for (self.statements) |stmt| {
            // Todo: print multiple lines prettier? \n maybe
            block_statement_literal = std.fmt.allocPrint(allocator, "{s}{s}", .{ block_statement_literal, stmt.token_literal(allocator) }) catch unreachable;
        }

        return block_statement_literal;
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

pub const Boolean = struct {
    token: token.Token,
    value: bool,

    pub fn token_literal(self: Boolean, _: mem.Allocator) []const u8 {
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: Expression,

    pub fn token_literal(self: PrefixExpression, allocator: mem.Allocator) []const u8 {
        const prefix_expression_literal = std.fmt.allocPrint(allocator, "{s}{s}", .{
            self.operator,
            self.right.token_literal(allocator),
        }) catch unreachable;
        return prefix_expression_literal;
    }
};

pub const InfixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: Expression,
    left: Expression,

    pub fn token_literal(self: InfixExpression, allocator: mem.Allocator) []const u8 {
        const infix_expression_literal = std.fmt.allocPrint(allocator, "{s}{s}{s}", .{
            self.left.token_literal(allocator),
            self.operator,
            self.right.token_literal(allocator),
        }) catch unreachable;
        return infix_expression_literal;
    }
};

pub const IfExpression = struct {
    token: token.Token,
    condition: Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,

    pub fn token_literal(self: IfExpression, allocator: mem.Allocator) []const u8 {
        var if_expression_literal = std.fmt.allocPrint(allocator, "{s}({s}){{{s}}}", .{
            self.token.literal,
            self.condition.token_literal(allocator),
            self.consequence.token_literal(allocator),
        }) catch unreachable;
        if (self.alternative) |stmt| {
            if_expression_literal = std.fmt.allocPrint(allocator, "{s}else{{{s}}}", .{
                if_expression_literal,
                stmt.token_literal(allocator),
            }) catch unreachable;
        }
        return if_expression_literal;
    }
};

pub const NilExpression = struct {
    token: token.Token,

    pub fn token_literal(_: NilExpression, _: mem.Allocator) []const u8 {
        return "you messed up, son";
    }
};
