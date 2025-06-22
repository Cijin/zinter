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
    expression_statement: ExpressionStatement,

    pub fn token_literal(self: Statement, allocator: mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.token_literal(allocator),
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    string: String,
    boolean: Boolean,
    array_literal: *ArrayLiteral,
    index_expression: *IndexExpression,
    prefix_expression: *PrefixExpression,
    infix_expression: *InfixExpression,
    if_expression: *IfExpression,
    fn_literal: FnLiteral,
    call_expression: *CallExpression,
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
        var program_literal: []u8 = "";
        for (self.statements) |stmt| {
            program_literal = std.fmt.allocPrint(allocator, "{s}{s}\n", .{ program_literal, stmt.token_literal(allocator) }) catch unreachable;
        }

        return program_literal;
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    pub fn token_literal(self: LetStatement, allocator: mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s} {s} = {s}", .{
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

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: Expression,

    pub fn token_literal(self: ExpressionStatement, allocator: mem.Allocator) []const u8 {
        return self.expression.token_literal(allocator);
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

pub const String = struct {
    token: token.Token,
    value: []const u8,

    pub fn token_literal(self: String, _: mem.Allocator) []const u8 {
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

pub const ArrayLiteral = struct {
    token: token.Token,
    elements: []Expression,

    pub fn token_literal(self: ArrayLiteral, allocator: mem.Allocator) []const u8 {
        var array_literal = std.fmt.allocPrint(allocator, "{s}", .{
            self.token.literal,
        }) catch unreachable;

        for (self.elements, 0..) |e, i| {
            if (i == self.elements.len - 1) {
                array_literal = std.fmt.allocPrint(allocator, "{s}{s}]", .{ array_literal, e.token_literal(allocator) }) catch unreachable;
                break;
            }
            array_literal = std.fmt.allocPrint(allocator, "{s}{s},", .{ array_literal, e.token_literal(allocator) }) catch unreachable;
        }

        return array_literal;
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

pub const FnLiteral = struct {
    token: token.Token,
    parameters: []Identifier,
    body: BlockStatement,

    pub fn token_literal(self: FnLiteral, allocator: mem.Allocator) []const u8 {
        var fn_literal = std.fmt.allocPrint(allocator, "{s}(", .{self.token.literal}) catch unreachable;
        for (self.parameters, 0..) |p, i| {
            if (i + 1 == self.parameters.len) {
                fn_literal = std.fmt.allocPrint(allocator, "{s}{s}", .{ fn_literal, p.token_literal(allocator) }) catch unreachable;
                continue;
            }
            fn_literal = std.fmt.allocPrint(allocator, "{s}{s}, ", .{ fn_literal, p.token_literal(allocator) }) catch unreachable;
        }
        fn_literal = std.fmt.allocPrint(allocator, "{s}){{{s}}}", .{ fn_literal, self.body.token_literal(allocator) }) catch unreachable;

        return fn_literal;
    }
};

pub const CallExpression = struct {
    token: token.Token,
    function: Identifier,
    arguments: []Expression,

    pub fn token_literal(self: CallExpression, allocator: mem.Allocator) []const u8 {
        var call_expression = std.fmt.allocPrint(allocator, "{s}{s}", .{ self.function.token_literal(allocator), self.token.literal }) catch unreachable;
        for (self.arguments, 0..) |e, i| {
            if (i + 1 == self.arguments.len) {
                call_expression = std.fmt.allocPrint(allocator, "{s}{s})", .{ call_expression, e.token_literal(allocator) }) catch unreachable;
                continue;
            }
            call_expression = std.fmt.allocPrint(allocator, "{s}{s},", .{ call_expression, e.token_literal(allocator) }) catch unreachable;
        }

        return call_expression;
    }
};

pub const IndexExpression = struct {
    token: token.Token,
    array: Expression,
    index: Expression,

    pub fn token_literal(self: IndexExpression, allocator: mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}[{s}]", .{ self.array.token_literal(allocator), self.index.token_literal(allocator) }) catch unreachable;
    }
};

pub const NilExpression = struct {
    token: token.Token,

    pub fn token_literal(_: NilExpression, _: mem.Allocator) []const u8 {
        return "you messed up, son";
    }
};
