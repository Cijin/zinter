const token = @import("token.zig");

// statement
// expression
//
// both the above are also interfaces that implement node
// other elements can be
//
// type program is a type of node
//
// statement: let x = 5;
// program []statement:
// let x = 5;
// let y = 6;
// let add = fn(a, b) {
//  return a + b;
// };
//
// expressions produce values | statements don't
const Node = struct {
    ptr: *anyopaque,
    token_literal_fn: fn (ptr: *anyopaque) []const u8,

    fn token_literal(self: Node) []const u8 {
        return self.token_literal_fn(self.ptr);
    }
};

const Program = struct {
    statements: []const Statement,

    fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *Program = @alignCast(@ptrCast(ptr));
        // Todo: implement this
        _ = self;
    }

    fn node(self: *Program) Node {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const Statement = struct {
    // Todo: not sure what to do with this yet
    statement_node_fn: fn () void,

    fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *Statement = @alignCast(@ptrCast(ptr));
        // Todo: implement this
        _ = self;
    }

    fn node(self: *Statement) Node {
        return .{
            .ptr = self,
            .token_literal_fn = token_literal,
        };
    }
};

const Expression = struct {
    fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *Expression = @alignCast(@ptrCast(ptr));
        // Todo: implement this
        _ = self;
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

    fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *Identifier = @alignCast(@ptrCast(ptr));
        // Todo: implement this
        _ = self;
    }

    fn node(self: *Identifier) Expression {
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

    // Todo: statement_node_fn
    // Todo: token_literal; i guess would be token.literal + identifier.token_literal() + "=" + value.token_literal();
};
