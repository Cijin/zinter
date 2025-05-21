const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const assert = std.debug.assert;

const code = @import("code.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");

const CompilerError = error{};

const ByteCode = struct {
    instructions: []u8,
    constants: []object.Object,
};

const compiler = struct {
    instructions: std.ArrayList(u8),
    constants: std.ArrayList(object.Object),

    fn compile(self: compiler, node: ast.Node) CompilerError!void {
        switch (node) {
            .program => |p| {
                for (p.statements) |stmt| {
                    try self.compile(ast.Node{ .statement = stmt });
                }
            },
            .statement => |s| {
                switch (s) {
                    .expression_statement => |es| {
                        try self.compile(ast.Node{ .expression = es.expression });
                    },
                    else => unreachable,
                }
            },
            .expression => |e| {
                switch (e) {
                    .infix_expression => |in| {
                        try self.compile(ast.Node{ .expression = in.left });

                        try self.compile(ast.Node{ .expression = in.right });
                    },
                    .integer => |int| {
                        self.constants.append(object.Object{ .integer = object.Integer{ .value = int.value } }) catch unreachable;

                        const idx = self.constants.items.len - 1;
                        const newInstructions = code.make(code.Opcode.opconstant, &.{idx});
                        var instr = self.instructions.addManyAsSlice(newInstructions.len);
                        instr = newInstructions;
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn byte_code(self: compiler) ByteCode {
        return ByteCode{
            .instructions = self.instructions.items,
            .constants = self.constants.items,
        };
    }
};

fn New(allocator: mem.Allocator) compiler {
    return compiler{
        .instructions = std.ArrayList(u8).init(allocator),
        .constants = std.ArrayList(object.Object).init(allocator),
    };
}

test "compiled arithmatic instructions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const i64,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "1 + 2;",
            .expectedConstants = &.{ 1, 2 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
            },
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = New(allocator);
        try c.compile(ast.Node{ .program = program });
        const got = c.byte_code();

        var concatted_instr = std.ArrayList(u8).init(allocator);
        for (t.expectedInstructions) |insts| {
            for (insts) |inst| {
                try concatted_instr.append(inst);
            }
        }

        try testing.expectEqualSlices(u8, concatted_instr.items, got.instructions);

        assert(t.expectedConstants.len == got.constants.len);
        for (got.constants, 0..) |constant, i| {
            const int: object.Integer = constant.integer;
            try testing.expectEqual(t.expectedConstants[i], int.value);
        }
    }
}
