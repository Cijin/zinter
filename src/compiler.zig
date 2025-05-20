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

    fn compile(_: compiler, _: ast.Program) CompilerError!void {}

    fn byte_code(self: compiler) ByteCode {
        return ByteCode{
            .instructions = self.instructions.items,
            .constants = self.constants.items,
        };
    }
};

fn New(allocator: mem.Allocator) compiler {
    const instructions = std.ArrayList(u8).init(allocator);
    const constants = std.ArrayList(object.Object).init(allocator);

    return compiler{
        .instructions = instructions,
        .constants = constants,
    };
}

// Todo: fix this test
// 1. Check input hex values, they look totally wrong
// 2. Check tests for code.make, I don't think it's correct
test "compiler" {
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
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
            },
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        const c = New(allocator);
        try c.compile(program);
        const got = c.byte_code();

        var concatted_instr = std.ArrayList(u8).init(allocator);
        for (t.expectedInstructions) |insts| {
            for (insts) |inst| {
                try concatted_instr.append(inst);
            }
        }

        assert(t.expectedConstants.len == got.constants.len);
        for (got.constants, 0..) |constant, i| {
            const int: object.Integer = constant.integer;
            try testing.expectEqual(t.expectedConstants[i], int.value);
        }

        try testing.expectEqualSlices(u8, concatted_instr.items, got.instructions);
    }
}
