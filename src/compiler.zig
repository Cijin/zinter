const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const assert = std.debug.assert;

const code = @import("code.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const object = @import("object.zig");
const ast = @import("ast.zig");
const token = @import("token.zig");

const CompilerError = error{
    Oom,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []object.Object,
};

const Compiler = struct {
    instructions: ?[]u8,
    constants: ?[]object.Object,
    allocator: mem.Allocator,

    pub fn compile(self: *Compiler, node: ast.Node) CompilerError!void {
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
                        self.emit(code.Opcode.opPop, &.{}) catch {
                            return CompilerError.Oom;
                        };
                    },
                    else => unreachable,
                }
            },
            .expression => |e| {
                switch (e) {
                    .infix_expression => |in| {
                        try self.compile(ast.Node{ .expression = in.left });
                        try self.compile(ast.Node{ .expression = in.right });
                        switch (in.token.token_type) {
                            .Plus => {
                                self.emit(code.Opcode.opAdd, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            else => unreachable,
                        }
                    },
                    .integer => |int| {
                        const idx = self.add_constant(object.Object{ .integer = object.Integer{ .value = int.value } }) catch {
                            return CompilerError.Oom;
                        };
                        self.emit(code.Opcode.opConstant, &.{idx}) catch {
                            return CompilerError.Oom;
                        };
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn add_constant(self: *Compiler, obj: object.Object) !u64 {
        var constants = std.ArrayList(object.Object).init(self.allocator);
        if (self.constants) |c| {
            try constants.appendSlice(c);
        }

        try constants.append(obj);
        self.constants = constants.items;

        return constants.items.len - 1;
    }

    fn emit(self: *Compiler, operator: code.Opcode, operands: []const u64) !void {
        var instructions = std.ArrayList(u8).init(self.allocator);
        if (self.instructions) |ins| {
            try instructions.appendSlice(ins);
        }

        var idx: usize = 0;
        if (self.constants) |c| {
            idx = c.len - 1;
        }

        const newInstructions = code.make(operator, operands, self.allocator);
        for (newInstructions) |inst| {
            try instructions.append(inst);
        }

        self.instructions = instructions.items;
    }

    pub fn byte_code(self: *Compiler) ByteCode {
        return ByteCode{
            .instructions = self.instructions orelse &.{},
            .constants = self.constants orelse &.{},
        };
    }
};

pub fn New(allocator: mem.Allocator) !*Compiler {
    const compiler = try allocator.create(Compiler);

    compiler.* = Compiler{
        .allocator = allocator,
        .instructions = null,
        .constants = null,
    };

    return compiler;
}

test "compiled arithmetic instructions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const i64,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "4 + 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opAdd, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try New(allocator);
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
