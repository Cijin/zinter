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
const stack_size = @import("machine.zig").stack_size;

const CompilerError = error{
    Oom,
};

const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: u64,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []object.Object,
};

const Compiler = struct {
    instructions: ?[]u8,
    constants: ?[]object.Object,
    allocator: mem.Allocator,
    prev_instr: ?EmittedInstruction,
    last_instr: ?EmittedInstruction,

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
                    .block_statement => |b| {
                        for (b.statements) |stmt| {
                            try self.compile(ast.Node{ .statement = stmt });
                        }
                    },
                    else => unreachable,
                }
            },
            .expression => |e| {
                switch (e) {
                    .if_expression => |if_e| {
                        // 1. condition -> infix -> sp+1 sp+1 opcode
                        // xx make(JUMP 01 sp + 1) -> bytecode sp -> *jump xy -> instruction_ptr?
                        // 2. consequence -> sp...
                        // xy = 22
                        // xx make(JUMP 02 -> sp + 1)
                        // 3. alternative

                        // Todo:
                        // let x = if (5 > 6) <consequence> else <alternative>
                        try self.compile(ast.Node{ .expression = if_e.condition });
                        self.emit(code.Opcode.opJumpNtTrue, &.{stack_size + 1}) catch {
                            return CompilerError.Oom;
                        };
                        // let x = if (true) { 5; } (pop 5);
                        try self.compile(ast.Node{ .statement = .{ .block_statement = if_e.consequence } });
                        self.remove_last_instr();
                    },
                    .prefix_expression => |p| {
                        try self.compile(ast.Node{ .expression = p.right });

                        switch (p.token.token_type) {
                            .Minus => {
                                self.emit(code.Opcode.opMinus, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Bang => {
                                self.emit(code.Opcode.opNot, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            else => unreachable,
                        }
                    },
                    .infix_expression => |in| {
                        try self.compile(ast.Node{ .expression = in.left });
                        try self.compile(ast.Node{ .expression = in.right });
                        switch (in.token.token_type) {
                            .Plus => {
                                self.emit(code.Opcode.opAdd, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Minus => {
                                self.emit(code.Opcode.opSub, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Asterix => {
                                self.emit(code.Opcode.opMul, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Slash => {
                                self.emit(code.Opcode.opDiv, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Equal => {
                                self.emit(code.Opcode.opEqual, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .NotEqual => {
                                self.emit(code.Opcode.opNotEqual, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Lt => {
                                self.emit(code.Opcode.opLt, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Gt => {
                                self.emit(code.Opcode.opGt, &.{}) catch {
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
                    .boolean => |b| {
                        const idx = self.add_constant(object.Object{ .boolean = object.Boolean{ .value = b.value } }) catch {
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

        self.add_last_instr(operator, instructions.items.len);

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

    fn remove_last_instr(self: *Compiler) void {
        const last_instr_pos = self.last_instr.?.pos;
        const instructions_len: u64 = self.instructions.?.len;
        assert(last_instr_pos < instructions_len);

        if (self.instructions) |s| {
            self.instructions = s[0..last_instr_pos];
        }
    }

    fn add_last_instr(self: *Compiler, opcode: code.Opcode, pos: u64) void {
        const last_instr = EmittedInstruction{ .opcode = opcode, .pos = pos };

        if (self.last_instr) |i| {
            self.prev_instr = i;
        }
        self.last_instr = last_instr;
    }
};

pub fn New(allocator: mem.Allocator) !*Compiler {
    const compiler = try allocator.create(Compiler);

    compiler.* = Compiler{
        .allocator = allocator,
        .instructions = null,
        .constants = null,
        .prev_instr = null,
        .last_instr = null,
    };

    return compiler;
}

test "compiled boolean instructions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const bool,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "true;",
            .expectedConstants = &.{true},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "false;",
            .expectedConstants = &.{false},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "true == true",
            .expectedConstants = &.{ true, true },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "false != false",
            .expectedConstants = &.{ false, false },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opNotEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "!false;",
            .expectedConstants = &.{false},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opNot, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "!true;",
            .expectedConstants = &.{true},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opNot, &.{}, allocator),
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
            const b: object.Boolean = constant.boolean;
            try testing.expectEqual(t.expectedConstants[i], b.value);
        }
    }
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
        .{
            .input = "4 - 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opSub, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "4 * 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opMul, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "4 / 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opDiv, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "5 == 5;",
            .expectedConstants = &.{ 5, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "4 != 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opNotEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "4 > 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opGt, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "4 < 5;",
            .expectedConstants = &.{ 4, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opLt, &.{}, allocator),
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

test "compiled conditional statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const i64,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "if (true) { 5; }",
            .expectedConstants = &.{ 4, 3, 5 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opJumpNtTrue, &.{3}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
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
