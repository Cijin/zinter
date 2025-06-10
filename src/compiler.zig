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
                        _ = self.emit(code.Opcode.opPop, &.{}) catch {
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
                        try self.compile(ast.Node{ .expression = if_e.condition });

                        const jumpNtTruePos = self.emit(code.Opcode.opJumpNtTrue, &.{stack_size + 1}) catch {
                            return CompilerError.Oom;
                        };

                        try self.compile(ast.Node{ .statement = .{ .block_statement = if_e.consequence } });
                        if (self.last_instr.?.opcode == code.Opcode.opPop) {
                            self.remove_last_instr();
                        }

                        const jumpPos = self.emit(code.Opcode.opJump, &.{stack_size + 1}) catch {
                            return CompilerError.Oom;
                        };

                        self.replace_instr(code.Opcode.opJumpNtTrue, jumpNtTruePos, self.instructions.?.len);

                        if (if_e.alternative) |a| {
                            try self.compile(ast.Node{ .statement = .{ .block_statement = a } });
                            if (self.last_instr.?.opcode == code.Opcode.opPop) {
                                self.remove_last_instr();
                            }
                        } else {
                            _ = self.emit(code.Opcode.opNull, &.{}) catch {
                                return CompilerError.Oom;
                            };
                        }

                        self.replace_instr(code.Opcode.opJump, jumpPos, self.instructions.?.len);
                    },
                    .prefix_expression => |p| {
                        try self.compile(ast.Node{ .expression = p.right });

                        switch (p.token.token_type) {
                            .Minus => {
                                _ = self.emit(code.Opcode.opMinus, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Bang => {
                                _ = self.emit(code.Opcode.opNot, &.{}) catch {
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
                                _ = self.emit(code.Opcode.opAdd, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Minus => {
                                _ = self.emit(code.Opcode.opSub, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Asterix => {
                                _ = self.emit(code.Opcode.opMul, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Slash => {
                                _ = self.emit(code.Opcode.opDiv, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Equal => {
                                _ = self.emit(code.Opcode.opEqual, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .NotEqual => {
                                _ = self.emit(code.Opcode.opNotEqual, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Lt => {
                                _ = self.emit(code.Opcode.opLt, &.{}) catch {
                                    return CompilerError.Oom;
                                };
                            },
                            .Gt => {
                                _ = self.emit(code.Opcode.opGt, &.{}) catch {
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
                        _ = self.emit(code.Opcode.opConstant, &.{idx}) catch {
                            return CompilerError.Oom;
                        };
                    },
                    .boolean => |b| {
                        var op_bool: code.Opcode = code.Opcode.opTrue;
                        if (!b.value) {
                            op_bool = code.Opcode.opFalse;
                        }

                        _ = self.emit(op_bool, &.{}) catch {
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

    fn emit(self: *Compiler, operator: code.Opcode, operands: []const u64) !u64 {
        var instructions = std.ArrayList(u8).init(self.allocator);
        var pos: u64 = 0;
        if (self.instructions) |ins| {
            pos = ins.len;
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

        return pos;
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

    fn replace_instr(self: *Compiler, opcode: code.Opcode, pos: u64, value: u64) void {
        const updated_instrs = code.make(opcode, &.{value}, self.allocator);

        if (self.instructions) |ins| {
            assert(@intFromEnum(opcode) == ins[pos]);

            for (updated_instrs, pos..) |updated_instr, i| {
                ins[i] = updated_instr;
            }
        } else unreachable;
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
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "true;",
            .expectedInstructions = &.{
                code.make(code.Opcode.opTrue, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "false;",
            .expectedInstructions = &.{
                code.make(code.Opcode.opFalse, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "true == true",
            .expectedInstructions = &.{
                code.make(code.Opcode.opTrue, &.{}, allocator),
                code.make(code.Opcode.opTrue, &.{}, allocator),
                code.make(code.Opcode.opEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "false != false",
            .expectedInstructions = &.{
                code.make(code.Opcode.opFalse, &.{}, allocator),
                code.make(code.Opcode.opFalse, &.{}, allocator),
                code.make(code.Opcode.opNotEqual, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "!false;",
            .expectedInstructions = &.{
                code.make(code.Opcode.opFalse, &.{}, allocator),
                code.make(code.Opcode.opNot, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "!true;",
            .expectedInstructions = &.{
                code.make(code.Opcode.opTrue, &.{}, allocator),
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
            .input = "if (true) { 10; }",
            .expectedConstants = &.{10},
            .expectedInstructions = &.{
                code.make(code.Opcode.opTrue, &.{}, allocator),
                code.make(code.Opcode.opJumpNtTrue, &.{10}, allocator),
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opJump, &.{11}, allocator),
                code.make(code.Opcode.opNull, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "if (false) { 10; } 100;",
            .expectedConstants = &.{ 10, 100 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opFalse, &.{}, allocator),
                code.make(code.Opcode.opJumpNtTrue, &.{10}, allocator),
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opJump, &.{11}, allocator),
                code.make(code.Opcode.opNull, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "if (true) { 10 } else { 20 }; 3333;",
            .expectedConstants = &.{ 10, 20, 3333 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opTrue, &.{}, allocator),
                code.make(code.Opcode.opJumpNtTrue, &.{10}, allocator),
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opJump, &.{13}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
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
