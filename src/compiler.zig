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
    UndeclaredIdentifier,
    DuplicateIdentifier,
};

const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: u64,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []object.Object,
};

// Todo: update exisiting global_var to symbol table
// Unless you can get scope info from existing implementation
const symbol_table = struct {
    scope: []const u8,
    symbols: [][]const u8,

    fn add_symbol() void {}
    fn get_symbol(_: []const u8) void {}
};

const Compiler = struct {
    instructions: ?[]u8,
    constants: ?[]object.Object,
    global_var: ?[][]const u8,
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
                        _ = try self.emit(code.Opcode.opPop, &.{});
                    },
                    .block_statement => |b| {
                        for (b.statements) |stmt| {
                            try self.compile(ast.Node{ .statement = stmt });
                        }
                    },
                    .let_statement => |l| {
                        try self.compile(ast.Node{ .expression = l.value });
                        const global_idx = self.add_global_variable(l.name.value) catch {
                            return CompilerError.Oom;
                        };
                        _ = try self.emit(code.Opcode.opSetGlobal, &.{global_idx});
                    },
                    else => unreachable,
                }
            },
            .expression => |e| {
                switch (e) {
                    .if_expression => |if_e| {
                        try self.compile(ast.Node{ .expression = if_e.condition });

                        const jumpNtTruePos = try self.emit(code.Opcode.opJumpNtTrue, &.{stack_size});

                        try self.compile(ast.Node{ .statement = .{ .block_statement = if_e.consequence } });
                        if (self.last_instr.?.opcode == code.Opcode.opPop) {
                            self.remove_last_instr();
                        }

                        const jumpPos = try self.emit(code.Opcode.opJump, &.{stack_size});
                        self.replace_instr(code.Opcode.opJumpNtTrue, jumpNtTruePos, self.instructions.?.len);

                        if (if_e.alternative) |a| {
                            try self.compile(ast.Node{ .statement = .{ .block_statement = a } });
                            if (self.last_instr.?.opcode == code.Opcode.opPop) {
                                self.remove_last_instr();
                            }
                        } else {
                            _ = try self.emit(code.Opcode.opNull, &.{});
                        }

                        self.replace_instr(code.Opcode.opJump, jumpPos, self.instructions.?.len);
                    },
                    .prefix_expression => |p| {
                        try self.compile(ast.Node{ .expression = p.right });

                        switch (p.token.token_type) {
                            .Minus => {
                                _ = try self.emit(code.Opcode.opMinus, &.{});
                            },
                            .Bang => {
                                _ = try self.emit(code.Opcode.opNot, &.{});
                            },
                            else => unreachable,
                        }
                    },
                    .infix_expression => |in| {
                        try self.compile(ast.Node{ .expression = in.left });
                        try self.compile(ast.Node{ .expression = in.right });
                        switch (in.token.token_type) {
                            .Plus => {
                                _ = try self.emit(code.Opcode.opAdd, &.{});
                            },
                            .Minus => {
                                _ = try self.emit(code.Opcode.opSub, &.{});
                            },
                            .Asterix => {
                                _ = try self.emit(code.Opcode.opMul, &.{});
                            },
                            .Slash => {
                                _ = try self.emit(code.Opcode.opDiv, &.{});
                            },
                            .Equal => {
                                _ = try self.emit(code.Opcode.opEqual, &.{});
                            },
                            .NotEqual => {
                                _ = try self.emit(code.Opcode.opNotEqual, &.{});
                            },
                            .Lt => {
                                _ = try self.emit(code.Opcode.opLt, &.{});
                            },
                            .Gt => {
                                _ = try self.emit(code.Opcode.opGt, &.{});
                            },
                            else => unreachable,
                        }
                    },
                    .identifier => |ident| {
                        const idx = self.get_global_var_idx(ident.value);
                        if (idx == -1) {
                            return CompilerError.UndeclaredIdentifier;
                        }

                        _ = try self.emit(code.Opcode.opGetGlobal, &.{@intCast(idx)});
                    },
                    .string => |s| {
                        const idx = try self.add_constant(object.Object{ .string = object.String{ .value = s.value } });
                        _ = try self.emit(code.Opcode.opConstant, &.{idx});
                    },
                    .integer => |int| {
                        const idx = try self.add_constant(object.Object{ .integer = object.Integer{ .value = int.value } });
                        _ = try self.emit(code.Opcode.opConstant, &.{idx});
                    },
                    .boolean => |b| {
                        var op_bool: code.Opcode = code.Opcode.opTrue;
                        if (!b.value) {
                            op_bool = code.Opcode.opFalse;
                        }

                        _ = try self.emit(op_bool, &.{});
                    },
                    .array_literal => |a| {
                        for (a.elements) |el| {
                            try self.compile(ast.Node{ .expression = el });
                        }

                        _ = try self.emit(code.Opcode.opArray, &.{@intCast(a.elements.len)});
                    },
                    else => unreachable,
                }
            },
        }
    }

    fn get_global_var_idx(self: *Compiler, variable_name: []const u8) i64 {
        if (self.global_var) |global_vars| {
            for (global_vars, 0..) |v, i| {
                if (mem.eql(u8, variable_name, v)) {
                    return @intCast(i);
                }
            }
        }

        return -1;
    }

    fn add_global_variable(self: *Compiler, variable_name: []const u8) CompilerError!u64 {
        const idx = self.get_global_var_idx(variable_name);
        if (idx != -1) {
            return CompilerError.DuplicateIdentifier;
        }

        // Todo: keep new array list capacity only as many as needed?
        var variables = std.ArrayList([]const u8).init(self.allocator);
        if (self.global_var) |c| {
            variables.appendSlice(c) catch {
                return CompilerError.Oom;
            };
        }

        variables.append(variable_name) catch {
            return CompilerError.Oom;
        };
        self.global_var = variables.items;

        return variables.items.len - 1;
    }

    fn add_constant(self: *Compiler, obj: object.Object) CompilerError!u64 {
        var constants = std.ArrayList(object.Object).init(self.allocator);
        if (self.constants) |c| {
            constants.appendSlice(c) catch {
                return CompilerError.Oom;
            };
        }

        constants.append(obj) catch {
            return CompilerError.Oom;
        };
        self.constants = constants.items;

        return constants.items.len - 1;
    }

    fn emit(self: *Compiler, operator: code.Opcode, operands: []const u64) CompilerError!u64 {
        var instructions = std.ArrayList(u8).init(self.allocator);
        var pos: u64 = 0;
        if (self.instructions) |ins| {
            pos = ins.len;
            instructions.appendSlice(ins) catch {
                return CompilerError.Oom;
            };
        }

        self.add_last_instr(operator, instructions.items.len);

        var idx: usize = 0;
        if (self.constants) |c| {
            idx = c.len - 1;
        }

        const newInstructions = code.make(operator, operands, self.allocator);
        for (newInstructions) |inst| {
            instructions.append(inst) catch {
                return CompilerError.Oom;
            };
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
        .global_var = null,
        .prev_instr = null,
        .last_instr = null,
    };

    return compiler;
}

pub fn NewWithState(allocator: mem.Allocator, constants: ?[]object.Object, global_variables: ?[][]const u8) !*Compiler {
    const c = try New(allocator);
    c.constants = constants;
    c.global_var = global_variables;

    return c;
}

test "compiled primitive type instructions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "'hello';",
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "'hello' + 'world';",
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opAdd, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
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

test "compile variables" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const i64,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "let x = 5;",
            .expectedConstants = &.{5},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
            },
        },
        .{
            .input = "let x = 5; x;",
            .expectedConstants = &.{5},
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opGetGlobal, &.{0}, allocator),
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

test "compile arrays" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const i64,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .expectedConstants = &.{ 1, 2, 3, 4, 5, 6 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opAdd, &.{}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opConstant, &.{3}, allocator),
                code.make(code.Opcode.opSub, &.{}, allocator),
                code.make(code.Opcode.opConstant, &.{4}, allocator),
                code.make(code.Opcode.opConstant, &.{5}, allocator),
                code.make(code.Opcode.opMul, &.{}, allocator),
                code.make(code.Opcode.opArray, &.{3}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "[]",
            .expectedConstants = &.{},
            .expectedInstructions = &.{
                code.make(code.Opcode.opArray, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "[0, 1, 2]",
            .expectedConstants = &.{ 0, 1, 2 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opArray, &.{3}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "[0, 1, 2, 3, 4]",
            .expectedConstants = &.{ 0, 1, 2, 3, 4 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opConstant, &.{3}, allocator),
                code.make(code.Opcode.opConstant, &.{4}, allocator),
                code.make(code.Opcode.opArray, &.{5}, allocator),
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
