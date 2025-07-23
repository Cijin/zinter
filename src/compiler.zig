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

const scope_size = 128;
const local_scope = "Local_";
const global_scope = "Global";

const CompilerError = error{
    Oom,
    UndeclaredIdentifier,
    DuplicateIdentifier,
};

const Scope = struct {
    instructions: ?[]u8,
    prev_instr: ?EmittedInstruction,
    last_instr: ?EmittedInstruction,
};

const EmittedInstruction = struct {
    opcode: code.Opcode,
    pos: u64,
};

pub const ByteCode = struct {
    instructions: []u8,
    constants: []object.Object,
};

const Symbol = struct {
    idx: u64,
    name: []const u8,
    scope: []const u8,
};

const SymbolTable = struct {
    prev: ?*SymbolTable,
    symbols: std.StringHashMap(Symbol),
    symbol_count: u32,

    pub fn get(self: *SymbolTable, k: []const u8) ?Symbol {
        if (self.symbols.get(k)) |v| {
            return v;
        }

        if (self.prev) |prev| {
            return prev.get(k);
        }

        return null;
    }

    pub fn add(self: *SymbolTable, k: []const u8) CompilerError!Symbol {
        if (self.symbols.get(k)) |_| {
            return CompilerError.DuplicateIdentifier;
        }

        var scope = global_scope;
        if (self.prev) |_| {
            scope = local_scope;
        }

        const symbol = Symbol{ .name = k, .idx = self.symbol_count, .scope = scope };
        self.symbols.put(k, symbol) catch unreachable;
        self.symbol_count += 1;

        return symbol;
    }
};

fn new_symbol_table(enclosing_scope: ?*SymbolTable, allocator: mem.Allocator) *SymbolTable {
    var symbol_table = allocator.create(SymbolTable) catch unreachable;
    symbol_table.* = SymbolTable{
        .prev = null,
        .symbols = std.StringHashMap(Symbol).init(allocator),
        .symbol_count = 0,
    };

    if (enclosing_scope) |scope| {
        symbol_table.prev = scope;
    }

    return symbol_table;
}

const Compiler = struct {
    instructions: ?[]u8,
    constants: ?[]object.Object,
    global_var: ?[][]const u8,
    symbol_table: *SymbolTable,
    scopes: [scope_size]Scope,
    scope_idx: u64,
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
                        _ = try self.emit(code.Opcode.opPop, &.{});
                    },
                    .block_statement => |b| {
                        for (b.statements) |stmt| {
                            try self.compile(ast.Node{ .statement = stmt });
                        }
                    },
                    .let_statement => |l| {
                        try self.compile(ast.Node{ .expression = l.value });

                        const symbol = try self.symbol_table.add(l.name.value);
                        if (mem.eql(u8, symbol.scope, global_scope)) {
                            _ = try self.emit(code.Opcode.opSetGlobal, &.{symbol.idx});
                        } else {
                            _ = try self.emit(code.Opcode.opSetLocal, &.{symbol.idx});
                        }
                    },
                    .return_statement => |r| {
                        if (r.return_value) |v| {
                            try self.compile(ast.Node{ .expression = v });
                            _ = try self.emit(code.Opcode.opReturnValue, &.{});
                        } else {
                            _ = try self.emit(code.Opcode.opReturn, &.{});
                        }
                    },
                }
            },
            .expression => |e| {
                switch (e) {
                    .if_expression => |if_e| {
                        try self.compile(ast.Node{ .expression = if_e.condition });

                        const jumpNtTruePos = try self.emit(code.Opcode.opJumpNtTrue, &.{stack_size});

                        try self.compile(ast.Node{ .statement = .{ .block_statement = if_e.consequence } });
                        if (self.scopes[self.scope_idx].last_instr.?.opcode == code.Opcode.opPop) {
                            self.remove_last_instr();
                        }

                        const jumpPos = try self.emit(code.Opcode.opJump, &.{stack_size});
                        self.replace_instr(code.Opcode.opJumpNtTrue, jumpNtTruePos, self.scopes[self.scope_idx].instructions.?.len);

                        if (if_e.alternative) |a| {
                            try self.compile(ast.Node{ .statement = .{ .block_statement = a } });
                            if (self.scopes[self.scope_idx].last_instr.?.opcode == code.Opcode.opPop) {
                                self.remove_last_instr();
                            }
                        } else {
                            _ = try self.emit(code.Opcode.opNull, &.{});
                        }

                        self.replace_instr(code.Opcode.opJump, jumpPos, self.scopes[self.scope_idx].instructions.?.len);
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
                        const symbol = self.symbol_table.get(ident.value);
                        if (symbol) |s| {
                            if (mem.eql(u8, s.scope, global_scope)) {
                                _ = try self.emit(code.Opcode.opGetGlobal, &.{@intCast(s.idx)});
                            } else {
                                _ = try self.emit(code.Opcode.opGetLocal, &.{@intCast(s.idx)});
                            }
                        } else {
                            return CompilerError.UndeclaredIdentifier;
                        }
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
                    .index_expression => |i| {
                        try self.compile(ast.Node{ .expression = i.array });
                        try self.compile(ast.Node{ .expression = i.index });

                        _ = try self.emit(code.Opcode.opIndex, &.{});
                    },
                    .fn_literal => |fn_lit| {
                        self.new_scope();

                        for (fn_lit.parameters) |p| {
                            _ = try self.symbol_table.add(p.value);
                        }

                        try self.compile(ast.Node{ .statement = .{ .block_statement = fn_lit.body } });

                        // handles cases where either function body is empty or implicitly returns
                        if (self.scopes[self.scope_idx].last_instr) |li| {
                            if (li.opcode != .opReturnValue and li.opcode != .opReturn) {
                                if (li.opcode == code.Opcode.opPop) {
                                    self.remove_last_instr();
                                }
                                _ = try self.emit(code.Opcode.opReturn, &.{});
                            }
                        } else {
                            _ = try self.emit(code.Opcode.opReturn, &.{});
                        }

                        // get count before exiting scope
                        const symbol_count = self.symbol_table.symbol_count;
                        const instrs = self.exit_scope();
                        const fn_instrs = object.Object{
                            .fn_instrs = .{
                                .value = instrs,
                                .symbol_count = symbol_count,
                                .param_count = @intCast(fn_lit.parameters.len),
                            },
                        };

                        const idx = try self.add_constant(fn_instrs);
                        _ = try self.emit(code.Opcode.opConstant, &.{idx});
                    },
                    .call_expression => |ce| {
                        try self.compile(ast.Node{ .expression = ce.function });

                        for (ce.arguments) |a| {
                            try self.compile(ast.Node{ .expression = a });
                        }

                        _ = try self.emit(code.Opcode.opCall, &.{ce.arguments.len});
                    },
                }
            },
        }
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
        if (self.scopes[self.scope_idx].instructions) |ins| {
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

        self.scopes[self.scope_idx].instructions = instructions.items;
        return pos;
    }

    pub fn byte_code(self: *Compiler) ByteCode {
        return ByteCode{
            .instructions = self.scopes[self.scope_idx].instructions orelse &.{},
            .constants = self.constants orelse &.{},
        };
    }

    fn remove_last_instr(self: *Compiler) void {
        const last_instr_pos = self.scopes[self.scope_idx].last_instr.?.pos;
        const instructions_len: u64 = self.scopes[self.scope_idx].instructions.?.len;
        assert(last_instr_pos < instructions_len);

        if (self.scopes[self.scope_idx].instructions) |s| {
            self.scopes[self.scope_idx].instructions = s[0..last_instr_pos];
        }
    }

    fn replace_instr(self: *Compiler, opcode: code.Opcode, pos: u64, value: u64) void {
        const updated_instrs = code.make(opcode, &.{value}, self.allocator);

        if (self.scopes[self.scope_idx].instructions) |ins| {
            assert(@intFromEnum(opcode) == ins[pos]);

            for (updated_instrs, pos..) |updated_instr, i| {
                ins[i] = updated_instr;
            }
        } else unreachable;
    }

    fn add_last_instr(self: *Compiler, opcode: code.Opcode, pos: u64) void {
        assert(self.scope_idx >= 0);

        const last_instr = EmittedInstruction{ .opcode = opcode, .pos = pos };

        if (self.scopes[self.scope_idx].last_instr) |i| {
            self.scopes[self.scope_idx].prev_instr = i;
        }

        self.scopes[self.scope_idx].last_instr = last_instr;
    }

    fn new_scope(self: *Compiler) void {
        assert(self.scope_idx < scope_size);

        self.symbol_table = new_symbol_table(self.symbol_table, self.allocator);

        self.scope_idx += 1;
        self.scopes[self.scope_idx] = Scope{ .instructions = null, .prev_instr = null, .last_instr = null };
    }

    fn exit_scope(self: *Compiler) []u8 {
        var instrs: []u8 = &.{};
        if (self.scopes[self.scope_idx].instructions) |i| {
            instrs = i;
        }

        self.symbol_table = self.symbol_table.prev orelse unreachable;

        assert(self.scope_idx > 0);
        self.scope_idx -= 1;

        return instrs;
    }
};

pub fn New(allocator: mem.Allocator) !*Compiler {
    const compiler = try allocator.create(Compiler);

    compiler.* = Compiler{
        .allocator = allocator,
        .instructions = null,
        .constants = null,
        .global_var = null,
        .symbol_table = new_symbol_table(null, allocator),
        .scopes = .{Scope{ .instructions = null, .prev_instr = null, .last_instr = null }} ** 128,
        .scope_idx = 0,
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

test "compile arrays and index operators" {
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
        .{
            .input = "[0, 4][0];",
            .expectedConstants = &.{ 0, 4, 0 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opArray, &.{2}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opIndex, &.{}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "[0, 4, 5][1 + 1];",
            .expectedConstants = &.{ 0, 4, 5, 1, 1 },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opArray, &.{3}, allocator),
                code.make(code.Opcode.opConstant, &.{3}, allocator),
                code.make(code.Opcode.opConstant, &.{4}, allocator),
                code.make(code.Opcode.opAdd, &.{}, allocator),
                code.make(code.Opcode.opIndex, &.{}, allocator),
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

test "compiler scopes" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "";
    const l = try lexer.New(allocator, input);
    const p = try parser.New(allocator, l);
    const program = try p.parse_program();

    var c = try New(allocator);
    try c.compile(ast.Node{ .program = program });

    _ = try c.emit(code.Opcode.opAdd, &.{});
    try testing.expectEqualSlices(
        u8,
        code.make(code.Opcode.opAdd, &.{}, allocator),
        c.byte_code().instructions,
    );

    c.new_scope();
    _ = try c.emit(code.Opcode.opSub, &.{});
    try testing.expectEqualSlices(
        u8,
        code.make(code.Opcode.opSub, &.{}, allocator),
        c.byte_code().instructions,
    );

    const prev_scope_instrs = c.exit_scope();
    try testing.expectEqualSlices(
        u8,
        code.make(code.Opcode.opSub, &.{}, allocator),
        prev_scope_instrs,
    );

    try testing.expectEqualSlices(
        u8,
        code.make(code.Opcode.opAdd, &.{}, allocator),
        c.byte_code().instructions,
    );
}

test "compile fn declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedConstants: []const object.Object,
        expectedInstructions: []const []u8,
    }{
        .{
            .input = "fn() { return 5 };",
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{
                    .fn_instrs = .{
                        .value = try mem.concat(
                            allocator,
                            u8,
                            &[_][]const u8{
                                code.make(code.Opcode.opConstant, &.{0}, allocator),
                                code.make(code.Opcode.opReturnValue, &.{}, allocator),
                            },
                        ),
                        .symbol_count = 0,
                        .param_count = 0,
                    },
                },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn() { 5 + 5; };",
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{
                    .fn_instrs = .{
                        .value = try mem.concat(
                            allocator,
                            u8,
                            &[_][]const u8{
                                code.make(code.Opcode.opConstant, &.{0}, allocator),
                                code.make(code.Opcode.opConstant, &.{1}, allocator),
                                code.make(code.Opcode.opAdd, &.{}, allocator),
                                code.make(code.Opcode.opReturn, &.{}, allocator),
                            },
                        ),
                        .symbol_count = 2,
                        .param_count = 0,
                    },
                },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn() { return 5 + 5; };",
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{0}, allocator),
                            code.make(code.Opcode.opConstant, &.{1}, allocator),
                            code.make(code.Opcode.opAdd, &.{}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 2,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn() { return 5 + 5; }();",
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{0}, allocator),
                            code.make(code.Opcode.opConstant, &.{1}, allocator),
                            code.make(code.Opcode.opAdd, &.{}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 2,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "let x = fn() { return 5; }; x();",
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{0}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 1,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opGetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn() { return; }();",
            .expectedConstants = &.{
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opReturn, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 0,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn() {}();",
            .expectedConstants = &.{
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opReturn, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 0,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn(a) {}(1);",
            .expectedConstants = &.{
                object.Object{
                    .fn_instrs = .{
                        .value = try mem.concat(
                            allocator,
                            u8,
                            &[_][]const u8{
                                code.make(code.Opcode.opReturn, &.{}, allocator),
                            },
                        ),
                        .symbol_count = 0,
                        .param_count = 0,
                    },
                },
                object.Object{ .integer = .{ .value = 1 } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opCall, &.{1}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn(a, b, c) {}(1, 2, 3);",
            .expectedConstants = &.{
                object.Object{
                    .fn_instrs = .{
                        .value = try mem.concat(
                            allocator,
                            u8,
                            &[_][]const u8{
                                code.make(code.Opcode.opReturn, &.{}, allocator),
                            },
                        ),
                        .symbol_count = 0,
                        .param_count = 0,
                    },
                },
                object.Object{ .integer = .{ .value = 1 } },
                object.Object{ .integer = .{ .value = 2 } },
                object.Object{ .integer = .{ .value = 3 } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opConstant, &.{3}, allocator),
                code.make(code.Opcode.opCall, &.{3}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input = "fn(a, b, c) { a; b; c; }(1, 2, 3);",
            .expectedConstants = &.{
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opGetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opPop, &.{}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{1}, allocator),
                            code.make(code.Opcode.opPop, &.{}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{2}, allocator),
                            code.make(code.Opcode.opReturn, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 0,
                    .param_count = 0,
                } },
                object.Object{ .integer = .{ .value = 1 } },
                object.Object{ .integer = .{ .value = 2 } },
                object.Object{ .integer = .{ .value = 3 } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opConstant, &.{3}, allocator),
                code.make(code.Opcode.opCall, &.{3}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input =
            \\ let x = 5;
            \\ fn() { return x;}();
            ,
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opGetGlobal, &.{0}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 0,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{1}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input =
            \\ let x = 5;
            \\ fn() { let y = 7; return x + y;}();
            ,
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 5 } },
                object.Object{ .integer = .{ .value = 7 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{1}, allocator),
                            code.make(code.Opcode.opSetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opGetGlobal, &.{0}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opAdd, &.{}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 1,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{0}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input =
            \\ fn() { 
            \\  let x = 6;
            \\  let y = 7;
            \\  return x + y;
            \\ };
            ,
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 6 } },
                object.Object{ .integer = .{ .value = 7 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{0}, allocator),
                            code.make(code.Opcode.opSetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opConstant, &.{1}, allocator),
                            code.make(code.Opcode.opSetLocal, &.{1}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{1}, allocator),
                            code.make(code.Opcode.opAdd, &.{}, allocator),
                            code.make(code.Opcode.opReturnValue, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 2,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opPop, &.{}, allocator),
            },
        },
        .{
            .input =
            \\ let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
            \\ let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
            \\ oneAndTwo() + threeAndFour();
            ,
            .expectedConstants = &.{
                object.Object{ .integer = .{ .value = 1 } },
                object.Object{ .integer = .{ .value = 2 } },
                object.Object{
                    .fn_instrs = .{
                        .value = try mem.concat(
                            allocator,
                            u8,
                            &[_][]const u8{
                                code.make(code.Opcode.opConstant, &.{0}, allocator),
                                code.make(code.Opcode.opSetLocal, &.{0}, allocator),
                                code.make(code.Opcode.opConstant, &.{1}, allocator),
                                code.make(code.Opcode.opSetLocal, &.{1}, allocator),
                                code.make(code.Opcode.opGetLocal, &.{0}, allocator),
                                code.make(code.Opcode.opGetLocal, &.{1}, allocator),
                                code.make(code.Opcode.opAdd, &.{}, allocator),
                                code.make(code.Opcode.opReturn, &.{}, allocator),
                            },
                        ),
                        .symbol_count = 2,
                        .param_count = 0,
                    },
                },
                object.Object{ .integer = .{ .value = 3 } },
                object.Object{ .integer = .{ .value = 4 } },
                object.Object{ .fn_instrs = .{
                    .value = try mem.concat(
                        allocator,
                        u8,
                        &[_][]const u8{
                            code.make(code.Opcode.opConstant, &.{3}, allocator),
                            code.make(code.Opcode.opSetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opConstant, &.{4}, allocator),
                            code.make(code.Opcode.opSetLocal, &.{1}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{0}, allocator),
                            code.make(code.Opcode.opGetLocal, &.{1}, allocator),
                            code.make(code.Opcode.opAdd, &.{}, allocator),
                            code.make(code.Opcode.opReturn, &.{}, allocator),
                        },
                    ),
                    .symbol_count = 2,
                    .param_count = 0,
                } },
            },
            .expectedInstructions = &.{
                code.make(code.Opcode.opConstant, &.{2}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opConstant, &.{5}, allocator),
                code.make(code.Opcode.opSetGlobal, &.{1}, allocator),
                code.make(code.Opcode.opGetGlobal, &.{0}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
                code.make(code.Opcode.opGetGlobal, &.{1}, allocator),
                code.make(code.Opcode.opCall, &.{0}, allocator),
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
        for (t.expectedConstants, 0..) |constant, i| {
            try testing.expectEqual(true, constant.equal(got.constants[i]));
        }
    }
}

test "resolve local symbol table" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const global = new_symbol_table(null, allocator);
    _ = try global.add("a");
    _ = try global.add("b");

    const local = new_symbol_table(global, allocator);
    _ = try local.add("c");
    _ = try local.add("d");

    const expected = [_]Symbol{
        Symbol{ .name = "a", .scope = global_scope, .idx = 0 },
        Symbol{ .name = "b", .scope = global_scope, .idx = 1 },
        Symbol{ .name = "c", .scope = local_scope, .idx = 0 },
        Symbol{ .name = "d", .scope = local_scope, .idx = 1 },
    };

    for (expected) |e| {
        const got = local.get(e.name);
        if (got) |g| {
            try testing.expectEqualStrings(e.name, g.name);
            try testing.expectEqualStrings(e.scope, g.scope);
            try testing.expectEqual(e.idx, g.idx);
        } else unreachable;
    }
}

test "resolve nested symbol table" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const global = new_symbol_table(null, allocator);
    _ = try global.add("a");
    _ = try global.add("b");

    const first_local = new_symbol_table(global, allocator);
    _ = try first_local.add("c");
    _ = try first_local.add("d");

    const second_local = new_symbol_table(global, allocator);
    _ = try second_local.add("e");
    _ = try second_local.add("f");

    const tests = [_]struct {
        table: *SymbolTable,
        symbols: [4]Symbol,
    }{
        .{
            .table = first_local,
            .symbols = [_]Symbol{
                Symbol{ .name = "a", .scope = global_scope, .idx = 0 },
                Symbol{ .name = "b", .scope = global_scope, .idx = 1 },
                Symbol{ .name = "c", .scope = local_scope, .idx = 0 },
                Symbol{ .name = "d", .scope = local_scope, .idx = 1 },
            },
        },
        .{
            .table = second_local,
            .symbols = [_]Symbol{
                Symbol{ .name = "a", .scope = global_scope, .idx = 0 },
                Symbol{ .name = "b", .scope = global_scope, .idx = 1 },
                Symbol{ .name = "e", .scope = local_scope, .idx = 0 },
                Symbol{ .name = "f", .scope = local_scope, .idx = 1 },
            },
        },
    };

    for (tests) |t| {
        for (t.symbols) |s| {
            const got = t.table.get(s.name);
            if (got) |g| {
                try testing.expectEqualStrings(s.name, g.name);
                try testing.expectEqualStrings(s.scope, g.scope);
                try testing.expectEqual(s.idx, g.idx);
            } else unreachable;
        }
    }
}
