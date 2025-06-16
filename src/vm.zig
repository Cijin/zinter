const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const compiler = @import("compiler.zig");
const code = @import("code.zig");
const stack_size = @import("machine.zig").stack_size;

const RuntimeError = error{
    StackOverflow,
    StackUnderflow,
    DivideByZero,
    IncompatibleTypes,
    IncompatibleOperator,
    UnexpectedType,
};

const VM = struct {
    constants: []object.Object,
    allocator: mem.Allocator,
    instructions: []u8,
    stack: [stack_size]object.Object,
    sp: u32,
    globals: [stack_size]object.Object,

    // Todo:
    // Visual representation of the stack
    // [ ]
    // " " " Instructions
    // [ .... ] | [ .... ]
    // What instructions are getting executed
    // What constant is the instruction executing on
    // Pause
    // Play
    // Control rate of execution
    pub fn run(self: *VM) RuntimeError!void {
        var instr_idx: u32 = 0;
        while (instr_idx < self.instructions.len) : (instr_idx += 1) {
            const instr = self.instructions[instr_idx];
            const opcode: code.Opcode = @enumFromInt(instr);
            switch (opcode) {
                .opConstant => {
                    var const_idx: u16 = @intCast(self.instructions[instr_idx + 1]);
                    const_idx <<= 8;
                    const_idx |= @intCast(self.instructions[instr_idx + 2]);
                    instr_idx += 2;

                    assert(const_idx < self.constants.len);
                    try self.push(self.constants[const_idx]);
                },
                .opAdd => {
                    assert(self.sp >= 2);

                    const right: object.Object = self.pop();
                    const left: object.Object = self.pop();

                    assert(mem.eql(u8, left.typ(), right.typ()));
                    assert(mem.eql(u8, left.typ(), object.INT) or mem.eql(u8, left.typ(), object.STRING));

                    const result = switch (left) {
                        .integer => object.Object{ .integer = .{ .value = left.integer.value + right.integer.value } },
                        .string => blk: {
                            const result = mem.concat(self.allocator, u8, &[_][]const u8{ left.string.value, right.string.value }) catch unreachable;
                            break :blk object.Object{ .string = .{ .value = result } };
                        },
                        else => unreachable,
                    };

                    try self.push(result);
                },
                .opSub, .opMul, .opDiv, .opGt, .opLt => {
                    assert(self.sp >= 2);

                    const obj1: object.Object = self.pop();
                    const obj2: object.Object = self.pop();

                    assert(mem.eql(u8, obj1.typ(), object.INT));
                    assert(mem.eql(u8, obj2.typ(), object.INT));

                    const right: object.Integer = obj1.integer;
                    const left: object.Integer = obj2.integer;

                    switch (opcode) {
                        .opSub => {
                            const result = left.value - right.value;
                            try self.push(object.Object{ .integer = .{ .value = result } });
                        },
                        .opMul => {
                            const result = left.value * right.value;
                            try self.push(object.Object{ .integer = .{ .value = result } });
                        },
                        .opDiv => {
                            const result = @divTrunc(left.value, right.value);
                            try self.push(object.Object{ .integer = .{ .value = result } });
                        },
                        .opEqual => {
                            const result = left.value == right.value;
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        .opNotEqual => {
                            const result = left.value != right.value;
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        .opGt => {
                            const result = left.value > right.value;
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        .opLt => {
                            const result = left.value < right.value;
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        else => unreachable,
                    }
                },
                .opEqual, .opNotEqual => {
                    assert(self.sp >= 2);

                    const obj1: object.Object = self.pop();
                    const obj2: object.Object = self.pop();
                    if (!mem.eql(u8, obj1.typ(), obj2.typ())) {
                        return RuntimeError.IncompatibleTypes;
                    }

                    switch (opcode) {
                        .opEqual => {
                            const result = obj1.equal(obj2);
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        .opNotEqual => {
                            const result = obj1.not_equal(obj2);
                            try self.push(object.Object{ .boolean = .{ .value = result } });
                        },
                        else => unreachable,
                    }
                },
                .opNot => {
                    assert(self.sp >= 1);

                    const obj1: object.Object = self.pop();
                    if (!mem.eql(u8, obj1.typ(), object.BOOL)) {
                        return RuntimeError.IncompatibleOperator;
                    }

                    const right = obj1.boolean.value;
                    try self.push(object.Object{ .boolean = .{ .value = !right } });
                },
                .opMinus => {
                    assert(self.sp >= 1);

                    const obj1: object.Object = self.pop();
                    if (!mem.eql(u8, obj1.typ(), object.INT)) {
                        return RuntimeError.IncompatibleOperator;
                    }

                    const right = obj1.integer.value;
                    try self.push(object.Object{ .integer = .{ .value = -right } });
                },
                .opTrue, .opFalse => {
                    const value = if (opcode == code.Opcode.opTrue) true else false;
                    try self.push(object.Object{ .boolean = .{ .value = value } });
                },
                .opPop => {
                    _ = self.pop();
                },
                .opJumpNtTrue => {
                    var jump_pos: u16 = @intCast(self.instructions[instr_idx + 1]);
                    jump_pos <<= 8;
                    jump_pos |= @intCast(self.instructions[instr_idx + 2]);
                    instr_idx += 2;

                    const obj: object.Object = self.pop();
                    if (!mem.eql(u8, obj.typ(), object.BOOL)) {
                        return RuntimeError.UnexpectedType;
                    }

                    if (!obj.equal(object.TRUE)) {
                        assert(jump_pos <= self.instructions.len);
                        instr_idx = jump_pos - 1;
                    }
                },
                .opJump => {
                    var jump_pos: u16 = @intCast(self.instructions[instr_idx + 1]);
                    jump_pos <<= 8;
                    jump_pos |= @intCast(self.instructions[instr_idx + 2]);

                    assert(jump_pos <= self.instructions.len);
                    instr_idx = jump_pos - 1;
                },
                .opSetGlobal => {
                    var ident_idx: u16 = @intCast(self.instructions[instr_idx + 1]);
                    ident_idx <<= 8;
                    ident_idx |= @intCast(self.instructions[instr_idx + 2]);
                    instr_idx += 2;

                    self.globals[ident_idx] = self.pop();
                },
                .opGetGlobal => {
                    var ident_idx: u16 = @intCast(self.instructions[instr_idx + 1]);
                    ident_idx <<= 8;
                    ident_idx |= @intCast(self.instructions[instr_idx + 2]);
                    instr_idx += 2;

                    try self.push(self.globals[ident_idx]);
                },
                .opNull => {
                    try self.push(object.Object{ .null = .{} });
                },
            }
        }
    }

    fn pop(self: *VM) object.Object {
        assert(self.sp >= 0);
        if (self.sp == 0) {
            return object.Object{ .null = .{} };
        }

        const p = self.stack[self.sp - 1];
        self.sp -= 1;

        return p;
    }

    fn push(self: *VM, obj: object.Object) RuntimeError!void {
        if (self.sp >= stack_size) {
            return RuntimeError.StackOverflow;
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
    }

    fn stack_top(self: *VM) object.Object {
        assert(self.sp > 0);

        return self.stack[self.sp - 1];
    }

    pub fn last_popped(self: *VM) object.Object {
        return self.stack[self.sp];
    }
};

pub fn New(b: compiler.ByteCode, allocator: mem.Allocator) !*VM {
    const vm = try allocator.create(VM);
    vm.* = VM{
        .constants = b.constants,
        .allocator = allocator,
        .instructions = b.instructions,
        .stack = undefined,
        .globals = undefined,
        .sp = 0,
    };

    return vm;
}

pub fn NewWithState(b: compiler.ByteCode, allocator: mem.Allocator, s: [stack_size]object.Object, g: [stack_size]object.Object) !*VM {
    const vm = try New(b, allocator);
    vm.stack = s;
    vm.globals = g;

    return vm;
}

test "virtual machine boolean expressions run" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedBoolean: bool,
    }{
        .{
            .input = "false;",
            .expectedBoolean = false,
        },
        .{
            .input = "true",
            .expectedBoolean = true,
        },
        .{
            .input = "!true",
            .expectedBoolean = false,
        },
        .{
            .input = "!false",
            .expectedBoolean = true,
        },
        .{
            .input = "!!false",
            .expectedBoolean = false,
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        const expected = object.Object{
            .boolean = object.Boolean{
                .value = t.expectedBoolean,
            },
        };

        try testing.expectEqual(expected, vm.last_popped());
    }
}

test "virtual machine boolean results" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedBool: bool,
    }{
        .{
            .input = "5 == 5",
            .expectedBool = true,
        },
        .{
            .input = "5 != 5",
            .expectedBool = false,
        },
        .{
            .input = "true != true",
            .expectedBool = false,
        },
        .{
            .input = "false  == false",
            .expectedBool = true,
        },
        .{
            .input = "5 > 5",
            .expectedBool = false,
        },
        .{
            .input = "5 < 5",
            .expectedBool = false,
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        const expected = object.Object{
            .boolean = object.Boolean{
                .value = t.expectedBool,
            },
        };

        try testing.expectEqual(expected, vm.last_popped());
    }
}

test "virtual machine integer expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedInt: i64,
    }{
        .{
            .input = "1;",
            .expectedInt = 1,
        },
        .{
            .input = "-1;",
            .expectedInt = -1,
        },
        .{
            .input = "2;",
            .expectedInt = 2,
        },
        .{
            .input = "2+3;",
            .expectedInt = 5,
        },
        .{
            .input = "2-3;",
            .expectedInt = -1,
        },
        .{
            .input = "2*3;",
            .expectedInt = 6,
        },
        .{
            .input = "6/2;",
            .expectedInt = 3,
        },
        .{
            .input = "50 / 2 * 2 + 10 - 5",
            .expectedInt = 55,
        },
        .{
            .input = "50 / 2 * 2 + 10 - 5 - -1",
            .expectedInt = 56,
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        const expected = object.Object{
            .integer = object.Integer{
                .value = t.expectedInt,
            },
        };

        try testing.expectEqual(expected, vm.last_popped());
    }
}

// Todo: this test is failing, fix
test "virtual machine string expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedString: []const u8,
    }{
        .{
            .input = "'hello';",
            .expectedString = "hello",
        },
        .{
            .input = "'hello' + ' world';",
            .expectedString = "hello world",
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        const last_popped = vm.last_popped();
        try testing.expectEqualStrings(object.STRING, last_popped.typ());
        try testing.expectEqualStrings(t.expectedString, last_popped.string.value);
    }
}

test "virtual machine if expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedObj: object.Object,
    }{
        .{
            .input = "if(true) { 10; }",
            .expectedObj = object.Object{ .integer = .{ .value = 10 } },
        },
        .{
            .input = "if(!false) { 10; }",
            .expectedObj = object.Object{ .integer = .{ .value = 10 } },
        },
        .{
            .input = "if(true) { 10; } 100;",
            .expectedObj = object.Object{ .integer = .{ .value = 100 } },
        },
        .{
            .input = "if(false) { 10; }",
            .expectedObj = object.Object{ .null = .{} },
        },
        .{
            .input = "if(if(10 > 5) { true; }) { 10; }",
            .expectedObj = object.Object{ .integer = .{ .value = 10 } },
        },
        .{
            .input = "if(false) { 10; } else { 20; }",
            .expectedObj = object.Object{ .integer = .{ .value = 20 } },
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        try testing.expectEqual(t.expectedObj, vm.last_popped());
    }
}

test "virtual machine let statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedObj: object.Object,
    }{
        .{
            .input = "let one = 1; one + 2",
            .expectedObj = object.Object{ .integer = .{ .value = 3 } },
        },
        .{
            .input = "let one = 1; let two = one + one; one + two",
            .expectedObj = object.Object{ .integer = .{ .value = 3 } },
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);
        try vm.run();

        try testing.expectEqual(t.expectedObj, vm.last_popped());
    }
}
