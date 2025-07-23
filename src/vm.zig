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
const max_frames = @import("machine.zig").max_frames;

const RuntimeError = error{
    StackOverflow,
    StackUnderflow,
    DivideByZero,
    IncompatibleTypes,
    IncompatibleOperator,
    UnexpectedType,
    ArrayIndexOutOfBound,
    Oom,
};

const Frame = struct {
    ip: i64,
    instr_obj: object.FnInstrs,
    base_pointer: u32,

    fn instrs(self: *Frame) []const u8 {
        return self.instr_obj.value;
    }
};

fn new_frame(fn_instrs: object.FnInstrs, base_pointer: u32, args_count: u8) Frame {
    const frame = Frame{
        .ip = 0,
        .instr_obj = fn_instrs,
        .base_pointer = base_pointer - args_count,
    };

    return frame;
}

const VM = struct {
    constants: []object.Object,
    allocator: mem.Allocator,
    frames: [max_frames]Frame,
    frame_idx: u64,
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
        while (self.current_frame().ip < self.current_frame().instrs().len) : (self.current_frame().ip += 1) {
            assert(self.current_frame().ip >= 0);

            const frame = self.current_frame();
            const ip: u64 = @intCast(frame.ip);
            const instrs = frame.instrs();
            const instr = instrs[ip];

            const opcode: code.Opcode = @enumFromInt(instr);
            switch (opcode) {
                .opConstant => {
                    assert(instrs[ip + 1] <= std.math.maxInt(u16));
                    assert(instrs[ip + 2] <= std.math.maxInt(u16));

                    var const_idx: u16 = @intCast(instrs[ip + 1]);
                    const_idx <<= 8;
                    const_idx |= @intCast(instrs[ip + 2]);
                    frame.ip += 2;

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
                    var jump_pos: u16 = @intCast(instrs[ip + 1]);
                    jump_pos <<= 8;
                    jump_pos |= @intCast(instrs[ip + 2]);
                    frame.ip += 2;

                    const obj: object.Object = self.pop();
                    if (!mem.eql(u8, obj.typ(), object.BOOL)) {
                        return RuntimeError.UnexpectedType;
                    }

                    if (!obj.equal(object.TRUE)) {
                        assert(jump_pos <= instrs.len);
                        frame.ip = jump_pos - 1;
                    }
                },
                .opJump => {
                    var jump_pos: u16 = @intCast(instrs[ip + 1]);
                    jump_pos <<= 8;
                    jump_pos |= @intCast(instrs[ip + 2]);

                    assert(jump_pos <= instrs.len);
                    frame.ip = jump_pos - 1;
                },
                .opSetGlobal => {
                    assert(instrs[ip + 1] <= std.math.maxInt(u16));
                    assert(instrs[ip + 2] <= std.math.maxInt(u16));

                    var ident_idx: u16 = @intCast(instrs[ip + 1]);
                    ident_idx <<= 8;
                    ident_idx |= @intCast(instrs[ip + 2]);
                    frame.ip += 2;

                    self.globals[ident_idx] = self.pop();
                },
                .opGetGlobal => {
                    assert(instrs[ip + 1] <= std.math.maxInt(u16));
                    assert(instrs[ip + 2] <= std.math.maxInt(u16));

                    var ident_idx: u16 = @intCast(instrs[ip + 1]);
                    ident_idx <<= 8;
                    ident_idx |= @intCast(instrs[ip + 2]);
                    frame.ip += 2;

                    try self.push(self.globals[ident_idx]);
                },
                .opSetLocal => {
                    assert(instrs[ip + 1] <= std.math.maxInt(u8));

                    const ident_idx: u8 = @intCast(instrs[ip + 1]);
                    frame.ip += 1;

                    self.stack[frame.base_pointer + ident_idx] = self.pop();
                },
                .opGetLocal => {
                    assert(instrs[ip + 1] <= std.math.maxInt(u8));

                    const ident_idx: u8 = @intCast(instrs[ip + 1]);
                    frame.ip += 1;

                    try self.push(self.stack[frame.base_pointer + ident_idx]);
                },
                .opArray => {
                    var array_len: u16 = @intCast(instrs[ip + 1]);
                    array_len <<= 8;
                    array_len |= @intCast(instrs[ip + 2]);
                    frame.ip += 2;

                    const array = self.allocator.create(object.Array) catch unreachable;
                    array.* = object.Array{
                        .value = &.{},
                    };

                    if (array_len > 0) {
                        array.value = self.stack[self.sp - array_len .. self.sp];
                    }

                    try self.push(object.Object{ .array = array });
                },
                .opIndex => {
                    const idx = self.pop();
                    const arr = self.pop();
                    if (!mem.eql(u8, arr.typ(), object.ARRAY)) {
                        return RuntimeError.UnexpectedType;
                    }

                    // Todo: these could be compile time errors
                    if (!mem.eql(u8, idx.typ(), object.INT)) {
                        return RuntimeError.UnexpectedType;
                    }

                    if (idx.integer.value >= arr.array.value.len or idx.integer.value < 0) {
                        return RuntimeError.ArrayIndexOutOfBound;
                    }

                    try self.push(arr.array.value[@intCast(idx.integer.value)]);
                },
                .opCall => {
                    // Todo: check if argument count is as expected
                    // Update fn object
                    // Post update, remove args_count from new_frame
                    assert(instrs[ip + 1] <= std.math.maxInt(u8));

                    const args_count: u8 = @intCast(instrs[ip + 1]);
                    frame.ip += 1;

                    assert(self.sp >= args_count + 1);
                    var obj = self.stack[self.sp - 1 - args_count];
                    assert(mem.eql(u8, obj.typ(), object.FN_INSTR));

                    self.add_frame(obj.fn_instrs, args_count);
                    self.current_frame().ip = -1;

                    self.sp += obj.fn_instrs.symbol_count + args_count;
                },
                .opReturnValue => {
                    const prev_frame = self.remove_frame();

                    const return_val = self.pop();
                    self.sp = prev_frame.base_pointer;

                    const obj = self.pop();
                    assert(mem.eql(u8, obj.typ(), object.FN_INSTR));

                    try self.push(return_val);
                },
                .opReturn => {
                    const prev_frame = self.remove_frame();
                    self.sp = prev_frame.base_pointer;

                    const obj = self.pop();
                    assert(mem.eql(u8, obj.typ(), object.FN_INSTR));

                    // implicit returns always return void(null)
                    try self.push(object.Object{ .null = .{} });
                },
                .opNull => {
                    try self.push(object.Object{ .null = .{} });
                },
            }
        }
    }

    fn current_frame(self: *VM) *Frame {
        return &self.frames[self.frame_idx];
    }

    fn add_frame(self: *VM, fn_instrs: object.FnInstrs, args_count: u8) void {
        self.frame_idx += 1;
        self.frames[self.frame_idx] = new_frame(fn_instrs, self.sp, args_count);
    }

    fn remove_frame(self: *VM) *Frame {
        assert(self.frame_idx >= 1);
        self.frame_idx -= 1;

        return &self.frames[self.frame_idx + 1];
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
        .frames = undefined,
        .frame_idx = 0,
        .stack = undefined,
        .globals = undefined,
        .sp = 0,
    };

    const program_instrs = object.FnInstrs{ .value = b.instructions, .symbol_count = 0 };
    vm.frames[0] = new_frame(program_instrs, vm.sp, 0);
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
        .{
            .input = "[2*2][0]",
            .expectedInt = 4,
        },
        .{
            .input = "[0, 1, 2][2]",
            .expectedInt = 2,
        },
        .{
            .input = "[2*2, 1, 2*2][1+1]",
            .expectedInt = 4,
        },
        .{
            .input = "[[1, 1, 1]][0][0]",
            .expectedInt = 1,
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

test "virtual machine array expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedInts: []const i64,
    }{
        .{
            .input = "[];",
            .expectedInts = &.{},
        },
        .{
            .input = "[1];",
            .expectedInts = &.{1},
        },
        .{
            .input = "[1, 2, 3];",
            .expectedInts = &.{ 1, 2, 3 },
        },
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6, 6 * 6]",
            .expectedInts = &.{ 3, -1, 30, 36 },
        },
        .{
            .input = "[2*2, 2*2, 3*3];",
            .expectedInts = &.{ 4, 4, 9 },
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

        var expectedInts: [10]object.Object = undefined;
        for (t.expectedInts, 0..) |int, i| {
            expectedInts[i] = object.Object{ .integer = .{ .value = int } };
        }

        const last_popped = vm.last_popped();
        try testing.expectEqualStrings(object.ARRAY, last_popped.typ());
        try testing.expectEqualSlices(object.Object, expectedInts[0..t.expectedInts.len], @constCast(last_popped.array.*.value));
    }
}

test "virtual machine fn calls" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedObj: object.Object,
    }{
        .{
            .input = "let x = fn() { return 5; }; x();",
            .expectedObj = object.Object{ .integer = .{ .value = 5 } },
        },
        .{
            .input = "let x = fn() { return 10 + 5; }; x();",
            .expectedObj = object.Object{ .integer = .{ .value = 15 } },
        },
        .{
            .input =
            \\ let one = fn() { return 1; };
            \\ let two = fn() { return 2; };
            \\ one() + two()
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 3 } },
        },
        .{
            .input =
            \\ let one = fn() { return 1; };
            \\ let two = fn() { return 2; };
            \\ one() + two()
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 3 } },
        },
        .{
            .input =
            \\ let earlyExit = fn() { return 99; 100; };
            \\ earlyExit();
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 99 } },
        },
        .{
            .input =
            \\ let noReturn = fn() { return; };
            \\ noReturn();
            ,
            .expectedObj = object.Object{ .null = .{} },
        },
        .{
            .input =
            \\ let noReturn = fn() {};
            \\ noReturn();
            ,
            .expectedObj = object.Object{ .null = .{} },
        },
        .{
            .input =
            \\ let returnsOne = fn() { return 1; };
            \\ let returnsOneReturner = fn() { return returnsOne; };
            \\ returnsOneReturner()();
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 1 } },
        },
        .{
            .input =
            \\ let oneAndTwo = fn() { let one = 1; let two = 2; return one + two; };
            \\ let threeAndFour = fn() { let three = 3; let four = 4; return three + four; };
            \\ oneAndTwo() + threeAndFour();
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 10 } },
        },
        .{
            .input =
            \\ let firstFoobar = fn() { let foobar = 50; return foobar; };
            \\ let secondFoobar = fn() { let foobar = 100; return foobar; };
            \\ firstFoobar() + secondFoobar();
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 150 } },
        },
        .{
            .input =
            \\ let globalSeed = 50;
            \\
            \\ let minusOne = fn() {
            \\  let num = 1;
            \\  return globalSeed - num;
            \\ }
            \\
            \\ let minusTwo = fn() {
            \\  let num = 2;
            \\  return globalSeed - num;
            \\ }
            \\
            \\ minusOne() + minusTwo();
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 97 } },
        },
        .{
            .input =
            \\  let sum = fn(a, b) { return a + b; };
            \\  sum(1, 2);
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 3 } },
        },
        .{
            .input =
            \\ let globalNum = 10;
            \\
            \\ let sum = fn(a, b) {
            \\  let c = a + b;
            \\  return c + globalNum;
            \\ };
            \\
            \\ let outer = fn() {
            \\  return sum(1, 2) + sum(3, 4) + globalNum;
            \\ };
            \\
            \\ outer() + globalNum;
            ,
            .expectedObj = object.Object{ .integer = .{ .value = 50 } },
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
