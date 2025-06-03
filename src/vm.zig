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

const RuntimeError = error{
    StackOverflow,
    StackUnderflow,
    DivideByZero,
    IncompatibleTypes,
    IncompatibleOperator,
};

const stack_size = 2048;
const VM = struct {
    constants: []object.Object,
    instructions: []u8,
    stack: [stack_size]object.Object,
    sp: u32,

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
                .opAdd, .opSub, .opMul, .opDiv, .opGt, .opLt => {
                    assert(self.sp >= 2);

                    const obj1: object.Object = try self.pop();
                    const obj2: object.Object = try self.pop();

                    assert(mem.eql(u8, obj1.typ(), object.INT));
                    assert(mem.eql(u8, obj2.typ(), object.INT));

                    const right: object.Integer = obj1.integer;
                    const left: object.Integer = obj2.integer;

                    switch (opcode) {
                        .opAdd => {
                            const result = left.value + right.value;
                            try self.push(object.Object{ .integer = .{ .value = result } });
                        },
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

                    const obj1: object.Object = try self.pop();
                    const obj2: object.Object = try self.pop();
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

                    const obj1: object.Object = try self.pop();
                    if (!mem.eql(u8, obj1.typ(), object.BOOL)) {
                        return RuntimeError.IncompatibleOperator;
                    }

                    const right = obj1.boolean.value;
                    try self.push(object.Object{ .boolean = .{ .value = !right } });
                },
                .opMinus => {
                    assert(self.sp >= 1);

                    const obj1: object.Object = try self.pop();
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
                    _ = try self.pop();
                },
            }
        }
    }

    fn pop(self: *VM) RuntimeError!object.Object {
        if (self.sp == 0) {
            return RuntimeError.StackUnderflow;
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
        if (self.sp == 0) {
            return object.Object{ .null = object.Null{} };
        }

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
        .instructions = b.instructions,
        .stack = undefined,
        .sp = 0,
    };

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
test "virtual machine arithmetic operations" {
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
