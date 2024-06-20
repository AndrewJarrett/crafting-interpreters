const std = @import("std");

const Expr = @import("parser.zig").Expr;
const ET = @import("parser.zig").ExprType;
const Binary = @import("parser.zig").Binary;
const Unary = @import("parser.zig").Unary;
const Literal = @import("parser.zig").Literal;
const Grouping = @import("parser.zig").Grouping;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;
const Value = @import("token.zig").Value;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const Allocator = std.mem.Allocator;

pub const Interpreter = struct {
    expr: Expr,
    allocator: Allocator,
    result: ?Value,

    pub fn init(allocator: Allocator, expr: Expr) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .expr = expr,
            .result = null,
        };
    }

    pub fn interpret(self: *Interpreter) !Value {
        const result = try self.expr.evaluate(self);
        
        switch (result) {
            .ok => |val| std.log.info("Value: {s}", .{val}),
            .err => |err| Lexer.handleRuntimeError(err.token.?, err.message),
        }

        // Make sure we remove any prior result and free strings with heap allocations
        self.deinit();
        self.result = result.unwrap() catch null;

        return self.result orelse return InterpreterError.InterpreterError;
    }

    pub fn deinit(self: *Interpreter) void {
        if (self.result) |res| {
            switch (res) {
                .String => |s| self.allocator.free(s),
                else => {}
            }
            self.result = null;
        }
    }
};

pub const InterpreterError = error {
    InterpreterError,
};

test "Interpreter init" {
    const expr = Expr { .Literal = Literal{ .value = Value{ .Bool = true }}};
    var interp = Interpreter.init(std.testing.allocator, expr);
    defer interp.deinit();

    try std.testing.expect(@TypeOf(interp) == Interpreter);
}

test "interpret addition" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    const expr = Expr.initBinary(&one, plus, &one);
    var interp = Interpreter.init(std.testing.allocator, expr);
    defer interp.deinit();

    try std.testing.expect(try (try interp.interpret()).asNumber() == 2);
}

test "interpret subtraction" {
    const minus = Token.init(.MINUS, "-", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    const expr = Expr.initBinary(&one, minus, &one);
    var interp = Interpreter.init(std.testing.allocator, expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 0);
}

test "interpret multiplication" {
    const star = Token.init(.STAR, "*", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    const expr = Expr.initBinary(&four, star, &four);
    var interp = Interpreter.init(std.testing.allocator, expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 16);
}

test "interpret division" {
    const slash = Token.init(.SLASH, "/", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    const expr = Expr.initBinary(&four, slash, &four);
    var interp = Interpreter.init(std.testing.allocator, expr);
    defer interp.deinit();

    try std.testing.expect(try (try interp.interpret()).asNumber() == 1);
}

test "interpret equality operators" {
    const gt = Token.init(.GREATER, ">", null, 1);
    const gte = Token.init(.GREATER_EQUAL, ">=", null, 1);
    const lt = Token.init(.LESS, "<", null, 1);
    const lte = Token.init(.LESS_EQUAL, "<=", null, 1);
    const ne = Token.init(.BANG_EQUAL, "!=", null, 1);
    const eq = Token.init(.EQUAL_EQUAL, "==", null, 1);

    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    var two = Expr{ .Literal = Literal{ .value = Value{ .Number = 2}}};
    const oneGtOne = Expr.initBinary(&one, gt, &one);
    const oneGtTwo = Expr.initBinary(&one, gt, &two);
    const twoGtOne = Expr.initBinary(&two, gt, &one);
    const oneGteOne = Expr.initBinary(&one, gte, &one);
    const oneGteTwo = Expr.initBinary(&one, gte, &two);
    const twoGteOne = Expr.initBinary(&two, gte, &one);
    const oneLtOne = Expr.initBinary(&one, lt, &one);
    const oneLtTwo = Expr.initBinary(&one, lt, &two);
    const twoLtOne = Expr.initBinary(&two, lt, &one);
    const oneLteOne = Expr.initBinary(&one, lte, &one);
    const oneLteTwo = Expr.initBinary(&one, lte, &two);
    const twoLteOne = Expr.initBinary(&two, lte, &one);
    const oneNeOne = Expr.initBinary(&one, ne, &one);
    const oneNeTwo = Expr.initBinary(&one, ne, &two);
    const twoNeOne = Expr.initBinary(&two, ne, &one);
    const oneEqOne = Expr.initBinary(&one, eq, &one);
    const oneEqTwo = Expr.initBinary(&one, eq, &two);
    const twoEqOne = Expr.initBinary(&two, eq, &one);

    const alloc = std.testing.allocator;
    var interp = Interpreter.init(alloc, oneGtOne);
    defer interp.deinit();
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, oneGtTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, twoGtOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneGteOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneGteTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, twoGteOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneLtOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, oneLtTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, twoLtOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, oneLteOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneLteTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, twoLteOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, oneNeOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, oneNeTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, twoNeOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneEqOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(alloc, oneEqTwo);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(alloc, twoEqOne);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);
}

test "interpret string concat" {
    const concat = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .String = "one"}}};
    var two = Expr{ .Literal = .{ .value = .{ .String = "two"}}};
    const expr = Expr.initBinary(&one, concat, &two);
    var interp = Interpreter.init(std.testing.allocator, expr);
    defer interp.deinit();

    try std.testing.expect(std.mem.eql(u8, try (try interp.interpret()).asString(), "onetwo"));
}

test "interpret binary error" {
    const badToken = Token.init(.FOR, "for", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var all = Expr{ .Literal = .{ .value = .{ .String = "all" } } };
    const oneForAll = Expr.initBinary(&one, badToken, &all);
    var interp = Interpreter.init(std.testing.allocator, oneForAll);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret adding error" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var trueBool = Expr{ .Literal = .{ .value = .{ .Bool = true } } };
    const truePlusTrue = Expr.initBinary(&trueBool, plus, &trueBool);
    var interp = Interpreter.init(std.testing.allocator, truePlusTrue);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret number error" {
    const minus = Token.init(.MINUS, "-", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1 } } };
    var oneStr = Expr{ .Literal = .{ .value = .{ .String = "one" } } };
    const oneMinusOneStr = Expr.initBinary(&one, minus, &oneStr);
    var interp = Interpreter.init(std.testing.allocator, oneMinusOneStr);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret unary operations" {
    const bang = Token.init(.BANG, "!", null, 1);
    const neg = Token.init(.MINUS, "-", null, 1);
    var num = Expr{ .Literal = .{ .value = .{ .Number = 1 } } };
    var falseBool = Expr{ .Literal = .{ .value = .{ .Bool = false } } };
    var trueBool = Expr{ .Literal = .{ .value = .{ .Bool = true } } };
    var nil = Expr{ .Literal = .{ .value = .{ .Nil = {} } } };
    var string = Expr{ .Literal = .{ .value = .{ .String = "a string is considered truthy" } } };

    const notNum = Expr.initUnary(bang, &num);
    const notFalse = Expr.initUnary(bang, &falseBool);
    const notTrue = Expr.initUnary(bang, &trueBool);
    const notNil = Expr.initUnary(bang, &nil);
    const notString = Expr.initUnary(bang, &string);
    const negOne = Expr.initUnary(neg, &num);
    const negString = Expr.initUnary(neg, &string);

    var interp = Interpreter.init(std.testing.allocator, notNum);
    defer interp.deinit();

    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(std.testing.allocator, notFalse);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(std.testing.allocator, notTrue);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(std.testing.allocator, notNil);
    try std.testing.expect(try (try interp.interpret()).asBool() == true);

    interp = Interpreter.init(std.testing.allocator, notString);
    try std.testing.expect(try (try interp.interpret()).asBool() == false);

    interp = Interpreter.init(std.testing.allocator, negOne);
    try std.testing.expect(try (try interp.interpret()).asNumber() == -1);

    interp = Interpreter.init(std.testing.allocator, negString);
    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret grouping" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var onePlusOne = Expr.initBinary(&one, plus, &one);
    const group = Expr.initGrouping(&onePlusOne);
    var interp = Interpreter.init(std.testing.allocator, group);
    defer interp.deinit();

    try std.testing.expect(try (try interp.interpret()).asNumber() == 2);
}

test "interpret literal" {
    const one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var interp = Interpreter.init(std.testing.allocator, one);
    defer interp.deinit();

    try std.testing.expect(try (try interp.interpret()).asNumber() == 1);
}
