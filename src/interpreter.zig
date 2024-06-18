const std = @import("std");

const Expr = @import("parser.zig").Expr;
const ET = @import("parser.zig").ExprType;
const Binary = @import("parser.zig").Binary;
const Unary = @import("parser.zig").Unary;
const Literal = @import("parser.zig").Literal;
const Grouping = @import("parser.zig").Grouping;
const Result = @import("parser.zig").Result;
const Error = @import("parser.zig").Error;
const Value = @import("token.zig").Value;
const Token = @import("token.zig").Token;
const TT = @import("token.zig").TokenType;

pub const Interpreter = struct {
    expr: Expr,

    pub fn init(expr: Expr) Interpreter {
        return Interpreter{
            .expr = expr,
        };
    }

    pub fn interpret(self: Interpreter) !Value {
        const result = try self.expr.evaluate(self);
        
        switch (result) {
            .ok => |val| std.log.info("{s}", .{val}),
            .err => |err| std.debug.print("Error unwrapping the result: {s}", .{err}),
        }

        return result.unwrap();
    }
};

const InterpreterError = error {
    NotALiteral,
    UnexpectedLiteralType,
};

test "Interpreter init" {
    const expr = Expr { .Literal = Literal{ .value = Value{ .Bool = true }}};
    const interp = Interpreter.init(expr);

    try std.testing.expect(@TypeOf(interp) == Interpreter);
}

test "interpret addition" {
    const plus = Token.init(TT.PLUS, "+", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    const expr = Expr.initBinary(&one, plus, &one);
    const interp = Interpreter.init(expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 2);
}

test "interpret subtraction" {
    const minus = Token.init(TT.MINUS, "-", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    const expr = Expr.initBinary(&one, minus, &one);
    const interp = Interpreter.init(expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 0);
}

test "interpret multiplication" {
    const star = Token.init(TT.STAR, "*", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    const expr = Expr.initBinary(&four, star, &four);
    const interp = Interpreter.init(expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 16);
}

test "interpret division" {
    const slash = Token.init(TT.SLASH, "/", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    const expr = Expr.initBinary(&four, slash, &four);
    const interp = Interpreter.init(expr);

    try std.testing.expect(try (try interp.interpret()).asNumber() == 1);
}

test "interpret equality operators" {
    const gt = Token.init(TT.GREATER, ">", null, 1);
    const gte = Token.init(TT.GREATER_EQUAL, ">=", null, 1);
    const lt = Token.init(TT.LESS, "<", null, 1);
    const lte = Token.init(TT.LESS_EQUAL, "<=", null, 1);
    const ne = Token.init(TT.BANG_EQUAL, "!=", null, 1);
    const eq = Token.init(TT.EQUAL_EQUAL, "==", null, 1);

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

    try std.testing.expect(try (try Interpreter.init(oneGtOne).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(oneGtTwo).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(twoGtOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneGteOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneGteTwo).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(twoGteOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneLtOne).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(oneLtTwo).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(twoLtOne).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(oneLteOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneLteTwo).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(twoLteOne).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(oneNeOne).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(oneNeTwo).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(twoNeOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneEqOne).interpret()).asBool() == true);
    try std.testing.expect(try (try Interpreter.init(oneEqTwo).interpret()).asBool() == false);
    try std.testing.expect(try (try Interpreter.init(twoEqOne).interpret()).asBool() == false);
}

test "interpret string concat" {
    const concat = Token.init(TT.PLUS, "+", null, 1);
    const one = .{ .Literal = .{ .value = .{ .String = "one"}}};
    const two = .{ .Literal = .{ .value = .{ .String = "two"}}};
    const expr = Expr.initBinary(&one, concat, &two);
    const interp = Interpreter.init(expr);

    try std.testing.expect(std.mem.eql(u8, try (try interp.interpret()).asString(), "onetwo"));
}
