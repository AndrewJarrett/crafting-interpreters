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
const HeapValue = @import("token.zig").HeapValue;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Stmt = @import("stmt.zig").Stmt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Interpreter = struct {
    allocator: Allocator,
    statements: ArrayList(Stmt),
    values: ArrayList(*HeapValue),

    pub fn init(allocator: Allocator, statements: ArrayList(Stmt)) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .statements = statements,
            .values = ArrayList(*HeapValue).init(allocator),
        };
    }

    pub fn interpret(self: *Interpreter) !void {
        for (self.statements.items) |stmt| {
            const result = try stmt.evaluate(self);

            switch (result) {
                .ok => |val| std.log.info("Value: {s}", .{val}),
                .err => |err| Lexer.handleRuntimeError(err.token.?, err.message),
            }

            const value = result.unwrap() catch return InterpreterError.InterpreterError;
            try self.values.append(value);
        }
    }

    pub fn deinit(self: *Interpreter) void {
        while (self.values.popOrNull()) |val| {
            val.deinit();
        }
        self.values.deinit();

        while (self.statements.popOrNull()) |stmt| {
            stmt.deinit(self.statements.allocator);
        }
        self.statements.deinit();
    }
};

pub const InterpreterError = error {
    InterpreterError,
};

test "Interpreter init" {
    const expr = Expr.initLiteral(std.testing.allocator, true);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expect(@TypeOf(interp) == Interpreter);
}

test "interpret addition" {
    const plus = Token.init(std.testing.allocator, .PLUS, "+", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    const one2 = Expr.initLiteral(std.testing.allocator, 1);
    const expr = Expr.initBinary(std.testing.allocator, one, plus, one2);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 2);
}

test "interpret subtraction" {
    const minus = Token.init(std.testing.allocator, .MINUS, "-", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    const one2 = Expr.initLiteral(std.testing.allocator, 1);
    const expr = Expr.initBinary(std.testing.allocator, one, minus, one2);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 0);
}

test "interpret multiplication" {
    const star = Token.init(std.testing.allocator, .STAR, "*", null, 1);
    const four = Expr.initLiteral(std.testing.allocator, 4);
    const four2 = Expr.initLiteral(std.testing.allocator, 4);
    const expr = Expr.initBinary(std.testing.allocator, four, star, four2);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));
    
    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 16);
}

test "interpret division" {
    const slash = Token.init(std.testing.allocator, .SLASH, "/", null, 1);
    const four = Expr.initLiteral(std.testing.allocator, 4);
    const four2 = Expr.initLiteral(std.testing.allocator, 4);
    const expr = Expr.initBinary(std.testing.allocator, four, slash, four2);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 1);
}

test "interpret equality operators" {
    const gt = Token.init(std.testing.allocator, .GREATER, ">", null, 1);
    //const gte = Token.init(std.testing.allocator, .GREATER_EQUAL, ">=", null, 1);
    //const lt = Token.init(std.testing.allocator, .LESS, "<", null, 1);
    //const lte = Token.init(std.testing.allocator, .LESS_EQUAL, "<=", null, 1);
    //const ne = Token.init(std.testing.allocator, .BANG_EQUAL, "!=", null, 1);
    //const eq = Token.init(std.testing.allocator, .EQUAL_EQUAL, "==", null, 1);

    const alloc = std.testing.allocator;
    var stmts = ArrayList(Stmt).init(alloc);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    //var two = Expr.initLiteral(HeapValue.init(std.testing.allocator, 2));

    //const newOne = copy(one);
    const newNewOne = Expr.initLiteral(std.testing.allocator, 1);
    const oneGtOne = Expr.initBinary(std.testing.allocator, one, gt, newNewOne);
    //var oneGtTwo = Expr.initBinary(copy(one), gt, &two);
    //var twoGtOne = Expr.initBinary(copy(two), gt, copy(one));
    //var oneGteOne = Expr.initBinary(copy(one), gte, copy(one));
    //var oneGteTwo = Expr.initBinary(copy(one), gte, copy(two));
    //var twoGteOne = Expr.initBinary(copy(two), gte, copy(one));
    //var oneLtOne = Expr.initBinary(copy(one), lt, copy(one));
    //var oneLtTwo = Expr.initBinary(copy(one), lt, copy(two));
    //var twoLtOne = Expr.initBinary(copy(two), lt, copy(one));
    //var oneLteOne = Expr.initBinary(copy(one), lte, copy(one));
    //var oneLteTwo = Expr.initBinary(copy(one), lte, copy(two));
    //var twoLteOne = Expr.initBinary(copy(two), lte, copy(one));
    //var oneNeOne = Expr.initBinary(copy(one), ne, copy(one));
    //var oneNeTwo = Expr.initBinary(copy(one), ne, copy(two));
    //var twoNeOne = Expr.initBinary(copy(two), ne, copy(one));
    //var oneEqOne = Expr.initBinary(copy(one), eq, copy(one));
    //var oneEqTwo = Expr.initBinary(copy(one), eq, copy(two));
    //var twoEqOne = Expr.initBinary(copy(two), eq, copy(one));

    try stmts.append(Stmt.expression(oneGtOne));
    //try stmts.append(Stmt.expression(&oneGtTwo));
    //try stmts.append(Stmt.expression(&twoGtOne));
    //try stmts.append(Stmt.expression(&oneGteOne));
    //try stmts.append(Stmt.expression(&oneGteTwo));
    //try stmts.append(Stmt.expression(&twoGteOne));
    //try stmts.append(Stmt.expression(&oneLtOne));
    //try stmts.append(Stmt.expression(&oneLtTwo));
    //try stmts.append(Stmt.expression(&twoLtOne));
    //try stmts.append(Stmt.expression(&oneLteOne));
    //try stmts.append(Stmt.expression(&oneLteTwo));
    //try stmts.append(Stmt.expression(&twoLteOne));
    //try stmts.append(Stmt.expression(&oneNeOne));
    //try stmts.append(Stmt.expression(&oneNeTwo));
    //try stmts.append(Stmt.expression(&twoNeOne));
    //try stmts.append(Stmt.expression(&oneEqOne));
    //try stmts.append(Stmt.expression(&oneEqTwo));
    //try stmts.append(Stmt.expression(&twoEqOne));

    var interp = Interpreter.init(alloc, stmts);
    defer interp.deinit();
    try interp.interpret();

    // Test in opposite order (popping values off of a stack)
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 2 == 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 1 == 2
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 == 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 2 != 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 != 2
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 1 != 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 2 <= 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 <= 2
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 <= 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 2 < 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 < 2
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 1 < 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 2 >= 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 1 >= 2
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 1 >= 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == true); // 2 > 1
    //try std.testing.expect(try interp.values.pop().value.asBool() == false); // 1 > 2
    try std.testing.expect(try interp.values.getLast().value.asBool() == false); // 1 > 1
}

test "interpret string concat" {
    const concat = Token.init(std.testing.allocator, .PLUS, "+", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, "one");
    const two = Expr.initLiteral(std.testing.allocator, "two");
    const expr = Expr.initBinary(std.testing.allocator, one, concat, two);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(std.mem.eql(u8, try interp.values.getLast().value.asString(), "onetwo"));
}

test "interpret binary error" {
    const badToken = Token.init(std.testing.allocator, .FOR, "for", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    const all = Expr.initLiteral(std.testing.allocator, "all");
    const oneForAll = Expr.initBinary(std.testing.allocator, one, badToken, all);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(oneForAll));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret adding error" {
    const plus = Token.init(std.testing.allocator, .PLUS, "+", null, 1);
    const trueBool = Expr.initLiteral(std.testing.allocator, true);
    const trueBool2 = Expr.initLiteral(std.testing.allocator, true);
    const truePlusTrue = Expr.initBinary(std.testing.allocator, trueBool, plus, trueBool2);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(truePlusTrue));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret number error" {
    const minus = Token.init(std.testing.allocator, .MINUS, "-", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    const oneStr = Expr.initLiteral(std.testing.allocator, "one");
    const oneMinusOneStr = Expr.initBinary(std.testing.allocator, one, minus, oneStr);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(oneMinusOneStr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret unary operations" {
    const bang = Token.init(std.testing.allocator, .BANG, "!", null, 1);
    const neg = Token.init(std.testing.allocator, .MINUS, "-", null, 1);
    const num = Expr.initLiteral(std.testing.allocator, 1);
    const num2 = Expr.initLiteral(std.testing.allocator, 1);
    const falseBool = Expr.initLiteral(std.testing.allocator, false);
    const trueBool = Expr.initLiteral(std.testing.allocator, true);
    const nil = Expr.initLiteral(std.testing.allocator, {});
    const string = Expr.initLiteral(std.testing.allocator, "a string is considered truthy");

    const notNum = Expr.initUnary(std.testing.allocator, bang, num);
    const notFalse = Expr.initUnary(std.testing.allocator, bang, falseBool);
    const notTrue = Expr.initUnary(std.testing.allocator, bang, trueBool);
    const notNil = Expr.initUnary(std.testing.allocator, bang, nil);
    const notString = Expr.initUnary(std.testing.allocator, bang, string);
    const negNum = Expr.initUnary(std.testing.allocator, neg, num2);

    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(notNum));
    try stmts.append(Stmt.expression(notFalse));
    try stmts.append(Stmt.expression(notTrue));
    try stmts.append(Stmt.expression(notNil));
    try stmts.append(Stmt.expression(notString));
    try stmts.append(Stmt.expression(negNum));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    // Expected results in order
    const expected = .{ false, true, false, true, false, -1 };

    inline for (interp.values.items, 0..expected.len) |item, i| {
        const value = switch (@TypeOf(expected[i])) {
            bool => try item.value.asBool(),
            else => try item.value.asNumber(),
        };
        try std.testing.expect(value == expected[i]);
    }
}

test "interpret bad unary expression" {
    const neg = Token.init(std.testing.allocator, .MINUS, "-", null, 1);
    const string = Expr.initLiteral(std.testing.allocator, "a string is considered truthy");

    // Test error (-"string")
    const negString = Expr.initUnary(std.testing.allocator, neg, string);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(negString));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret grouping" {
    const plus = Token.init(std.testing.allocator, .PLUS, "+", null, 1);
    const one = Expr.initLiteral(std.testing.allocator, 1);
    const one2 = Expr.initLiteral(std.testing.allocator, 1);
    const onePlusOne = Expr.initBinary(std.testing.allocator, one, plus, one2);
    const group = Expr.initGrouping(std.testing.allocator, onePlusOne);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(group));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 2);
}

test "interpret literal" {
    const one = Expr.initLiteral(std.testing.allocator, 1);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(one));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.getLast().value.asNumber() == 1);
}

fn copy(expr: Expr) *Expr {
    //var newExpr = expr;
    var newExpr = copy: {
        break :copy switch (expr) {
            .Unary => |u| Expr{ .Unary = .{ .operator = u.operator, .right = copy(u.right.*) }},
            .Binary => |b| Expr{ .Binary = .{ .left = copy(b.left.*), .operator = b.operator, .right = copy(b.right.*) }},
            .Literal => |l| if (l.value) |val| {
                break :copy switch (val.value) {
                    .Bool => |b| Expr.initLiteral(std.testing.allocator, b),
                    .Number => |num| {
                        std.debug.print("\nnum: {d}", .{num});
                        break: copy Expr.initLiteral(std.testing.allocator, num);
                    },
                    .Nil => |nil| Expr.initLiteral(std.testing.allocator, nil),
                    .String => |s| Expr.initLiteral(std.testing.allocator, s),
                };
            } else {
                break :copy expr;
            },
            .Grouping => |g| Expr{ .Grouping = .{ .expression = copy(g.expression.*) }},
        };
    };
    return &newExpr;
}
