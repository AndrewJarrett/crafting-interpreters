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
const Stmt = @import("stmt.zig").Stmt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Interpreter = struct {
    allocator: Allocator,
    statements: ArrayList(Stmt),
    values: ArrayList(*Value),

    pub fn init(allocator: Allocator, statements: ArrayList(Stmt)) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .statements = statements,
            .values = ArrayList(*Value).init(allocator),
        };
    }

    pub fn interpret(self: *Interpreter) !void {
        for (self.statements.items) |stmt| {
            const result = try stmt.evaluate(self);

            switch (result) {
                .ok => |val| std.log.info("Value: {s}", .{val}),
                .err => |err| Lexer.handleRuntimeError(err.token.?, err.message),
            }

            var value = result.unwrap() catch return InterpreterError.InterpreterError;
            try self.values.append(&value);
        }
    }

    pub fn deinit(self: *Interpreter) void {
        self.values.deinit();
        self.statements.deinit();
    }
};

pub const InterpreterError = error {
    InterpreterError,
};

test "Interpreter init" {
    var expr = Expr { .Literal = Literal{ .value = Value{ .Bool = true }}};
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expect(@TypeOf(interp) == Interpreter);
}

test "interpret addition" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    var expr = Expr.initBinary(&one, plus, &one);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 2);
}

test "interpret subtraction" {
    const minus = Token.init(.MINUS, "-", null, 1);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    var expr = Expr.initBinary(&one, minus, &one);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 0);
}

test "interpret multiplication" {
    const star = Token.init(.STAR, "*", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    var expr = Expr.initBinary(&four, star, &four);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));
    
    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 16);
}

test "interpret division" {
    const slash = Token.init(.SLASH, "/", null, 1);
    var four = Expr{ .Literal = Literal{ .value = Value{ .Number = 4}}};
    var expr = Expr.initBinary(&four, slash, &four);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 1);
}

test "interpret equality operators" {
    const gt = Token.init(.GREATER, ">", null, 1);
    const gte = Token.init(.GREATER_EQUAL, ">=", null, 1);
    const lt = Token.init(.LESS, "<", null, 1);
    const lte = Token.init(.LESS_EQUAL, "<=", null, 1);
    const ne = Token.init(.BANG_EQUAL, "!=", null, 1);
    const eq = Token.init(.EQUAL_EQUAL, "==", null, 1);

    const alloc = std.testing.allocator;
    var stmts = ArrayList(Stmt).init(alloc);
    var one = Expr{ .Literal = Literal{ .value = Value{ .Number = 1}}};
    var two = Expr{ .Literal = Literal{ .value = Value{ .Number = 2}}};
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, gt, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, gt, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, gt, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, gte, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, gte, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, gte, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, lt, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, lt, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, lt, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, lte, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, lte, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, lte, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, ne, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, ne, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, ne, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, eq, &one))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&one, eq, &two))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initBinary(&two, eq, &one))));

    var interp = Interpreter.init(alloc, stmts);
    defer interp.deinit();
    try interp.interpret();

    // Test in opposite order (popping values off of a stack)
    try std.testing.expect(try interp.values.pop().asBool() == false); // 2 == 1
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 == 2
    try std.testing.expect(try interp.values.pop().asBool() == true); // 1 == 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 2 != 1
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 != 2
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 != 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 1 <= 2
    try std.testing.expect(try interp.values.pop().asBool() == false); // 2 <= 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 1 <= 1
    try std.testing.expect(try interp.values.pop().asBool() == false); // 2 < 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 1 < 2
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 < 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 2 >= 1
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 >= 2
    try std.testing.expect(try interp.values.pop().asBool() == true); // 1 >= 1
    try std.testing.expect(try interp.values.pop().asBool() == true); // 2 > 1
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 > 2
    try std.testing.expect(try interp.values.pop().asBool() == false); // 1 > 1
}

test "interpret string concat" {
    const concat = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .String = "one"}}};
    var two = Expr{ .Literal = .{ .value = .{ .String = "two"}}};
    var expr = Expr.initBinary(&one, concat, &two);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&expr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(std.mem.eql(u8, try interp.values.pop().asString(), "onetwo"));
}

test "interpret binary error" {
    const badToken = Token.init(.FOR, "for", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var all = Expr{ .Literal = .{ .value = .{ .String = "all" } } };
    var oneForAll = Expr.initBinary(&one, badToken, &all);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&oneForAll));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret adding error" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var trueBool = Expr{ .Literal = .{ .value = .{ .Bool = true } } };
    var truePlusTrue = Expr.initBinary(&trueBool, plus, &trueBool);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&truePlusTrue));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret number error" {
    const minus = Token.init(.MINUS, "-", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1 } } };
    var oneStr = Expr{ .Literal = .{ .value = .{ .String = "one" } } };
    var oneMinusOneStr = Expr.initBinary(&one, minus, &oneStr);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&oneMinusOneStr));

    var interp = Interpreter.init(std.testing.allocator, stmts);
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

    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(bang, &num))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(bang, &falseBool))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(bang, &trueBool))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(bang, &nil))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(bang, &string))));
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(neg, &num))));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    // Test in reverse order (popping values off the stack)
    try std.testing.expect(try interp.values.pop().asNumber() == -1); // -1
    try std.testing.expect(try interp.values.pop().asBool() == false); // !"string"
    try std.testing.expect(try interp.values.pop().asBool() == true); // !nil
    try std.testing.expect(try interp.values.pop().asBool() == false); // !true
    try std.testing.expect(try interp.values.pop().asBool() == true); // !false

    // Test error (-"string")
    try stmts.append(Stmt.expression(@constCast(&Expr.initUnary(neg, &string))));
    interp = Interpreter.init(std.testing.allocator, stmts);
    try std.testing.expectError(InterpreterError.InterpreterError, interp.interpret());
}

test "interpret grouping" {
    const plus = Token.init(.PLUS, "+", null, 1);
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var onePlusOne = Expr.initBinary(&one, plus, &one);
    var group = Expr.initGrouping(&onePlusOne);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&group));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 2);
}

test "interpret literal" {
    var one = Expr{ .Literal = .{ .value = .{ .Number = 1} } };
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    try stmts.append(Stmt.expression(&one));

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();
    try interp.interpret();

    try std.testing.expect(try interp.values.pop().asNumber() == 1);
}
