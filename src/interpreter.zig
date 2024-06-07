const std = @import("std");

const Expr = @import("parser.zig").Expr;
const ET = @import("parser.zig").ExprType;
const Binary = @import("parser.zig").Binary;
const Unary = @import("parser.zig").Unary;
const Literal = @import("parser.zig").Literal;
const Grouping = @import("parser.zig").Grouping;
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

    pub fn getLiteral(self: Interpreter) !Value {
        return switch (self.expr) {
            .Literal => self.expr.Literal.value.?,
            else => InterpreterError.NotALiteral, 
        };
    }

    pub fn getGrouping(self: Interpreter) !void {
        return self.evaluate(self.expr.expression);
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

test "getLiteral" {
    const interp = Interpreter.init(Expr { .Literal = Literal{ .value = Value{ .Bool = true }}});
    const value = try interp.getLiteral();

    try std.testing.expect(value.isBool());
    try std.testing.expect(try value.asBool());
}

test "getLiteral error" {
    const interp = Interpreter.init(Expr { .Unary = Unary{ .operator = Token.init(TT.BANG, "!", null, 1), .right = &Expr{ .Literal = Literal{ .value = Value{ .Bool = true }}}}});

    try std.testing.expectError(InterpreterError.NotALiteral, interp.getLiteral());
}

test "getGrouping" {
}

test "getGrouping error" {
}
