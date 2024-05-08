const std = @import("std");
const Token = @import("token.zig").Token;
const TT = @import("token.zig").TokenType;

const str = []const u8;

const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

const Unary = struct {
    operator: Token,
    right: *const Expr,
};

const Literal = struct {
    value: str = "nil",
};

const Grouping = struct {
    expression: *const Expr,
};

const ExprType = enum {
    Binary,
    Unary,
    Literal,
    Grouping,
};

const Expr = union(ExprType) {
    const Self = @This();

    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Grouping: Grouping,

    pub fn format(self: Self, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .Binary => |b| try writer.print("({s} {s} {s})", .{ b.operator.lexeme, b.left, b.right }),
            .Unary => |u| try writer.print("({s} {s})", .{ u.operator.lexeme, u.right }),
            .Literal => |l| try writer.print("{s}", .{l.value}),
            .Grouping => |g| try writer.print("(group {s})", .{g.expression}),
        }
    }
};

fn testExprMatchesExpected(comptime expected: str, expr: Expr) bool {
    var tokenBuffer: [expected.len]u8 = undefined;
    return std.mem.eql(u8, std.fmt.bufPrint(&tokenBuffer, "{s}", .{expr}) catch "FAILED", expected);
}

test "Expr: 1" {
    const expr = Expr{ .Literal = Literal{ .value = "1" } };
    try std.testing.expect(testExprMatchesExpected("1", expr));
}

test "Expr: (+ 1 2)" {
    const expr = Expr{ .Binary = .{ .left = &Expr{ .Literal = .{ .value = "1" } }, .operator = Token.init(TT.PLUS, "+", 1), .right = &Expr{ .Literal = .{ .value = "2" } } } };
    try std.testing.expect(testExprMatchesExpected("(+ 1 2)", expr));
}

test "Expr: (* (- 123) (group 45.67))" {
    const expr = Expr{ .Binary = Binary{ .left = &Expr{ .Unary = .{ .operator = Token.init(TT.MINUS, "-", 1), .right = &Expr{ .Literal = .{ .value = "123" } } } }, .operator = Token.init(TT.STAR, "*", 1), .right = &Expr{ .Grouping = .{ .expression = &Expr{ .Literal = .{ .value = "45.67" } } } } } };
    try std.testing.expect(testExprMatchesExpected("(* (- 123) (group 45.67))", expr));
}
