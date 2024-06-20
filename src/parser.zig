const std = @import("std");
const Token = @import("token.zig").Token;
const Nil = @import("token.zig").Nil;
const Value = @import("token.zig").Value;
const Lexer = @import("lexer.zig").Lexer;
const TT = @import("token.zig").TokenType;
const VT = @import("token.zig").ValueType;
const Interpreter = @import("interpreter.zig").Interpreter;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const str = []const u8;

// It would be cool to figure out a way to generate the structs used in the syntax tree from a file
// at comptime. There is a way to generate structs at comptime, but I don't know of a way to
// dynamically set the name of the structs or if that would make sense. I might need to
// generate a map of the struct name to the configuration and set each named struct by calling
// another function to dynamically create the struct based on the input configuration.
// I may also want to add the format string to the configuration as well and also look at dynamically
// adding all possible types to the ExprType struct at comptime as well as generating the format function
// switch prongs based on the dynamic tagged enum value and format string.
fn createStructs() str {
    const grammar = @embedFile("zlox.grammar");
    var lineIt = std.mem.splitSequence(u8, grammar, "\n");
    while (lineIt.next()) |line| {
        const separatorIndex = std.mem.indexOf(u8, line, ":");

        if (separatorIndex) |i| {
            const name = std.mem.trim(u8, line[0..i], " \t");
            const fields = std.mem.trim(u8, line[(i + 1)..], " \t\n\r");
            std.debug.print("it.next(): Name: {s}; Fields: {s}\n", .{ name, fields });

            var fieldIt = std.mem.splitSequence(u8, fields, ", ");
            while (fieldIt.next()) |field| {
                const trimmedField = std.mem.trim(u8, field, " \t\n\r");
                std.debug.print("Field: {s}\n", .{trimmedField});
            }
        }
    }
    return grammar;
}
//_ = createStructs();

const Type = @Type(.{
    .Struct = .{
        .layout = .Auto,
        .fields = &[_]std.builtin.TypeInfo.StructField{},
        .decls = &[_]std.builtin.TypeInfo.Declaration{},
        .is_tuple = false,
    },
});

pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,

    fn evaluate(self: Binary, interp: *Interpreter) ResultError!Result(Value) {
        const left = try (try self.left.evaluate(interp)).unwrap();
        const right = try (try self.right.evaluate(interp)).unwrap();

        return switch (self.operator.tokenType) {
            .GREATER => self.getNumberResult(left, right),
            .GREATER_EQUAL => self.getNumberResult(left, right),
            .LESS => self.getNumberResult(left, right),
            .LESS_EQUAL => self.getNumberResult(left, right),
            .MINUS => self.getNumberResult(left, right),
            .PLUS => if (left.isNumber() and right.isNumber()) {
                return self.getNumberResult(left, right);
            } else if (left.isString() and right.isString()) {
                return self.getStringResult(left, right, interp);
            } else {
                return Result(Value).err(Error{ .token = self.operator, .message = "Addition operator expects a number or a string" });
            },
            .SLASH => self.getNumberResult(left, right),
            .STAR => self.getNumberResult(left, right),
            .BANG_EQUAL => Result(Value).ok(.{ .Bool = left.isNotEqual(right) }),
            .EQUAL_EQUAL => Result(Value).ok(.{ .Bool = left.isEqual(right) }),
            else => Result(Value).err(Error{ .token = self.operator, .message = "Unexpected operator for a binary expression" }),
        };
    }

    pub fn getNumberResult(self: Binary, left: Value, right: Value) Result(Value) {
        if (left.isNumber() and right.isNumber()) {
            const l = left.asNumber() catch unreachable;
            const r = right.asNumber() catch unreachable;

            return switch (self.operator.tokenType) {
                .GREATER => Result(Value).ok(.{ .Bool = l > r }),
                .GREATER_EQUAL => Result(Value).ok(.{ .Bool = l >= r }),
                .LESS => Result(Value).ok(.{ .Bool = l < r }),
                .LESS_EQUAL => Result(Value).ok(.{ .Bool = (l <= r) }),
                .MINUS => Result(Value).ok(.{ .Number = l - r }),
                .PLUS => Result(Value).ok(.{ .Number = l + r }),
                .SLASH => Result(Value).ok(.{ .Number = l / r }),
                .STAR => Result(Value).ok(.{ .Number = l * r }),
                else => Result(Value).err(Error.init(self.operator, "Unexpected operator for a binary expression with two numbers")),
            };
        } else {
            return Result(Value).err(Error.init(self.operator, "Operator expects two numbers"));
        }
    }

    pub fn getStringResult(self: Binary, left: Value, right: Value, interp: *Interpreter) Result(Value) {
        if (left.isString() and right.isString()) {
            const l = left.asString() catch unreachable;
            const r = right.asString() catch unreachable;

            return switch (self.operator.tokenType) {
                .PLUS => {
                    const result = std.fmt.allocPrint(interp.allocator, "{s}{s}", .{l, r}) catch {
                        return Result(Value).err(Error.init(self.operator, "Error allocating space for concatenated string"));
                    };
                    return Result(Value).ok(.{ .String = result });
                },
                else => Result(Value).err(Error.init(self.operator, "Unexpected operator for a binary expression with two strings")),
            };
        } else {
            return Result(Value).err(Error.init(self.operator, "Operator expects two strings"));
        }
    }
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,

    fn evaluate(self: Unary, interp: *Interpreter) ResultError!Result(Value) {
        const right = try (try self.right.evaluate(interp)).unwrap();

        return switch (self.operator.tokenType) {
            .BANG => Result(Value).ok(.{ .Bool = !right.isTruthy() }),
            .MINUS => self.getNumberResult(right),
            else => Result(Value).err(Error.init(self.operator, "Unexpected operator for a unary expression")),
        };
    }

    pub fn getNumberResult(self: Unary, right: Value) Result(Value) {
        if (right.isNumber()) {
            const r = right.asNumber() catch unreachable;

            return switch (self.operator.tokenType) {
                .MINUS => Result(Value).ok(.{ .Number = -(r) }),
                else => Result(Value).err(Error.init(self.operator, "Unexpected unary operator for a number")),
            };
        } else {
            const err = Error.init(self.operator, "Operator expects a number");
            return Result(Value).err(err);
        }
    }
};

pub const Literal = struct {
    value: ?Value = null,

    fn evaluate(self: Literal, interp: *Interpreter) Result(Value) {
        _ = interp;
        if (self.value) |value| {
            return Result(Value).ok(value);
        } else {
            return Result(Value).err(Error{ .token = null, .message = "The literal value was null." });
        }
    }
};

pub const Grouping = struct {
    expression: *const Expr,

    fn evaluate(self: Grouping, interp: *Interpreter) ResultError!Result(Value) {
        return try self.expression.evaluate(interp);
    }
};

pub const Expr = union(enum) {
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Grouping: Grouping,

    pub fn initBinary(left: *Expr, operator: Token, right: *Expr) Expr {
        return Expr{ .Binary = Binary{ .left = left, .operator = operator, .right = right } };
    }

    pub fn initUnary(operator: Token, right: *Expr) Expr {
        return Expr{ .Unary = Unary{ .operator = operator, .right = right } };
    }

    pub fn initLiteral(value: ?Value) Expr {
        return Expr{ .Literal = Literal{ .value = value } };
    }

    pub fn initGrouping(expression: *Expr) Expr {
        return Expr{ .Grouping = Grouping{ .expression = expression } };
    }

    pub fn evaluate(self: Expr, interp: *Interpreter) ResultError!Result(Value) {
        return switch (self) {
            .Binary => try self.Binary.evaluate(interp),
            .Unary => try self.Unary.evaluate(interp),
            .Literal => self.Literal.evaluate(interp),
            .Grouping => try self.Grouping.evaluate(interp),
        };
    }

    pub fn format(self: *const Expr, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self.*) {
            .Binary => |b| try writer.print("({s} {s} {s})", .{ b.operator.lexeme, b.left.*, b.right.* }),
            .Unary => |u| try writer.print("({s} {s})", .{ u.operator.lexeme, u.right.* }),
            .Literal => |l| try writer.print("{?}", .{l.value}),
            .Grouping => |g| try writer.print("(group {s})", .{g.expression}),
        }
    }
};

pub const EvaluateError = error {
    ExpectedLiteral,
    ExpectedNumber,
    UnexpectedOperator,
};

pub const Parser = struct {
    tokens: ArrayList(Token),
    current: usize = 0,
    allocator: Allocator,
    nodes: ArrayList(*Expr),

    pub fn init(allocator: Allocator, tokens: ArrayList(Token)) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .nodes = ArrayList(*Expr).init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        while (self.nodes.popOrNull()) |expr| {
            self.allocator.destroy(expr);
        }
        self.nodes.deinit();
    }

    fn createExpr(self: *Parser, inner: anytype) ParseError!*Expr {
        const expr = try self.allocator.create(Expr);
        expr.* = switch (@TypeOf(inner)) {
            Binary => Expr{ .Binary = inner },
            Unary => Expr{ .Unary = inner },
            Literal => Expr{ .Literal = inner },
            Grouping => Expr{ .Grouping = inner },
            else => return ParseError.NoExpression,
        };
        _ = try self.nodes.append(expr);
        return expr;
    }

    pub fn parse(self: *Parser) ParseError!Expr {
        const expr =  try self.expression();
        return expr.*;
    }

    fn expression(self: *Parser) ParseError!*Expr {
        return try self.equality();
    }

    fn equality(self: *Parser) ParseError!*Expr {
        const expr = try self.comparison();

        while (self.match(.{ TT.BANG_EQUAL, TT.EQUAL_EQUAL })) {
            const operator: Token = self.previous();
            const right = try self.comparison();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn comparison(self: *Parser) ParseError!*Expr {
        const expr = try self.term();

        while (self.match(.{
            TT.GREATER, TT.GREATER_EQUAL, TT.LESS, TT.LESS_EQUAL,
        })) {
            const operator: Token = self.previous();
            const right = try self.term();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn term(self: *Parser) ParseError!*Expr {
        const expr = try self.factor();

        while (self.match(.{ TT.MINUS, TT.PLUS })) {
            const operator: Token = self.previous();
            const right = try self.factor();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn factor(self: *Parser) ParseError!*Expr {
        const expr = try self.unary();

        while (self.match(.{ TT.SLASH, TT.STAR })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn unary(self: *Parser) ParseError!*Expr {
        if (self.match(.{ TT.BANG, TT.MINUS })) {
            const operator: Token = self.previous();
            const right: *const Expr = try self.unary();
            return self.createExpr(Unary{ .operator = operator, .right = right });
        }

        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*Expr {
        if (self.match(.{TT.FALSE})) return self.createExpr(Literal{ .value = .{ .Bool = false } });
        if (self.match(.{TT.TRUE})) return self.createExpr(Literal{ .value = .{ .Bool = true } });
        if (self.match(.{TT.NIL})) return self.createExpr(Literal{ .value = .{ .Nil = { } }});

        if (self.match(.{ TT.NUMBER, TT.STRING })) {
            return self.createExpr(Literal{ .value = self.previous().literal });
        }

        if (self.match(.{TT.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.", ParseError.MissingParens);
            return self.createExpr(Grouping{ .expression = expr });
        }

        _ = try Parser.handleError(self.peek(), "Expect expression.", ParseError.NoExpression);
        return ParseError.NoExpression;
    }

    fn consume(self: *Parser, tokenType: TT, msg: str, err: ParseError) !Token {
        if (self.check(tokenType)) return self.advance();
        _ = try Parser.handleError(self.peek(), msg, err);
        return err;
    }

    fn handleError(token: Token, msg: str, err: ParseError) ParseError!void {
        Lexer.handleTokenError(token, msg);
        return err;
    }

    fn match(self: *Parser, types: anytype) bool {
        inline for (types) |tokenType| {
            if (self.check(tokenType)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(self: Parser, tokenType: TT) bool {
        if (self.isAtEnd()) return false;
        return self.peek().tokenType == tokenType;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: Parser) bool {
        return self.peek().tokenType == TT.EOF;
    }

    fn peek(self: Parser) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: Parser) Token {
        return self.tokens.items[self.current - 1];
    }

    fn synchronize(self: Parser) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().tokenType == TT.SEMICOLON) return;

            switch (self.peek().tokenType) {
                .CLASS,
                .FUN,
                .VAR,
                .FOR,
                .IF,
                .WHILE,
                .PRINT,
                .RETURN,
                => return,
                else => {},
            }
        }

        _ = self.advance();
    }
};

const ParseError = error{
    MissingParens,
    NoExpression,
    OutOfMemory,
};

test "Parser.init()" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));

    var parser = Parser.init(std.testing.allocator, tokens);
    defer parser.deinit();

    try std.testing.expect(@TypeOf(parser) == Parser);
    try std.testing.expectEqual(parser.tokens.items.len, 1);
    try std.testing.expectEqual(parser.current, 0);
}

test "Parse error no expression" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));

    var parser = Parser.init(std.testing.allocator, tokens);
    defer parser.deinit();
    try std.testing.expectError(ParseError.NoExpression, parser.parse());
}

test "Parser success" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.NUMBER, "1", .{ .Number = 1.0 }, 1));
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));
    try tokens.append(Token.init(TT.NUMBER, "1", .{ .Number = 1.0 }, 1));
    try tokens.append(Token.init(TT.EOF, "", null, 1));

    var parser = Parser.init(std.testing.allocator, tokens);
    defer parser.deinit();
    const expr = try parser.parse();
    const expected: str = "(+ 1 1)";
    try std.testing.expect(testExprMatchesExpected(expected, expr));
}

//test "Read grammar file" {
//    std.debug.print("{s}", .{createStructs()});
//}

test "Expr: 1" {
    const expr = Expr{ .Literal = Literal{ .value = .{ .Number = 1.0 } } };
    try std.testing.expect(testExprMatchesExpected("1", expr));
}

test "Expr: (+ 1 2)" {
    const expr = Expr{ .Binary = .{ .left = &Expr{ .Literal = .{ .value = .{ .Number = 1.0 } } }, .operator = Token.init(TT.PLUS, "+", null, 1), .right = &Expr{ .Literal = .{ .value = .{ .Number = 2.0 } } } } };
    try std.testing.expect(testExprMatchesExpected("(+ 1 2)", expr));
}

test "Expr: (* (- 123) (group 45.67))" {
    const expr = Expr{ .Binary = Binary{ .left = &Expr{ .Unary = .{ .operator = Token.init(TT.MINUS, "-", null, 1), .right = &Expr{ .Literal = .{ .value = .{ .Number = 123.0 } } } } }, .operator = Token.init(TT.STAR, "*", null, 1), .right = &Expr{ .Grouping = .{ .expression = &Expr{ .Literal = .{ .value = .{ .Number = 45.67 } } } } } } };
    try std.testing.expect(testExprMatchesExpected("(* (- 123) (group 45.67))", expr));
}

// Helper method for checking if Expression matches expected string
fn testExprMatchesExpected(comptime expected: str, expr: Expr) bool {
    var tokenBuffer: [1000]u8 = undefined;
    return std.mem.eql(u8, std.fmt.bufPrint(&tokenBuffer, "{s}", .{expr}) catch "FAILED", expected);
}
