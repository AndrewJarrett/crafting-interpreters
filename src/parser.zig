const std = @import("std");
const Token = @import("token.zig").Token;
const Value = @import("token.zig").Value;
const Lexer = @import("lexer.zig").Lexer;
const TT = @import("token.zig").TokenType;

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
    value: ?Value = null,
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
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Grouping: Grouping,

    pub fn initBinary(left: *Expr, operator: Token, right: *Expr) Expr {
        return Expr { .Binary = Binary{ .left = left, .operator = operator, .right = right }};
    }

    pub fn initUnary(operator: Token, right: *Expr) Expr {
        return Expr { .Unary = Unary{ .operator = operator, .right = right }};
    }

    pub fn initLiteral(value: ?Value) Expr {
        return Expr { .Literal = Literal{ .value = value }};
    }

    pub fn initGrouping(expression: *Expr) Expr {
        return Expr { .Grouping = Grouping{ .expression = expression }};
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
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right});
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
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right});
        }

        return expr;
    }

    fn term(self: *Parser) ParseError!*Expr {
        const expr = try self.factor();

        while (self.match(.{ TT.MINUS, TT.PLUS })) {
            const operator: Token = self.previous();
            const right = try self.factor();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right});
        }

        return expr;
    }

    fn factor(self: *Parser) ParseError!*Expr {
        const expr = try self.unary();

        while (self.match(.{ TT.SLASH, TT.STAR })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right});
        }

        return expr;
    }

    fn unary(self: *Parser) ParseError!*Expr {
        if (self.match(.{ TT.BANG, TT.MINUS })) {
            const operator: Token = self.previous();
            const right: *const Expr = try self.unary();
            return self.createExpr(Unary{ .operator = operator, .right = right});
        }

        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*Expr {
        if (self.match(.{TT.FALSE})) return self.createExpr(Literal{ .value = Value{ .Bool = false }});
        if (self.match(.{TT.TRUE})) return self.createExpr(Literal{ .value = Value{ .Bool = true }});
        if (self.match(.{TT.NIL})) return self.createExpr(Literal{ .value = Value{ .Nil = {} }});

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
    try tokens.append(Token.init(TT.NUMBER, "1", Value{ .Number = 1.0 }, 1));
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));
    try tokens.append(Token.init(TT.NUMBER, "1", Value{ .Number = 1.0 }, 1));
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
