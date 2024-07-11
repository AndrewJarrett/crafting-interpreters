const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TT = @import("token.zig").TokenType;
const Expr = @import("expr.zig").Expr;
const Binary = @import("expr.zig").Binary;
const Literal = @import("expr.zig").Literal;
const Unary = @import("expr.zig").Unary;
const Grouping = @import("expr.zig").Grouping;
const Value = @import("value.zig").Value;
const HeapValue = @import("value.zig").HeapValue;
const Interpreter = @import("interpreter.zig").Interpreter;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;
const Stmt = @import("stmt.zig").Stmt;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const str = []const u8;

pub const Parser = struct {
    tokens: *ArrayList(*const Token),
    statements: ArrayList(Stmt),
    current: usize = 0,
    allocator: Allocator,
    nodes: ArrayList(*Expr),

    pub fn init(allocator: Allocator, tokens: *ArrayList(*const Token)) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .statements = ArrayList(Stmt).init(allocator),
            .nodes = ArrayList(*Expr).init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        while (self.nodes.popOrNull()) |expr| {
            self.allocator.destroy(expr);
        }
        self.nodes.deinit();
        self.statements.deinit();
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

    pub fn parse(self: *Parser) ParseError!ArrayList(Stmt) {
        while (!self.isAtEnd()) {
            const result = try self.statement();
            try self.statements.append(result);
        }
        return self.statements;
    }

    fn statement(self: *Parser) ParseError!Stmt {
        if (self.match(.{.PRINT})) {
            return self.printStatement();
        } else {
            return self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) ParseError!Stmt {
        const val = try self.expression();

        _ = try self.consume(.SEMICOLON, "Expect ';' after value.", ParseError.MissingSemicolon);
        return Stmt.print(val);
    }

    fn expressionStatement(self: *Parser) ParseError!Stmt {
        const expr = try self.expression();

        _ = try self.consume(.SEMICOLON, "Expect ';' after expression.", ParseError.MissingSemicolon);
        return Stmt.expression(expr);
    }

    fn expression(self: *Parser) ParseError!*Expr {
        return try self.equality();
    }

    fn equality(self: *Parser) ParseError!*Expr {
        const expr = try self.comparison();

        while (self.match(.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator: Token = self.previous();
            const right = try self.comparison();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn comparison(self: *Parser) ParseError!*Expr {
        const expr = try self.term();

        while (self.match(.{
            .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL,
        })) {
            const operator: Token = self.previous();
            const right = try self.term();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn term(self: *Parser) ParseError!*Expr {
        const expr = try self.factor();

        while (self.match(.{ .MINUS, .PLUS })) {
            const operator: Token = self.previous();
            const right = try self.factor();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn factor(self: *Parser) ParseError!*Expr {
        const expr = try self.unary();

        while (self.match(.{ .SLASH, .STAR })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            return self.createExpr(Binary{ .left = expr, .operator = operator, .right = right });
        }

        return expr;
    }

    fn unary(self: *Parser) ParseError!*Expr {
        if (self.match(.{ .BANG, .MINUS })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            return self.createExpr(Unary{ .operator = operator, .right = right });
        }

        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*Expr {
        if (self.match(.{.FALSE})) return self.createExpr(Expr.initLiteral(self.allocator, false));
        if (self.match(.{.TRUE})) return self.createExpr(Expr.initLiteral(self.allocator, true));
        if (self.match(.{.NIL})) return self.createExpr(Expr.initLiteral(self.allocator, {}));

        if (self.match(.{ .NUMBER, .STRING })) {
            return self.createExpr(Literal{ .value = self.previous().literal });
        }

        if (self.match(.{.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = try self.consume(.RIGHT_PAREN, "Expect ')' after expression.", ParseError.MissingParens);
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
        return self.peek().tokenType == .EOF;
    }

    fn peek(self: Parser) Token {
        return self.tokens.items[self.current].*;
    }

    fn previous(self: Parser) Token {
        return self.tokens.items[self.current - 1].*;
    }

    fn synchronize(self: Parser) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().tokenType == .SEMICOLON) return;

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
    MissingSemicolon,
    NoExpression,
    OutOfMemory,
};

test "Parser.init()" {
    var tokens = ArrayList(*const Token).init(std.testing.allocator);
    defer {
        for (tokens.items) |tok| {
            tok.deinit();
        }
        tokens.deinit();
    }

    try tokens.append(&Token.init(std.testing.allocator, .PLUS, "+", null, 1));

    var parser = Parser.init(std.testing.allocator, &tokens);
    defer parser.deinit();

    try std.testing.expect(@TypeOf(parser) == Parser);
    try std.testing.expectEqual(parser.tokens.items.len, 1);
    try std.testing.expectEqual(parser.current, 0);
}

test "Parse error no expression" {
    var tokens = ArrayList(*const Token).init(std.testing.allocator);
    defer {
        for (tokens.items) |tok| {
            tok.deinit();
        }
        tokens.deinit();
    }

    try tokens.append(&Token.init(std.testing.allocator, .PLUS, "+", null, 1));

    var parser = Parser.init(std.testing.allocator, &tokens);
    defer parser.deinit();
    try std.testing.expectError(ParseError.NoExpression, parser.parse());
}

test "Parser success" {
    var tokens = ArrayList(*const Token).init(std.testing.allocator);
    defer {
        for (tokens.items) |tok| {
            tok.deinit();
        }
        tokens.deinit();
    }

    try tokens.append(&Token.init(std.testing.allocator, .NUMBER, "1", 1.0, 1));
    try tokens.append(&Token.init(std.testing.allocator, .PLUS, "+", null, 1));
    try tokens.append(&Token.init(std.testing.allocator, .NUMBER, "1", 1.0, 1));
    try tokens.append(&Token.init(std.testing.allocator, .SEMICOLON, ";", null, 1));
    try tokens.append(&Token.init(std.testing.allocator, .EOF, "", null, 1));

    var parser = Parser.init(std.testing.allocator, &tokens);
    defer parser.deinit();
    const stmts = try parser.parse();
    const expected = "(+ 1 1)";
    try std.testing.expect(testStmtsMatchesExpected(expected, stmts));
}

// Helper method for checking if Expression matches expected string
fn testStmtsMatchesExpected(comptime expected: str, stmts: ArrayList(Stmt)) bool {
    var isEqual = false;

    for (stmts.items) |stmt| {
        isEqual = testExprMatchesExpected(expected, stmt.expression.*);
    }

    return isEqual;
}

fn testExprMatchesExpected(comptime expected: str, expr: Expr) bool {
    var tokenBuffer: [1000]u8 = undefined;

    return  std.mem.eql(u8, std.fmt.bufPrint(&tokenBuffer, "{s}", .{expr}) catch "FAILED", expected);
}
