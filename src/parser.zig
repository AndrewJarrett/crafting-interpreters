const std = @import("std");
const Token = @import("token.zig").Token;
const Value = @import("token.zig").Value;
const Lexer = @import("lexer.zig").Lexer;
const TT = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;

const str = []const u8;

// It would be cool to figure out a way to generate the structs used in the syntax tree from a file
// at comptime. There is a way to generate structs at comptime, but I don't know of a way to
// dynamically set the name of the structs or if that would make sense. I might need to
// generate a map of the struct name to the configuration and set each named struct by calling
// another function to dynamicallly create the struct based on the input configuration.
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
            .Literal => |l| try writer.print("{?}", .{l.value}),
            .Grouping => |g| try writer.print("(group {s})", .{g.expression}),
        }
    }
};

const Parser = struct {
    const Self = @This();

    tokens: ArrayList(Token),
    current: usize = 0,

    pub fn init(tokens: ArrayList(Token)) Self {
        return Self{
            .tokens = tokens,
        };
    }

    pub fn parse(self: *Self) ParseError!Expr {
        std.debug.print("{s}", .{self.tokens.items});
        const expr = self.expression();
        std.debug.print("{any}", .{expr});
        return expr;
    }

    fn expression(self: *Self) ParseError!Expr {
        return try self.equality();
    }

    fn equality(self: *Self) ParseError!Expr {
        var expr = try self.comparison();

        while (self.match(.{ TT.BANG_EQUAL, TT.EQUAL_EQUAL })) {
            const operator: Token = self.previous();
            const right = try self.comparison();
            expr = Expr{ .Binary = .{ .left = &expr, .operator = operator, .right = &right } };
        }

        return expr;
    }

    fn comparison(self: *Self) ParseError!Expr {
        var expr = try self.term();

        while (self.match(.{
            TT.GREATER, TT.GREATER_EQUAL, TT.LESS, TT.LESS_EQUAL,
        })) {
            const operator: Token = self.previous();
            var right = try self.term();
            expr = .{ .Binary = .{ .left = &expr, .operator = operator, .right = &right } };
        }

        return expr;
    }

    fn term(self: *Self) ParseError!Expr {
        var expr = try self.factor();

        while (self.match(.{ TT.MINUS, TT.PLUS })) {
            const operator: Token = self.previous();
            var right = try self.factor();
            expr = .{ .Binary = .{ .left = &expr, .operator = operator, .right = &right } };
        }

        return expr;
    }

    fn factor(self: *Self) ParseError!Expr {
        var expr = try self.unary();

        while (self.match(.{ TT.SLASH, TT.STAR })) {
            const operator: Token = self.previous();
            const right = try self.unary();
            expr = .{ .Binary = .{ .left = &expr, .operator = operator, .right = &right } };
        }

        return expr;
    }

    fn unary(self: *Self) ParseError!Expr {
        if (self.match(.{ TT.BANG, TT.MINUS })) {
            const operator: Token = self.previous();
            var right = try self.unary();
            return Expr{ .Unary = .{ .operator = operator, .right = &right } };
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParseError!Expr {
        if (self.match(.{TT.FALSE})) return .{ .Literal = .{ .value = .{ .Bool = false } } };
        if (self.match(.{TT.TRUE})) return .{ .Literal = .{ .value = .{ .Bool = true } } };
        if (self.match(.{TT.NIL})) return .{ .Literal = .{ .value = .{ .Nil = {} } } };

        if (self.match(.{ TT.NUMBER, TT.STRING })) return .{ .Literal = .{ .value = self.previous().literal } };

        if (self.match(.{TT.LEFT_PAREN})) {
            var expr = try self.expression();
            _ = try self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.", ParseError.MissingParens);
            return Expr{ .Grouping = .{ .expression = &expr } };
        }

        _ = try Parser.handleError(self.peek(), "Expect expression.", ParseError.NoExpression);
        return ParseError.NoExpression;
    }

    fn consume(self: *Self, tokenType: TT, msg: str, err: ParseError) !Token {
        if (self.check(tokenType)) return self.advance();
        _ = try Parser.handleError(self.peek(), msg, err);
        return err;
    }

    fn handleError(token: Token, msg: str, err: ParseError) ParseError!void {
        Lexer.handleTokenError(token, msg);
        return err;
    }

    fn match(self: *Self, types: anytype) bool {
        inline for (types) |tokenType| {
            if (self.check(tokenType)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(self: Self, tokenType: TT) bool {
        if (self.isAtEnd()) return false;
        return self.peek().tokenType == tokenType;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().tokenType == TT.EOF;
    }

    fn peek(self: Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn synchronize(self: Self) void {
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
};

test "Parser.init()" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));

    const parser = Parser.init(tokens);

    try std.testing.expect(@TypeOf(parser) == Parser);
    try std.testing.expectEqual(parser.tokens.items.len, 1);
    try std.testing.expectEqual(parser.current, 0);
}

test "Parse error no expression" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));

    var parser = Parser.init(tokens);
    try std.testing.expectError(ParseError.NoExpression, parser.parse());
}

test "Parser success" {
    var tokens = ArrayList(Token).init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append(Token.init(TT.NUMBER, "1", null, 1));
    try tokens.append(Token.init(TT.PLUS, "+", null, 1));
    try tokens.append(Token.init(TT.NUMBER, "1", null, 1));
    try tokens.append(Token.init(TT.EOF, "", null, 1));

    var parser = Parser.init(tokens);
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
    var tokenBuffer: [expected.len]u8 = undefined;
    return std.mem.eql(u8, std.fmt.bufPrint(&tokenBuffer, "{s}", .{expr}) catch "FAILED", expected);
}
