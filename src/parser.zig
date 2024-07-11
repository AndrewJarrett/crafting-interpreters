const std = @import("std");
const Token = @import("token.zig").Token;
const Nil = @import("token.zig").Nil;
const Value = @import("token.zig").Value;
const HeapValue = @import("token.zig").HeapValue;
const Lexer = @import("lexer.zig").Lexer;
const TT = @import("token.zig").TokenType;
const VT = @import("token.zig").ValueType;
const Interpreter = @import("interpreter.zig").Interpreter;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;
const Stmt = @import("stmt.zig").Stmt;

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
    left: *Expr,
    operator: Token,
    right: *Expr,

    fn evaluate(self: Binary, interp: *Interpreter) ResultError!Result(HeapValue) {
        const leftResult = try self.left.evaluate(interp);
        defer leftResult.deinit(); // Free temporary processing result
        const left = (try leftResult.unwrap()).value;

        const rightResult = try self.right.evaluate(interp);
        defer rightResult.deinit(); // Free temporary processing result
        const right = (try rightResult.unwrap()).value;

        return switch (self.operator.tokenType) {
            .GREATER => self.getNumberResult(interp, &left, &right),
            .GREATER_EQUAL => self.getNumberResult(interp, &left, &right),
            .LESS => self.getNumberResult(interp, &left, &right),
            .LESS_EQUAL => self.getNumberResult(interp, &left, &right),
            .MINUS => self.getNumberResult(interp, &left, &right),
            .PLUS => if (left.isNumber() and right.isNumber()) {
                return self.getNumberResult(interp, &left, &right);
            } else if (left.isString() and right.isString()) {
                return self.getStringResult(interp, &left, &right);
            } else {
                return Result(HeapValue).err(Error{ .token = self.operator, .message = "Addition operator expects a number or a string" });
            },
            .SLASH => self.getNumberResult(interp, &left, &right),
            .STAR => self.getNumberResult(interp, &left, &right),
            .BANG_EQUAL => Result(HeapValue).ok(HeapValue.init(interp.allocator, left.isNotEqual(right))),
            .EQUAL_EQUAL => Result(HeapValue).ok(HeapValue.init(interp.allocator, left.isEqual(right))),
            else => Result(HeapValue).err(Error{ .token = self.operator, .message = "Unexpected operator for a binary expression" }),
        };
    }

    pub fn getNumberResult(self: Binary, interp: *Interpreter, left: *const Value, right: *const Value) Result(HeapValue) {
        if (left.isNumber() and right.isNumber()) {
            const l = left.asNumber() catch unreachable;
            const r = right.asNumber() catch unreachable;

            return switch (self.operator.tokenType) {
                .GREATER => Result(HeapValue).ok(HeapValue.init(interp.allocator, l > r)),
                .GREATER_EQUAL => Result(HeapValue).ok(HeapValue.init(interp.allocator, l >= r)),
                .LESS => Result(HeapValue).ok(HeapValue.init(interp.allocator, l < r)),
                .LESS_EQUAL => Result(HeapValue).ok(HeapValue.init(interp.allocator, (l <= r))),
                .MINUS => Result(HeapValue).ok(HeapValue.init(interp.allocator, l - r)),
                .PLUS => Result(HeapValue).ok(HeapValue.init(interp.allocator, l + r)),
                .SLASH => Result(HeapValue).ok(HeapValue.init(interp.allocator, l / r)),
                .STAR => Result(HeapValue).ok(HeapValue.init(interp.allocator, l * r)),
                else => Result(HeapValue).err(Error.init(self.operator, "Unexpected operator for a binary expression with two numbers")),
            };
        } else {
            return Result(HeapValue).err(Error.init(self.operator, "Operator expects two numbers"));
        }
    }

    pub fn getStringResult(self: Binary, interp: *Interpreter, left: *const Value, right: *const Value) Result(HeapValue) {
        if (left.isString() and right.isString()) {
            const l = left.asString() catch unreachable;
            const r = right.asString() catch unreachable;

            return switch (self.operator.tokenType) {
                .PLUS => {
                    const result = std.fmt.allocPrint(interp.allocator, "{s}{s}", .{l, r}) catch {
                        return Result(HeapValue).err(Error.init(self.operator, "Error allocating space for concatenated string"));
                    };
                    return Result(HeapValue).ok(HeapValue.init(interp.allocator, result).setFreeValue(true));
                },
                else => Result(HeapValue).err(Error.init(self.operator, "Unexpected operator for a binary expression with two strings")),
            };
        } else {
            return Result(HeapValue).err(Error.init(self.operator, "Operator expects two strings"));
        }
    }
};

pub const Unary = struct {
    operator: Token,
    right: *Expr,

    fn evaluate(self: Unary, interp: *Interpreter) ResultError!Result(HeapValue) {
        const rightResult = try self.right.evaluate(interp);
        defer rightResult.deinit(); // Free temporary processing results
        const right = (try rightResult.unwrap()).value;

        return switch (self.operator.tokenType) {
            .BANG => Result(HeapValue).ok(HeapValue.init(interp.allocator, !right.isTruthy())),
            .MINUS => self.getNumberResult(interp, &right),
            else => Result(HeapValue).err(Error.init(self.operator, "Unexpected operator for a unary expression")),
        };
    }

    pub fn getNumberResult(self: Unary, interp: *Interpreter, right: *const Value) Result(HeapValue) {
        if (right.isNumber()) {
            const r = right.asNumber() catch unreachable;

            return switch (self.operator.tokenType) {
                .MINUS => Result(HeapValue).ok(HeapValue.init(interp.allocator, -(r))),
                else => Result(HeapValue).err(Error.init(self.operator, "Unexpected unary operator for a number")),
            };
        } else {
            const err = Error.init(self.operator, "Operator expects a number");
            return Result(HeapValue).err(err);
        }
    }
};

pub const Literal = struct {
    value: ?*HeapValue = null,

    fn evaluate(self: Literal, interp: *Interpreter) Result(HeapValue) {
        if (self.value) |value| {
            // Need to copy the value to prevent a double-free in case the
            // result of an expression is the statement itself
            const copy = switch (value.value) {
                .Bool => HeapValue.init(interp.allocator, value.value.asBool() catch unreachable),
                .Nil => HeapValue.init(interp.allocator, value.value.asNil() catch unreachable),
                .Number => HeapValue.init(interp.allocator, value.value.asNumber() catch unreachable),
                .String => HeapValue.init(interp.allocator, value.value.asString() catch unreachable),
            };
            return Result(HeapValue).ok(copy);
        } else {
            return Result(HeapValue).err(Error.init(null, "The literal value was null."));
        }
    }
};

pub const Grouping = struct {
    expression: *Expr,

    fn evaluate(self: Grouping, interp: *Interpreter) ResultError!Result(HeapValue) {
        return try self.expression.evaluate(interp);
    }
};

pub const Expr = union(enum) {
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Grouping: Grouping,

    pub fn initBinary(alloc: Allocator, left: *Expr, operator: Token, right: *Expr) *Expr {
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = Expr{ .Binary = Binary{ .left = left, .operator = operator, .right = right } };
        return ptr;
    }

    pub fn initUnary(alloc: Allocator, operator: Token, right: *Expr) *Expr {
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = Expr{ .Unary = Unary{ .operator = operator, .right = right } };
        return ptr;
    }

    pub fn initLiteral(alloc: Allocator, value: anytype) *Expr {
        const literal = if (@TypeOf(value) == @TypeOf(null)) null else HeapValue.init(alloc, value);
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = Expr{ .Literal = Literal{ .value = literal } };
        return ptr;
    }

    pub fn initGrouping(alloc: Allocator, expression: *Expr) *Expr {
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = Expr{ .Grouping = Grouping{ .expression = expression } };
        return ptr;
    }

    pub fn deinit(self: *Expr, alloc: Allocator) void {
        // Recursively deinit down the expression tree
        switch (self.*) {
            // Base case - the tree has to have literals at the leaf nodes
            .Literal => |lit| if (lit.value) |val| {
                val.deinit();
            },
            .Binary => |b| {
                b.left.deinit(alloc);
                b.right.deinit(alloc);
            },
            .Unary => |u| {
                u.right.deinit(alloc);
            },
            .Grouping => |g| {
                g.expression.deinit(alloc);
            },
        }

        // Each expr should destroy itself at the end
        alloc.destroy(self);
    }

    pub fn evaluate(self: Expr, interp: *Interpreter) ResultError!Result(HeapValue) {
        return switch (self) {
            .Binary => try self.Binary.evaluate(interp),
            .Unary => try self.Unary.evaluate(interp),
            .Literal => self.Literal.evaluate(interp),
            .Grouping => try self.Grouping.evaluate(interp),
        };
    }

    pub fn format(self: Expr, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
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
            const right = try self.unary();
            return self.createExpr(Unary{ .operator = operator, .right = right });
        }

        return try self.primary();
    }

    fn primary(self: *Parser) ParseError!*Expr {
        if (self.match(.{TT.FALSE})) return self.createExpr(Expr.initLiteral(self.allocator, false));
        if (self.match(.{TT.TRUE})) return self.createExpr(Expr.initLiteral(self.allocator, true));
        if (self.match(.{TT.NIL})) return self.createExpr(Expr.initLiteral(self.allocator, {}));

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
        return self.tokens.items[self.current].*;
    }

    fn previous(self: Parser) Token {
        return self.tokens.items[self.current - 1].*;
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

    try tokens.append(&Token.init(std.testing.allocator, TT.PLUS, "+", null, 1));

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

//test "Read grammar file" {
//    std.debug.print("{s}", .{createStructs()});
//}

test "Expr: 1" {
    const expr = Expr.initLiteral(std.testing.allocator, 1.0);
    defer expr.deinit(std.testing.allocator);

    try std.testing.expect(testExprMatchesExpected("1", expr.*));
}

test "Expr: (+ 1 2)" {
    const expr = Expr.initBinary( 
        std.testing.allocator,
        Expr.initLiteral(std.testing.allocator, 1.0), 
        Token.init(std.testing.allocator, TT.PLUS, "+", null, 1), 
        Expr.initLiteral(std.testing.allocator, 2.0)
    );
    defer expr.deinit(std.testing.allocator);

    try std.testing.expect(testExprMatchesExpected("(+ 1 2)", expr.*));
}

test "Expr: (* (- 123) (group 45.67))" {
    const minus = Token.init(std.testing.allocator, TT.MINUS, "-", null, 1);
    const mult = Token.init(std.testing.allocator, TT.STAR, "*", null, 1);
    const negNum1 = Expr.initLiteral(std.testing.allocator, 123.0);
    const num2 = Expr.initLiteral(std.testing.allocator, 45.67);
    const expr = Expr.initBinary(
        std.testing.allocator,
        Expr.initUnary(std.testing.allocator, minus, negNum1),
        mult,
        Expr.initGrouping(std.testing.allocator, num2)
    );
    defer expr.deinit(std.testing.allocator);

    try std.testing.expect(testExprMatchesExpected("(* (- 123) (group 45.67))", expr.*));
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
