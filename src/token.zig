const std = @import("std");
const HeapValue = @import("value.zig").HeapValue;

const Allocator = std.mem.Allocator;
const str = []const u8;

pub const TokenType = enum {
    // Single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,

    pub fn format(self: TokenType, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const Token = struct {
    alloc: Allocator,
    tokenType: TokenType,
    lexeme: str,
    literal: ?*const HeapValue,
    line: usize,

    pub fn init(alloc: Allocator, tokenType: TokenType, lexeme: str, literal: anytype, line: usize) Token {
        return Token{
            .alloc = alloc,
            .tokenType = tokenType,
            .lexeme = lexeme,
            .literal = if (@TypeOf(literal) == @TypeOf(null)) null else HeapValue.init(alloc, literal),
            .line = line,
        };
    }

    pub fn deinit(self: *const Token) void {
        if (self.literal) |lit| lit.deinit();
    }

    pub fn format(self: Token, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {s} {?}", .{ @tagName(self.tokenType), self.lexeme, self.literal });
    }
};

test "init a new token" {
    var token = Token.init(std.testing.allocator, .AND, "and", null, 1);
    defer token.deinit();

    try std.testing.expect(@TypeOf(token) == Token);
    try std.testing.expect(token.tokenType == TokenType.AND);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, "and") == true);
    try std.testing.expect(token.literal == null);
    try std.testing.expect(token.line == 1);
}

test "print the token" {
    var token = Token.init(std.testing.allocator, .WHILE, "while", null, 420);
    defer token.deinit();

    const expected: str = "WHILE while null";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}

test "print a number" {
    var token = Token.init(std.testing.allocator, .NUMBER, "1", 1, 2);
    defer token.deinit();

    const expected: str = "NUMBER 1 1";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}

test "print a string" {
    var token = Token.init(std.testing.allocator, .STRING, "this is a string", "this is a string", 3);
    defer token.deinit();

    const expected: str = "STRING this is a string \"this is a string\"";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}
