const std = @import("std");

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
};

pub const Token = struct {
    const Self = @This();

    tokenType: TokenType,
    lexeme: str,
    //literal: anytype,
    line: usize,

    //pub fn init(tokenType: TokenType, lexeme: str, literal: anytype, line: usize) Self {
    pub fn init(tokenType: TokenType, lexeme: str, line: usize) Self {
        return Self{
            .tokenType = tokenType,
            .lexeme = lexeme,
            //.literal = literal,
            .line = line,
        };
    }

    pub fn format(self: Self, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {s}", .{ @tagName(self.tokenType), self.lexeme });
    }
};

test "init a new token" {
    const token = Token.init(
        TokenType.AND,
        "and",
        1,
    );

    try std.testing.expect(@TypeOf(token) == Token);
    try std.testing.expect(token.tokenType == TokenType.AND);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, "and") == true);
    try std.testing.expect(token.line == 1);
}

test "print the token" {
    const token = Token.init(
        TokenType.WHILE,
        "while",
        420, // Take a toke(n)
    );

    const expected: str = "WHILE while";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}
