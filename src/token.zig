const std = @import("std");
const Allocator = std.mem.Allocator;

const str = []const u8;

const TokenType = enum {
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

    allocator: Allocator,
    tokenType: TokenType,
    lexeme: str,
    //literal: anytype,
    line: usize,

    to_string: ?str = null,

    //pub fn init(tokenType: TokenType, lexeme: str, literal: anytype, line: usize) Self {
    pub fn init(allocator: Allocator, tokenType: TokenType, lexeme: str, line: usize) Self {
        return Self{
            .allocator = allocator,
            .tokenType = tokenType,
            .lexeme = lexeme,
            //.literal = literal,
            .line = line,
        };
    }

    pub fn deinit(self: Self) void {
        if (self.to_string) |to_string| {
            self.allocator.free(to_string);
        }
    }

    pub fn print(self: *Self) ?str {
        self.to_string = std.fmt.allocPrint(self.allocator, "{s} {s}", .{ @tagName(self.tokenType), self.lexeme }) catch "format failed!"; // add self.literal;
        return self.to_string;
    }
};

test "init a new token" {
    const allocator = std.testing.allocator;

    const token = Token.init(
        allocator,
        TokenType.AND,
        "and",
        1,
    );
    defer token.deinit();

    try std.testing.expect(@TypeOf(token) == Token);
    try std.testing.expect(token.tokenType == TokenType.AND);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, "and") == true);
    try std.testing.expect(token.line == 1);
}

test "print the token" {
    const allocator = std.testing.allocator;

    var token = Token.init(
        allocator,
        TokenType.WHILE,
        "while",
        420, // Take a toke(n)
    );
    defer token.deinit();

    const token_str = token.print();

    try std.testing.expect(@TypeOf(token.print()) == ?[]const u8);
    try std.testing.expect(std.mem.eql(u8, token_str.?, "WHILE while") == true);
}
