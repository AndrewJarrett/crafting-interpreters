const std = @import("std");
const Token = @import("token.zig").Token;
const TT = @import("token.zig").TokenType;
const Value = @import("token.zig").Value;
const Lexer = @import("lexer.zig").Lexer;

const ArrayList = std.ArrayList;
const ArrayListAligned = std.array_list.ArrayListAligned;
const Allocator = std.mem.Allocator;
const str = []const u8;

// Setup the keywords as a static final hash map
const keywords = std.ComptimeStringMap(TT, .{
    .{ "and", TT.AND },
    .{ "class", TT.CLASS },
    .{ "else", TT.ELSE },
    .{ "false", TT.FALSE },
    .{ "for", TT.FOR },
    .{ "fun", TT.FUN },
    .{ "if", TT.IF },
    .{ "nil", TT.NIL },
    .{ "or", TT.OR },
    .{ "print", TT.PRINT },
    .{ "return", TT.RETURN },
    .{ "super", TT.SUPER },
    .{ "this", TT.THIS },
    .{ "true", TT.TRUE },
    .{ "var", TT.VAR },
    .{ "while", TT.WHILE },
});

pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    src: str,
    tokens: ArrayList(Token) = undefined,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(allocator: Allocator, src: str) Self {
        return Self{
            .allocator = allocator,
            .tokens = ArrayList(Token).init(allocator),
            .src = src,
        };
    }

    pub fn scanTokens(self: *Self) !ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.addToken(TT.EOF);
        return self.tokens;
    }

    fn scanToken(self: *Self) !void {
        const c: u8 = self.advance();

        switch (c) {
            // Single characters
            '(' => try self.addToken(TT.LEFT_PAREN),
            ')' => try self.addToken(TT.RIGHT_PAREN),
            '[' => try self.addToken(TT.LEFT_BRACE),
            ']' => try self.addToken(TT.RIGHT_BRACE),
            ',' => try self.addToken(TT.COMMA),
            '.' => try self.addToken(TT.DOT),
            '-' => try self.addToken(TT.MINUS),
            '+' => try self.addToken(TT.PLUS),
            ';' => try self.addToken(TT.SEMICOLON),
            '*' => try self.addToken(TT.STAR),

            // Equality
            '!' => try self.addToken(if (self.match('=')) TT.BANG_EQUAL else TT.BANG),
            '=' => try self.addToken(if (self.match('=')) TT.EQUAL_EQUAL else TT.EQUAL),
            '<' => try self.addToken(if (self.match('=')) TT.LESS_EQUAL else TT.LESS),
            '>' => try self.addToken(if (self.match('=')) TT.GREATER_EQUAL else TT.GREATER),

            // Comments
            '/' => if (self.match('/')) {
                while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
            } else {
                try self.addToken(TT.SLASH);
            },

            // Whitespace
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,

            // Literals
            '"' => try self.string(),

            else => {
                if (self.isDigit(c)) {
                    try self.number();
                } else if (self.isAlphaNumeric(c)) {
                    try self.identifier();
                } else {
                    Lexer.handleError(self.line, "Unexpected character.");
                }
            },
        }
    }

    fn identifier(self: *Self) !void {
        while (self.isAlphaNumeric(self.peek())) _ = self.advance();

        const text = self.src[self.start..self.current];
        const tokenType = if (keywords.get(text)) |keyword| keyword else TT.IDENTIFIER;

        try self.addToken(tokenType);
    }

    fn number(self: *Self) !void {
        while (self.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            _ = self.advance();

            while (self.isDigit(self.peek())) _ = self.advance();
        }

        const literal = self.src[self.start..self.current];
        try self.addTokenWithLiteral(TT.NUMBER, .{ .Number = try std.fmt.parseFloat(f64, literal) });
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isAtEnd()) : (_ = self.advance()) {
            if (self.peek() == '\n') self.line += 1;
        }

        if (self.isAtEnd()) {
            Lexer.handleError(self.line, "Unterminated string.");
        }

        _ = self.advance(); // consume the clsoing '"'

        // Trim the surrounding quotes
        const value = self.src[(self.start + 1)..(self.current - 1)];
        try self.addTokenWithLiteral(TT.STRING, .{ .String = value });
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd() or self.src[self.current] != expected) {
            return false;
        } else {
            self.current += 1; // If expected matches, advance the cursor
            return true;
        }
    }

    fn peek(self: Self) u8 {
        if (self.isAtEnd()) return '\n';
        return self.src[self.current];
    }

    fn peekNext(self: Self) u8 {
        if (self.current + 1 >= self.src.len) return '\n';
        return self.src[self.current + 1];
    }

    fn isAlpha(self: Self, char: u8) bool {
        _ = self;
        return switch (char) {
            'a'...'z', 'A'...'Z', '_' => true,
            else => false,
        };
    }

    fn isDigit(self: Self, char: u8) bool {
        _ = self;
        return switch (char) {
            '0'...'9' => true,
            else => false,
        };
    }

    fn isAlphaNumeric(self: Self, char: u8) bool {
        return self.isAlpha(char) or self.isDigit(char);
    }

    fn isAtEnd(self: Self) bool {
        return self.current >= self.src.len;
    }

    fn advance(self: *Self) u8 {
        const c = self.src[self.current];
        self.current += 1;
        return c;
    }

    fn addToken(self: *Self, tokenType: TT) !void {
        try self.addTokenWithLiteral(tokenType, null);
    }

    fn addTokenWithLiteral(self: *Self, tokenType: TT, literal: ?Value) !void {
        var text: []const u8 = "";
        if (self.current <= self.src.len) {
            text = self.src[self.start..self.current];
        }
        std.log.info("TokenType: {s}; Len: {d}; Start: {d}; Current: {d}; Text: {d}", .{tokenType, self.src.len, self.start, self.current, text});
        try self.tokens.append(Token.init(tokenType, text, literal, self.line));
    }

    pub fn deinit(self: Self) void {
        self.tokens.deinit();
    }
};

test "init" {
    const src = "this is my source code";
    const scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    try std.testing.expect(@TypeOf(scanner) == Scanner);
    try std.testing.expect(std.mem.eql(u8, scanner.src, src));
    try std.testing.expect(scanner.start == 0);
    try std.testing.expect(scanner.current == 0);
    try std.testing.expect(scanner.line == 1);
    try std.testing.expect(scanner.tokens.items.len == 0);
}

test "scanTokens" {
    const src = "this is my source code";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try std.testing.expect(tokens.items.len > 0);
    try std.testing.expect(tokens.items.len == 6);

    for (scanner.tokens.items) |token| {
        try std.testing.expect(token.tokenType == TT.THIS or token.tokenType == TT.IDENTIFIER or token.tokenType == TT.EOF);
    }
}

test "scanToken" {
    const src = "()";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    try std.testing.expect(scanner.src.len == 2);
    try std.testing.expect(scanner.start == 0);
    try std.testing.expect(scanner.current == 0);

    try scanner.scanToken();
    scanner.start = scanner.current; // Need to manually advance the start
    try std.testing.expect(scanner.start == 1);
    try std.testing.expect(scanner.current == 1);

    try scanner.scanToken();
    try std.testing.expect(scanner.current == 2);

    try std.testing.expect(scanner.tokens.items.len == 2);
    try std.testing.expect(std.mem.eql(u8, scanner.tokens.items[0].lexeme, "("));
    try std.testing.expect(std.mem.eql(u8, scanner.tokens.items[1].lexeme, ")"));
}

test "advance" {
    const src = "this is my source code";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    try std.testing.expect(scanner.advance() == src[0]);
}

test "isAtEnd" {
    const src = "this is my source code";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    // less than
    try std.testing.expect(scanner.isAtEnd() == false);

    // equal to length
    scanner.current = src.len;
    try std.testing.expect(scanner.isAtEnd() == true);

    // greater than length
    scanner.current = src.len + 15;
    try std.testing.expect(scanner.isAtEnd() == true);
}

test "string literal" {
    const src = "\"this is my source code\"";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try std.testing.expect(tokens.items.len == 2); // Include EOF
    try std.testing.expect(std.mem.eql(u8, tokens.items[0].lexeme, src));
}

test "number literal" {
    const src = "123";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    try std.testing.expect(tokens.items.len == 2); // Include EOF
    try std.testing.expect(std.mem.eql(u8, tokens.items[0].lexeme, src));
}

test "isDigit" {
    const src = "this can be whatever";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    for ('0'..'9') |c| {
        try std.testing.expect(scanner.isDigit(@intCast(c)) == true);
    }

    for ('a'..'z') |c| {
        try std.testing.expect(scanner.isDigit(@intCast(c)) == false);
    }

    for ('A'..'Z') |c| {
        try std.testing.expect(scanner.isDigit(@intCast(c)) == false);
    }

    for ("!@#$%^&*()_+-=`~") |c| {
        try std.testing.expect(scanner.isDigit(@intCast(c)) == false);
    }
}

test "number" {
    const areNumbers: [3][]const u8 = .{
        "123",
        "123.4",
        "1",
    };
    const notNumbers: [5][]const u8 = .{
        "+",
        "-",
        "a",
        "/",
        ".",
    };

    const src = "this can be whatever";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    for (areNumbers) |num| {
        scanner.src = num;
        scanner.current = 0;
        scanner.start = 0;
        try scanner.scanToken();
        const token = scanner.tokens.popOrNull();
        try std.testing.expect(token != null);
        try std.testing.expect(std.mem.eql(u8, token.?.lexeme, num));
        try std.testing.expect(token.?.tokenType == TT.NUMBER);
    }
    for (notNumbers) |num| {
        scanner.src = num;
        scanner.current = 0;
        scanner.start = 0;
        try scanner.scanToken();
        while (scanner.tokens.popOrNull()) |token| {
            try std.testing.expect(token.tokenType != TT.NUMBER);
        }
    }
}

test "isAlpha" {
    const src = "this can be whatever";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    for ('a'..'z') |c| {
        try std.testing.expect(scanner.isAlpha(@intCast(c)) == true);
    }

    for ('A'..'Z') |c| {
        try std.testing.expect(scanner.isAlpha(@intCast(c)) == true);
    }
    try std.testing.expect(scanner.isAlpha('_') == true);

    for ("!@#$%^&*()+=-`~./<>{}][;:") |c| {
        try std.testing.expect(scanner.isAlpha(@intCast(c)) == false);
    }
}

test "isAlphaNumeric" {
    const src = "this can be whatever";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    for ('a'..'z') |c| {
        try std.testing.expect(scanner.isAlphaNumeric(@intCast(c)) == true);
    }
    for ('A'..'Z') |c| {
        try std.testing.expect(scanner.isAlphaNumeric(@intCast(c)) == true);
    }
    for ('0'..'9') |c| {
        try std.testing.expect(scanner.isAlphaNumeric(@intCast(c)) == true);
    }
    try std.testing.expect(scanner.isAlphaNumeric('_') == true);

    for ("!@#$%^&*()+=-`~./<>{}][;:") |c| {
        try std.testing.expect(scanner.isAlphaNumeric(@intCast(c)) == false);
    }
}

test "identifier" {
    const areIdentifiers: [5][]const u8 = .{
        "asdf",
        "_thisIsValid",
        "this1isValie2",
        "_h_e_l_l_o_",
        "c",
    };
    const notIdentifiers: [5][]const u8 = .{
        "420",
        "+1234",
        "-//",
        "[",
        ".",
    };

    const src = "this can be whatever";
    var scanner = Scanner.init(std.testing.allocator, src);
    defer scanner.deinit();

    for (areIdentifiers) |iden| {
        scanner.src = iden;
        scanner.current = 0;
        scanner.start = 0;
        try scanner.scanToken();
        const token = scanner.tokens.popOrNull();
        try std.testing.expect(token != null);
        try std.testing.expect(std.mem.eql(u8, token.?.lexeme, iden));
        try std.testing.expect(token.?.tokenType == TT.IDENTIFIER);
    }
    for (notIdentifiers) |other| {
        scanner.src = other;
        scanner.current = 0;
        scanner.start = 0;
        try scanner.scanToken();
        while (scanner.tokens.popOrNull()) |token| {
            try std.testing.expect(token.tokenType != TT.IDENTIFIER);
        }
    }
}
