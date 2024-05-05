const std = @import("std");
const Token = @import("token.zig").Token;
const TT = @import("token.zig").TokenType;
const Lexer = @import("lexer.zig").Lexer;

const ArrayList = std.ArrayList;
const ArrayListAligned = std.array_list.ArrayListAligned;
const Allocator = std.mem.Allocator;
const str = []const u8;

const Scanner = struct {
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

        try self.tokens.append(Token.init(self.allocator, TT.EOF, "", self.line));
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
                } else {
                    Lexer.handle_error(self.line, "Unexpected character.");
                }
            },
        }
    }

    fn number(self: *Self) !void {
        while (self.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            _ = self.advance();

            while (self.isDigit(self.peek())) _ = self.advance();
        }

        const literal = self.src[self.start..self.current];
        try self.addTokenWithLiteral(TT.NUMBER, literal);
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isAtEnd()) : (_ = self.advance()) {
            if (self.peek() == '\n') self.line += 1;
        }

        if (self.isAtEnd()) {
            Lexer.handle_error(self.line, "Unterminated string.");
        }

        _ = self.advance(); // consume the clsoing '"'

        // Trim the surrounding quotes
        const value = self.src[(self.start + 1)..(self.current - 1)];
        try self.addTokenWithLiteral(TT.STRING, value);
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

    fn isDigit(self: Self, char: u8) bool {
        _ = self;
        return switch (char) {
            '0'...'9' => true,
            else => false,
        };
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

    fn addTokenWithLiteral(self: *Self, tokenType: TT, literal: ?str) !void {
        _ = literal;
        const text = self.src[self.start..self.current];
        try self.tokens.append(Token.init(self.allocator, tokenType, text, self.line));
    }

    pub fn deinit(self: Self) void {
        for (self.tokens.items) |token| {
            token.deinit();
        }
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

    //for (scanner.tokens.items) |token| {
    //    std.debug.print("{s}", .{token});
    //    try std.testing.expect(std.mem.eql(u8, token, undefined));
    //}
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
