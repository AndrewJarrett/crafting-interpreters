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

            else => Lexer.handle_error(self.line, "Unexpected character."),
        }
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

    fn isAtEnd(self: Self) bool {
        return self.current >= self.src.len;
    }

    fn advance(self: *Self) u8 {
        const c = self.src[self.current];
        self.current += 1;
        return c;
    }

    fn addToken(self: *Self, tokenType: TT) !void {
        //self.addToken(tokenType, null);

        const text = self.src[self.start..self.current];
        try self.tokens.append(Token.init(self.allocator, tokenType, text, self.line));
    }

    //fn addToken(self: Self, tokenType: TT, literal: ?Object) void {
    //    _ = literal;
    //    const text = std.mem.substring(self.src, self.start, self.current);
    //    self.tokens.append(Token.init(self.allocator, text, self.line));
    //}

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
