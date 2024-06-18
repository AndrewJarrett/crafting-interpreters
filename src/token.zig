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

    pub fn format(self: TokenType, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const ValueType = enum {
    Bool,
    Nil,
    Number,
    String,
};

pub const Value = union(enum) {
    Bool: bool,
    Nil,
    Number: f64,
    String: str,

    pub fn init(valueType: ValueType, value: anytype) Value {
        return switch (valueType) {
            .Bool => Value{ .Bool = value },
            .Nil => Value{ .Nil = {} },
            .Number => Value{ .Number = value },
            .String => Value{ .String = value },
        };
    }

    pub fn isBool(self: Value) bool {
        return self == .Bool;
    }

    pub fn asBool(self: Value) !bool {
        return if (self.isBool()) self.Bool else ValueError.NotABool;
    }

    pub fn isNil(self: Value) bool {
        return self == .Nil;
    }

    pub fn asNil(self: Value) !void {
        return if (self.isNil()) self.Nil else ValueError.NotNil;
    }

    pub fn isNumber(self: Value) bool {
        return self == .Number;
    }

    pub fn asNumber(self: Value) !f64 {
        return if (self.isNumber()) self.Number else ValueError.NotANumber;
    }

    pub fn isString(self: Value) bool {
        return self == .String;
    }

    pub fn asString(self: Value) !str {
        return if (self.isString()) self.String else ValueError.NotAString;
    }

    pub fn isEqual(self: Value, other: Value) bool {
        var eql = true;
        errdefer eql = false;

        eql = switch (self) {
            .Nil => other.isNil() and (self.asNil() catch {}) == (other.asNil() catch {}),
            .Bool => other.isBool() and (self.asBool() catch false) == (other.asBool() catch false),
            .Number => other.isNumber() and (self.asNumber() catch 0) == (other.asNumber() catch 0),
            .String => other.isString() and std.mem.eql(u8, self.asString() catch "", other.asString() catch ""),
        };

        return eql;
    }

    pub fn isNotEqual(self: Value, other: Value) bool {
        return !self.isEqual(other);
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .Nil => false,
            .Bool => self.asBool() catch unreachable,
            else => true,
        };
    }

    pub fn format(self: Value, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Bool => |b| try writer.print("{}", .{b}),
            .Nil => |_| try writer.print("nil", .{}),
            .Number => |n| try writer.print("{d}", .{n}),
            .String => |s| try writer.print("{s}", .{s}),
        }
    }
};

pub const ValueError = error {
    NotABool,
    NotNil,
    NotANumber,
    NotAString,
};

pub const Token = struct {
    const Self = @This();

    tokenType: TokenType,
    lexeme: str,
    literal: ?Value,
    line: usize,

    pub fn init(tokenType: TokenType, lexeme: str, literal: ?Value, line: usize) Self {
        return Self{
            .tokenType = tokenType,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn format(self: Self, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {s} {?}", .{ @tagName(self.tokenType), self.lexeme, self.literal });
    }
};

test "init a new token" {
    const token = Token.init(
        TokenType.AND,
        "and",
        null,
        1,
    );

    try std.testing.expect(@TypeOf(token) == Token);
    try std.testing.expect(token.tokenType == TokenType.AND);
    try std.testing.expect(std.mem.eql(u8, token.lexeme, "and") == true);
    try std.testing.expect(token.literal == null);
    try std.testing.expect(token.line == 1);
}

test "print the token" {
    const token = Token.init(
        TokenType.WHILE,
        "while",
        null,
        420, // Take a toke(n)
    );

    const expected: str = "WHILE while null";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}

test "print a number" {
    const token = Token.init(
        TokenType.NUMBER,
        "1",
        Value{ .Number = @as(f64, 1) },
        2,
    );

    const expected: str = "NUMBER 1 1";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}

test "print a string" {
    const token = Token.init(
        TokenType.STRING,
        "this is a string",
        Value{ .String = "this is a string" },
        3,
    );

    const expected: str = "STRING this is a string this is a string";
    var tokenBuffer: [expected.len]u8 = undefined;

    _ = try std.fmt.bufPrint(&tokenBuffer, "{s}", .{token});
    try std.testing.expect(std.mem.eql(u8, &tokenBuffer, expected));
}

test "value bool" {
    const boolTrue = Value { .Bool = true };
    const boolFalse = Value { .Bool = false };
    const notBool = Value { .Number = 3 };

    try std.testing.expect(boolTrue.isBool());
    try std.testing.expect(try boolTrue.asBool());
    try std.testing.expect(boolFalse.isBool());
    try std.testing.expect(try boolFalse.asBool() == false);
    try std.testing.expect(notBool.isBool() == false);
    try std.testing.expectError(ValueError.NotABool, notBool.asBool());
}

test "value nil" {
    const nil = Value { .Nil = {} };
    const notNil = Value { .Number = 3 };

    try std.testing.expect(nil.isNil());
    try std.testing.expect(try nil.asNil() == {});
    try std.testing.expect(notNil.isNil() == false);
    try std.testing.expectError(ValueError.NotNil, notNil.asNil());
}

test "value number" {
    const num = Value { .Number = 42 };
    const NaN = Value { .Bool = false };

    try std.testing.expect(num.isNumber());
    try std.testing.expect(try num.asNumber() == @as(f64, 42));
    try std.testing.expect(NaN.isNumber() == false);
    try std.testing.expectError(ValueError.NotANumber, NaN.asNumber());
}

test "value string" {
    const string = Value { .String = "this is cool yo" };
    const notString = Value { .Number = 3 };

    try std.testing.expect(string.isString());
    try std.testing.expect(std.mem.eql(u8, try string.asString(), "this is cool yo"));
    try std.testing.expect(notString.isString() == false);
    try std.testing.expectError(ValueError.NotAString, notString.asString());
}

test "value isTruthy" {
    const truthy1 = Value { .Bool = true };
    const truthy2 = Value { .Number = 0 };
    const truthy3 = Value { .Number = 42 };
    const truthy4 = Value { .Number = -42 };
    const truthy5 = Value { .String = "" };
    const truthy6 = Value { .String = "this is true?" };

    const falsey1 = Value { .Bool = false };
    const falsey2 = Value { .Nil = {} };

    try std.testing.expect(truthy1.isTruthy());
    try std.testing.expect(truthy2.isTruthy());
    try std.testing.expect(truthy3.isTruthy());
    try std.testing.expect(truthy4.isTruthy());
    try std.testing.expect(truthy5.isTruthy());
    try std.testing.expect(truthy6.isTruthy());

    try std.testing.expect(falsey1.isTruthy() == false);
    try std.testing.expect(falsey2.isTruthy() == false);
}

test "value isEqual and isNotEqual" {
    const nil1 = Value { .Nil = {} };
    const nil2 = Value { .Nil = {} };
    const number1 = Value { .Number = 42 };
    const number2 = Value { .Number = 42 };
    const number3 = Value { .Number = 0 };
    const number4 = Value { .Number = -42 };
    const number5 = Value { .Number = 0.00 };
    const bool1 = Value { .Bool = true };
    const bool2 = Value { .Bool = true };
    const bool3 = Value { .Bool = false };
    const string1 = Value { .String = "hello how are you today?" };
    const string2 = Value { .String = "hello how are you today?" };
    const string3 = Value { .String = "" };

    try std.testing.expect(nil1.isEqual(nil1));
    try std.testing.expect(nil1.isEqual(nil2));
    try std.testing.expect(nil1.isNotEqual(number1));
    try std.testing.expect(nil1.isNotEqual(bool3));
    try std.testing.expect(nil1.isNotEqual(string3));
    try std.testing.expect(number1.isEqual(number1));
    try std.testing.expect(number1.isEqual(number2));
    try std.testing.expect(number3.isEqual(number5));
    try std.testing.expect(number1.isNotEqual(number3));
    try std.testing.expect(number1.isNotEqual(number4));
    try std.testing.expect(number1.isNotEqual(nil1));
    try std.testing.expect(number1.isNotEqual(bool1));
    try std.testing.expect(number1.isNotEqual(string1));
    try std.testing.expect(bool1.isEqual(bool1));
    try std.testing.expect(bool1.isEqual(bool2));
    try std.testing.expect(bool1.isNotEqual(bool3));
    try std.testing.expect(bool1.isNotEqual(nil1));
    try std.testing.expect(bool1.isNotEqual(number1));
    try std.testing.expect(bool1.isNotEqual(string1));
    try std.testing.expect(string1.isEqual(string1));
    try std.testing.expect(string1.isEqual(string2));
    try std.testing.expect(string1.isNotEqual(string3));
    try std.testing.expect(string1.isNotEqual(nil1));
    try std.testing.expect(string1.isNotEqual(number1));
    try std.testing.expect(string1.isNotEqual(bool1));

    try std.testing.expect(nil1.isNotEqual(number3));
    try std.testing.expect(nil1.isNotEqual(bool3));
    try std.testing.expect(nil1.isNotEqual(string3));
    try std.testing.expect(number3.isNotEqual(nil1));
    try std.testing.expect(number3.isNotEqual(bool3));
    try std.testing.expect(number3.isNotEqual(string3));
    try std.testing.expect(bool3.isNotEqual(nil1));
    try std.testing.expect(bool3.isNotEqual(number3));
    try std.testing.expect(bool3.isNotEqual(string3));
}

//test "value isEqual and isNotEqual errors" {
//    const bool1 = Value { .Bool = true };
//    const nil1 = Value { .Nil = {} };
//    const string1 = Value { .String = "" };
//    const number1 = Value { .Number = 0 };
//
//    try std.testing.expectError(ValueError.NotNil, nil1.isEqual(bool1));
//    try std.testing.expectError(ValueError.NotNil, nil1.isEqual(string1));
//    try std.testing.expectError(ValueError.NotNil, nil1.isEqual(number1));
//    try std.testing.expectError(ValueError.NotNil, nil1.isNotEqual(bool1));
//    try std.testing.expectError(ValueError.NotNil, nil1.isNotEqual(string1));
//    try std.testing.expectError(ValueError.NotNil, nil1.isNotEqual(number1));
//
//    try std.testing.expectError(ValueError.NotABool, bool1.isEqual(nil1));
//    try std.testing.expectError(ValueError.NotABool, bool1.isEqual(string1));
//    try std.testing.expectError(ValueError.NotABool, bool1.isEqual(number1));
//    try std.testing.expectError(ValueError.NotABool, bool1.isNotEqual(nil1));
//    try std.testing.expectError(ValueError.NotABool, bool1.isNotEqual(string1));
//    try std.testing.expectError(ValueError.NotABool, bool1.isNotEqual(number1));
//
//    try std.testing.expectError(ValueError.NotANumber, number1.isEqual(bool1));
//    try std.testing.expectError(ValueError.NotANumber, number1.isEqual(string1));
//    try std.testing.expectError(ValueError.NotANumber, number1.isEqual(nil1));
//    try std.testing.expectError(ValueError.NotANumber, number1.isNotEqual(bool1));
//    try std.testing.expectError(ValueError.NotANumber, number1.isNotEqual(string1));
//    try std.testing.expectError(ValueError.NotANumber, number1.isNotEqual(nil1));
//
//    try std.testing.expectError(ValueError.NotAString, string1.isEqual(bool1));
//    try std.testing.expectError(ValueError.NotAString, string1.isEqual(nil1));
//    try std.testing.expectError(ValueError.NotAString, string1.isEqual(number1));
//    try std.testing.expectError(ValueError.NotAString, string1.isNotEqual(bool1));
//    try std.testing.expectError(ValueError.NotAString, string1.isNotEqual(nil1));
//    try std.testing.expectError(ValueError.NotAString, string1.isNotEqual(number1));
//}
