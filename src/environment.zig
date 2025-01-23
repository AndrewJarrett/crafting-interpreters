const std = @import("std");
const Value = @import("value.zig").Value;
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const str = []const u8;

pub const Environment = struct  {
    alloc: Allocator,
    values: StringHashMap(Value),

    pub fn init(alloc: Allocator) Environment {
        return Environment{
            .alloc = alloc,
            .values = StringHashMap(Value).init(alloc),
        };
    }

    pub fn deinit(self: *Environment) void {
        //while (self.values.valueIterator().next()) |val| {
        //    val.deinit();
        //}
        self.values.deinit();
    }

    pub fn define(self: *Environment, variable: str, value: Value) void {
        self.values.put(variable, value) catch @panic("Out of memory when defining a variable");
    }

    pub fn get(self: Environment, name: Token) !Value {
        if (self.values.get(name.lexeme)) |val| {
            return val;
        } else {
            Lexer.handleRuntimeError(name, "Undefined variable");
            return EnvironmentError.UndefinedVariable;
        }
    }
};

pub const EnvironmentError = error {
    UndefinedVariable,
};

test "environment should init and deinit" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    try std.testing.expectEqual(@TypeOf(env), Environment);
    try std.testing.expect(env.values.count() == 0);
}

test "define should work for all types of values" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    try std.testing.expect(env.values.count() == 0);

    env.define("testNumber", Value.init(1));
    env.define("testString", Value.init("this is a test!"));
    env.define("testBool", Value.init(true));
    env.define("testNil", Value.init({}));

    try std.testing.expect(env.values.count() == 4);
}

test "define should overwrite an existing key in the map" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const testNumber = Token.init(std.testing.allocator, .VAR, "testNumber", 1, 1);
    defer testNumber.deinit();
    env.define(testNumber.lexeme, testNumber.literal.?.value);
    env.define(testNumber.lexeme, Value.init(2));

    try std.testing.expect(env.values.count() == 1);
    try std.testing.expect(try (try env.get(testNumber)).asNumber() == 2);
}

test "get should return all types of values" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const testNumber = Token.init(std.testing.allocator, .VAR, "testNumber", 1, 1);
    defer testNumber.deinit();
    const testString = Token.init(std.testing.allocator, .VAR, "testString", "this is a test!", 1);
    defer testString.deinit();
    const testBool = Token.init(std.testing.allocator, .VAR, "testBool", true, 1);
    defer testBool.deinit();
    const testNil = Token.init(std.testing.allocator, .VAR, "testNil", {}, 1);
    defer testNil.deinit();

    env.define(testNumber.lexeme, testNumber.literal.?.value);
    env.define(testString.lexeme, testString.literal.?.value);
    env.define(testBool.lexeme, testBool.literal.?.value);
    env.define(testNil.lexeme, testNil.literal.?.value);

    try std.testing.expect(try (try env.get(testNumber)).asNumber() == 1);
    try std.testing.expect(std.mem.eql(u8, try (try env.get(testString)).asString(), "this is a test!"));
    try std.testing.expect(try (try env.get(testBool)).asBool() == true);
    try std.testing.expect(try (try env.get(testNil)).asNil() == {});
}

test "get should return a runtime error when getting a non-existent value in the map" {
    var env = Environment.init(std.testing.allocator);
    defer env.deinit();

    const token = Token.init(std.testing.allocator, .VAR, "testNumber", 1, 1);
    defer token.deinit();

    try std.testing.expectError(EnvironmentError.UndefinedVariable, env.get(token));
    try std.testing.expect(Lexer.hasRuntimeError == true);
}
