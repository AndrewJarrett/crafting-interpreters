const std = @import("std");

const Allocator = std.mem.Allocator;
const str = []const u8;

pub const HeapValue = struct {
    alloc: Allocator,
    value: Value,
    freeValue: bool,

    pub fn init(alloc: Allocator, value: anytype) *const HeapValue {
        return initWithFree(alloc, value, false);
    }

    pub fn initWithFree(alloc: Allocator, value: anytype, freeValue: bool) *const HeapValue {
        const val = Value.init(value);

        const ptr = alloc.create(HeapValue) catch unreachable;
        ptr.alloc = alloc;
        ptr.value = val;
        ptr.freeValue = freeValue;

        return ptr;
    }

    pub fn deinit(self: *const HeapValue) void {
        if (self.freeValue) {
            switch (self.value) {
                .String => |s| self.alloc.free(s),
                else => {},
            }
        }
        self.alloc.destroy(self);
    }

    pub fn format(self: HeapValue, fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.value});
    }
};

pub const Value = union(enum) {
    Bool: bool,
    Nil,
    Number: f64,
    String: str,

    pub fn init(value: anytype) Value {
        //std.debug.print("\nvalue: {any}", .{value});
        return switch (@TypeOf(value)) {
            bool => Value{ .Bool = value },

            usize, u8, u16, u32, u64, 
            f16, f32, f64, 
            i8, i16, i32, i64,
            comptime_int, comptime_float => Value{ .Number = @as(f64, value) },

            void => Value{ .Nil = {} },

            else => Value{ .String = @as(str, value) }, // Any unsupported types will automatically try to be a string
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
            .Bool => |b| if (b) try writer.print("true", .{}) else try writer.print("false", .{}),
            .Nil => |_| try writer.print("nil", .{}),
            .Number => |n| try writer.print("{d}", .{n}),
            .String => |s| try writer.print("\"{s}\"", .{s}),
        }
    }
};

pub const ValueError = error {
    NotABool,
    NotNil,
    NotANumber,
    NotAString,
};

test "heap value" {
    const boolTrue = HeapValue.init(std.testing.allocator, true);
    defer boolTrue.deinit();
    const boolFalse = HeapValue.init(std.testing.allocator, false);
    defer boolFalse.deinit();
    const num1 = HeapValue.init(std.testing.allocator, 1);
    defer num1.deinit();
    const num2 = HeapValue.init(std.testing.allocator, 42);
    defer num2.deinit();
    const nil1 = HeapValue.init(std.testing.allocator, {});
    defer nil1.deinit();
    const nil2 = HeapValue.init(std.testing.allocator, {});
    defer nil2.deinit();
    const string1 = HeapValue.init(std.testing.allocator, "hello");
    defer string1.deinit();
    const string2 = HeapValue.init(std.testing.allocator, "goodbye");
    defer string2.deinit();


    try std.testing.expect(boolTrue.value.isBool() == true);
    try std.testing.expect(try boolTrue.value.asBool() == true);
    try std.testing.expect(boolFalse.value.isBool() == true);
    try std.testing.expect(try boolFalse.value.asBool() == false);
    try std.testing.expect(num1.value.isNumber() == true);
    try std.testing.expect(try num1.value.asNumber() == 1);
    try std.testing.expect(num2.value.isNumber() == true);
    try std.testing.expect(try num2.value.asNumber() == 42);
    try std.testing.expect(nil1.value.isNil() == true);
    try std.testing.expect(try nil1.value.asNil() == {});
    try std.testing.expect(nil2.value.isNil() == true);
    try std.testing.expect(try nil2.value.asNil() == {});
    try std.testing.expect(string1.value.isString() == true);
    try std.testing.expect(std.mem.eql(u8, try string1.value.asString(), "hello"));
    try std.testing.expect(string2.value.isString() == true);
    try std.testing.expect(std.mem.eql(u8, try string2.value.asString(), "goodbye"));
}

test "value bool" {
    const boolTrue = Value.init(true);
    const boolFalse = Value.init(false);
    const notBool = Value.init(3);

    try std.testing.expect(boolTrue.isBool());
    try std.testing.expect(try boolTrue.asBool());
    try std.testing.expect(boolFalse.isBool());
    try std.testing.expect(try boolFalse.asBool() == false);
    try std.testing.expect(notBool.isBool() == false);
    try std.testing.expectError(ValueError.NotABool, notBool.asBool());
}

test "value nil" {
    const nil = Value.init({});
    const notNil = Value.init(3);

    try std.testing.expect(nil.isNil());
    try std.testing.expect(try nil.asNil() == {});
    try std.testing.expect(notNil.isNil() == false);
    try std.testing.expectError(ValueError.NotNil, notNil.asNil());
}

test "value number" {
    const num = Value.init(42);
    const NaN = Value.init(false);

    try std.testing.expect(num.isNumber());
    try std.testing.expect(try num.asNumber() == @as(f64, 42));
    try std.testing.expect(NaN.isNumber() == false);
    try std.testing.expectError(ValueError.NotANumber, NaN.asNumber());
}

test "value string" {
    const string = Value.init("this is cool yo");
    const notString = Value.init(3);

    try std.testing.expect(string.isString());
    try std.testing.expect(std.mem.eql(u8, try string.asString(), "this is cool yo"));
    try std.testing.expect(notString.isString() == false);
    try std.testing.expectError(ValueError.NotAString, notString.asString());
}

test "value isTruthy" {
    const truthy1 = Value.init(true);
    const truthy2 = Value.init(0);
    const truthy3 = Value.init(42);
    const truthy4 = Value.init(-42);
    const truthy5 = Value.init("");
    const truthy6 = Value.init("this is true?");

    const falsey1 = Value.init(false);
    const falsey2 = Value.init({});

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
    const nil1 = Value.init({});
    const nil2 = Value.init({});
    const number1 = Value.init(42);
    const number2 = Value.init(42);
    const number3 = Value.init(0);
    const number4 = Value.init(-42);
    const number5 = Value.init(0.00);
    const bool1 = Value.init(true);
    const bool2 = Value.init(true);
    const bool3 = Value.init(false);
    const string1 = Value.init("hello how are you today?");
    const string2 = Value.init("hello how are you today?");
    const string3 = Value.init("");

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
