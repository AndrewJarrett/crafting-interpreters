const std = @import("std");
const Token = @import("token.zig").Token;
const Interpreter = @import("interpreter.zig").Interpreter;
const Value = @import("value.zig").Value;
const HeapValue = @import("value.zig").HeapValue;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;
const Stmt = @import("stmt.zig").Stmt;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const str = []const u8;

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
                    return Result(HeapValue).ok(HeapValue.initWithFree(interp.allocator, result, true));
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
    value: ?*const HeapValue = null,

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

pub const Variable = struct {
    name: Token,

    fn evaluate(self: Variable, interp: *Interpreter) ResultError!Result(HeapValue){
        _ = interp;
        if (self.name.literal) |val| {
            return Result(HeapValue).ok(val);
        } else {
            return Result(HeapValue).err(Error.init(self.name, "Expected a literal value for the variable"));
        }
    }
};

pub const Expr = union(enum) {
    Binary: Binary,
    Unary: Unary,
    Literal: Literal,
    Grouping: Grouping,
    Variable: Variable,

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

    pub fn initVariable(alloc: Allocator, name: Token) *Expr {
        const ptr = alloc.create(Expr) catch unreachable;
        ptr.* = Expr{ .Variable = Variable{ .name = name }};
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
            .Variable => |v| {
                v.name.deinit();
            },
        }

        // Must destroy self from the heap
        alloc.destroy(self);
    }

    pub fn evaluate(self: Expr, interp: *Interpreter) ResultError!Result(HeapValue) {
        return switch (self) {
            .Binary => try self.Binary.evaluate(interp),
            .Unary => try self.Unary.evaluate(interp),
            .Literal => self.Literal.evaluate(interp),
            .Grouping => try self.Grouping.evaluate(interp),
            .Variable => try self.Variable.evaluate(interp),
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
            .Variable => |v| try writer.print("var {s}", .{v.name}),
        }
    }
};

test "Expr: 1" {
    const expr = Expr.initLiteral(std.testing.allocator, 1.0);
    defer expr.deinit(std.testing.allocator);

    try std.testing.expect(testExprMatchesExpected("1", expr.*));
}

test "Expr: (+ 1 2)" {
    const expr = Expr.initBinary( 
        std.testing.allocator,
        Expr.initLiteral(std.testing.allocator, 1.0), 
        Token.init(std.testing.allocator, .PLUS, "+", null, 1), 
        Expr.initLiteral(std.testing.allocator, 2.0)
    );
    defer expr.deinit(std.testing.allocator);

    try std.testing.expect(testExprMatchesExpected("(+ 1 2)", expr.*));
}

test "Expr: (* (- 123) (group 45.67))" {
    const minus = Token.init(std.testing.allocator, .MINUS, "-", null, 1);
    const mult = Token.init(std.testing.allocator, .STAR, "*", null, 1);
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
