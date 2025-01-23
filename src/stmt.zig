const std = @import("std");

const Expr = @import("expr.zig").Expr;
const HeapValue = @import("value.zig").HeapValue;
const Value = @import("value.zig").Value;
const Token = @import("token.zig").Token;
const Result = @import("result.zig").Result;
const Error = @import("result.zig").Error;
const ResultError = @import("result.zig").ResultError;
const Interpreter = @import("interpreter.zig").Interpreter;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const str = []const u8;

pub const Var = struct {
    token: Token,
    initializer: ?*Expr,

    pub fn init(token: Token, initializer: ?*Expr) *Var {
        var variable = Var{
            .token = token,
            .initializer = initializer,
        };

        return &variable;
    }

    pub fn format(self: Var, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {?}", .{self.token, self.initializer});
    }
};

pub const Stmt = union(enum) {
    expression: *Expr,
    print: *Expr,
    variable: *Var,

    pub fn expression(expr: *Expr) Stmt {
        return Stmt{
            .expression = expr,
        };
    }

    pub fn print(expr: *Expr) Stmt {
        return Stmt{
            .print = expr,
        };
    }
    
    pub fn variable(token: Token, initializer: ?*Expr) Stmt {
        return Stmt{
            .variable = Var.init(token, initializer)
        };
    }

    pub fn evaluate(self: Stmt, interp: *Interpreter) ResultError!Result(HeapValue) {
        return switch (self) {
            .expression => {
                return try self.expression.evaluate(interp);
            },
            .print => {
                const result = try self.print.evaluate(interp);
                const val = try result.unwrap();

                const writer = std.io.getStdOut().writer();
                writer.print("{s}\n", .{val}) catch return ResultError.PrintError; 

                return result;
            },
            .variable => {
                return if (self.variable.initializer) |init| {
                    // If the variable has been initialized, evaluate it and get the value
                    const result = try init.evaluate(interp);
                    const val = try result.unwrap();

                    std.debug.print("\n\nLexeme: {s}; Value: {s}", .{self.variable.token, val.value});
                    interp.environment.define(self.variable.token.lexeme, val.value);
                    return result;
                } else {
                    // If not initialized, then we choose to set the value to null
                    const val = HeapValue.init(interp.allocator, {});

                    std.debug.print("\n\nLexeme: {s}; Value: {s}", .{self.variable, val.value});
                    interp.environment.define(self.variable.token.lexeme, val.value);
                    return Result(HeapValue).ok(val);
                };
            },
        };
    }

    pub fn deinit(self: *const Stmt, alloc: Allocator) void {
        switch (self.*) {
            .expression => |expr| expr.deinit(alloc),
            .print => |p| p.deinit(alloc),
            .variable => |v| {
                //v.token.deinit();
                if (v.initializer) |init| {
                    init.deinit(alloc);
                }
            },
        }
    }

    pub fn format(self: Stmt, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .expression => |e| try writer.print("{s}", .{e}),
            .print => |p| try writer.print("{s}", .{p}),
            .variable => |v| try writer.print("{s}", .{v}),
        }
    }
};

test "init an expression statement" {
    const expr = Expr.initLiteral(std.testing.allocator, "1");
    const stmt = Stmt.expression(expr);
    defer stmt.deinit(std.testing.allocator);

    try std.testing.expect(@TypeOf(stmt) == Stmt);
    try std.testing.expect(@TypeOf(stmt.expression) == *Expr);
}

test "init a print statement" {
    const expr = Expr.initLiteral(std.testing.allocator, "1");
    const stmt = Stmt.print(expr);
    defer stmt.deinit(std.testing.allocator);

    try std.testing.expect(@TypeOf(stmt) == Stmt);
    try std.testing.expect(@TypeOf(stmt.print) == *Expr);
}

test "init a variable statement with an initializer" {
    const token = Token.init(std.testing.allocator, .VAR, "testVar", null, 1);
    const initializer = Expr.initVariable(std.testing.allocator, token);
    const stmt = Stmt.variable(token, initializer);
    defer stmt.deinit(std.testing.allocator);

    try std.testing.expect(@TypeOf(stmt) == Stmt);
    try std.testing.expect(@TypeOf(stmt.print) == *Expr);
}

test "evaluate an expression statement" {
    const expr = Expr.initLiteral(std.testing.allocator, 1);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    const stmt = Stmt.expression(expr);
    try stmts.append(stmt);

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    const result = try stmt.evaluate(&interp);
    const val = try result.unwrap();

    try std.testing.expect(try val.value.asNumber() == 1);
}

test "evaluate a print statement" {
    const expr = Expr.initLiteral(std.testing.allocator, 1);
    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    const stmt = Stmt.print(expr);
    try stmts.append(stmt);

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    const result = try stmt.evaluate(&interp);
    const val = try result.unwrap();

    try std.testing.expect(try val.value.asNumber() == 1);
}

test "evaluate a variable statement with initializer" {
    const token = Token.init(std.testing.allocator, .VAR, "testVar", 1, 1);
    const initializer = Expr.initVariable(std.testing.allocator, token);

    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    const stmt = Stmt.variable(token, initializer);
    try stmts.append(stmt);

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    const result = try stmt.evaluate(&interp);
    const val = try result.unwrap();

    try std.testing.expect(try val.value.asNumber() == 1);
}

test "evaluate a variable statement without initializer" {
    const token = Token.init(std.testing.allocator, .VAR, "testVar", null, 1);
    defer token.deinit();

    const initializer = Expr.initVariable(std.testing.allocator, token);

    var stmts = ArrayList(Stmt).init(std.testing.allocator);
    const stmt = Stmt.variable(token, initializer);
    try stmts.append(stmt);

    var interp = Interpreter.init(std.testing.allocator, stmts);
    defer interp.deinit();

    const result = try stmt.evaluate(&interp);
    const val = try result.unwrap();

    try std.testing.expect(try val.value.asNumber() == 1);
}
