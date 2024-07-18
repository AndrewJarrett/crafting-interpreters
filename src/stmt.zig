const std = @import("std");

const Expr = @import("expr.zig").Expr;
const HeapValue = @import("value.zig").HeapValue;
const Token = @import("token.zig").Token;
const Result = @import("result.zig").Result;
const ResultError = @import("result.zig").ResultError;
const Interpreter = @import("interpreter.zig").Interpreter;

const Allocator = std.mem.Allocator;
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
                return try self.variable.initializer.?.evaluate(interp);
            },
        };
    }

    pub fn deinit(self: *const Stmt, alloc: Allocator) void {
        switch (self.*) {
            .expression => |expr| expr.deinit(alloc),
            .print => |p| p.deinit(alloc),
            else => {},
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
