const std = @import("std");

const Expr = @import("parser.zig").Expr;
const HeapValue = @import("value.zig").HeapValue;
const Result = @import("result.zig").Result;
const ResultError = @import("result.zig").ResultError;
const Interpreter = @import("interpreter.zig").Interpreter;

const Allocator = std.mem.Allocator;
const str = []const u8;

pub const Stmt = union(enum) {
    expression: *Expr,
    print: *Expr,

    pub fn print(expr: *Expr) Stmt {
        return Stmt{
            .print = expr,
        };
    }

    pub fn expression(expr: *Expr) Stmt {
        return Stmt{
            .expression = expr,
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
        };
    }

    pub fn deinit(self: *const Stmt, alloc: Allocator) void {
        switch (self.*) {
            .expression => |expr| expr.deinit(alloc),
            .print => |p| p.deinit(alloc),
        }
    }

    pub fn format(self: Stmt, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .expression => |e| try writer.print("{s}", .{e}),
            .print => |p| try writer.print("{s}", .{p}),
        }
    }
};
