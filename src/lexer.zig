const std = @import("std");
const builtin = @import("builtin");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const ExitStatus = @import("main.zig").ExitStatus;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig").Token;
const Value = @import("token.zig").Value;
const Expr = @import("parser.zig").Expr;
const Parser = @import("parser.zig").Parser;
const InterpreterError = @import("interpreter.zig").InterpreterError;
const Interpreter = @import("interpreter.zig").Interpreter;

const str = []const u8;

pub const Lexer = struct {
    allocator: Allocator,
    var interp: Interpreter = undefined;

    pub fn init(allocator: Allocator) Lexer {
        return Lexer{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Lexer) void {
        self.interp.deinit();
    }

    pub fn runPrompt(self: Lexer) !ExitStatus {
        if (builtin.is_test) {
            // If running a test, don't run the REPL
            return ExitStatus.EX_OK;
        }

        var in = std.io.getStdIn();
        var buf = std.io.bufferedReader(in.reader());
        var input_stream = buf.reader();

        var lines = ArrayList(u8).init(self.allocator);
        defer lines.deinit();

        const stdout = std.io.getStdOut().writer();
        try stdout.print("Welcome to the zlox interpreter!\n", .{});
        try stdout.print("When you are done, press CTRL+D to quit.\n\n", .{});
        try stdout.print("> ", .{});

        var buffer: [1024]u8 = undefined;
        while (input_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            if (line) |source| {
                const result = self.run(source) catch {
                    continue; // ignore errors and try again
                };
                try stdout.print("{s}\n", .{result});
                try stdout.print("> ", .{});
            } else {
                try stdout.print("\nGoodbye!\n", .{});
                return ExitStatus.EX_OK;
            }
        } else |err| switch (err) {
            else => return err,
        }
    }

    pub fn runFile(self: Lexer, file_path: str) !ExitStatus {
        var file = std.fs.cwd().openFile(file_path, .{}) catch |err| switch (err) {
            error.FileNotFound => return ExitStatus.EX_NOINPUT,
            else => return err,
        };
        defer file.close();

        if (builtin.is_test) {
            // If running a test, don't run the file
            return ExitStatus.EX_OK;
        }

        var buf = std.io.bufferedReader(file.reader());
        var input_stream = buf.reader();

        var buffer: [1024]u8 = undefined;
        while (input_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            if (line) |source| {
                _ = self.run(source) catch |err| switch (err) {
                    error.InterpreterError => return ExitStatus.EX_SOFTWARE,
                    else => return ExitStatus.EX_DATAERR,
                };
            } else {
                return ExitStatus.EX_OK;
            }
        } else |err| switch (err) {
            else => return err,
        }
    }

    fn run(self: Lexer, source: str) !Value {
        var scanner = Scanner.init(self.allocator, source);
        defer scanner.deinit();

        const tokens = try scanner.scanTokens();
        std.log.info("Tokens: {s}", .{tokens.items});
        var parser = Parser.init(self.allocator, tokens);
        defer parser.deinit();
        const expr = try parser.parse();
        std.log.info("Expr: {s}", .{expr});

        interp = Interpreter.init(self.allocator, expr);
        const value = try interp.interpret();
        return value;
    }

    pub fn handleError(line_num: usize, source: str) void {
        report(line_num, "", source);
    }

    pub fn handleTokenError(token: Token, msg: str) void {
        if (token.tokenType == .EOF) {
            report(token.line, " at end", msg);
        } else {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const where: str = std.fmt.allocPrint(arena.allocator(), " at '{s}'", .{token.lexeme}) catch unreachable;
            report(token.line, where, msg);
        }
    }

    pub fn handleRuntimeError(token: Token, msg: str) void {
        std.debug.print("{s}\n[line {d}]", .{msg, token.line});
    }

    fn report(line_num: usize, where: str, source: str) void {
        std.debug.print("[line {d}] Error {s}: {s}", .{
            line_num,
            where,
            source,
        });
    }
};

test "init" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);

    try std.testing.expect(@TypeOf(lex) == Lexer);
}

test "run prompt" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    try std.testing.expect(try lex.runPrompt() == ExitStatus.EX_OK);
}

test "run file with existing file" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    try std.testing.expect(try lex.runFile("src/main.zig") == ExitStatus.EX_OK);
}

test "run file with file not found" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    try std.testing.expect(try lex.runFile("not_found.zlox") == ExitStatus.EX_NOINPUT);
}

test "run method should parse" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    const source = "asdf 1234 efghi";
    try std.testing.expect(@TypeOf(try lex.run(source)) == Value);
}

test "error method should return void on success" {
    const line_num = 1;
    const source = "asdf 1234 efghi";
    try std.testing.expect(@TypeOf(Lexer.handleError(line_num, source)) == void);
}

test "report method should return void on success" {
    const line_num = 1;
    const source = "asdf 1234 efghi";
    try std.testing.expect(@TypeOf(Lexer.report(line_num, "", source)) == void);
}
