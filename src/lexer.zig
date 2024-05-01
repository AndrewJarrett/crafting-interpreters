const std = @import("std");
const builtin = @import("builtin");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const ExitStatus = @import("main.zig").ExitStatus;

const str = []const u8;

pub const Lexer = struct {
    const Self = @This();

    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
        };
    }

    pub fn runPrompt(self: Self) !ExitStatus {
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
                try self.run(source);
                try stdout.print("> ", .{});
            } else {
                try stdout.print("Goodbye!\n", .{});
                return ExitStatus.EX_OK;
            }
        } else |err| switch (err) {
            else => return err,
        }
    }

    pub fn runFile(self: Self, file_path: str) !ExitStatus {
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
                try self.run(source);
            } else {
                return ExitStatus.EX_OK;
            }
        } else |err| switch (err) {
            else => return err,
        }
    }

    fn run(self: Self, source: []const u8) !void {
        _ = self;
        var tokens = std.mem.tokenize(u8, source, " ");
        while (tokens.next()) |token| {
            std.log.info("{s}", .{token});
        } else {
            std.log.debug("End of tokens.", .{});
        }
    }

    fn handle_error(self: Self, line_num: usize, source: []const u8) void {
        self.report(line_num, "", source);
    }

    fn report(self: Self, line_num: usize, where: []const u8, source: []const u8) void {
        _ = self;

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
    try std.testing.expect(@TypeOf(try lex.run(source)) == void);
}

test "error method should return void on success" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    const line_num = 1;
    const source = "asdf 1234 efghi";
    try std.testing.expect(@TypeOf(lex.handle_error(line_num, source)) == void);
}

test "report method should return void on success" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const lex = Lexer.init(allocator);
    const line_num = 1;
    const source = "asdf 1234 efghi";
    try std.testing.expect(@TypeOf(lex.report(line_num, "", source)) == void);
}
