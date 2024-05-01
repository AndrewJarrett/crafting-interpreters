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

        const writer = lines.writer();
        while (input_stream.streamUntilDelimiter(writer, '\n', null)) {
            try self.run(lines); // Call run for each line that is read
        } else |err| switch (err) {
            error.EndOfStream => return ExitStatus.EX_OK, // Exit if we reach EndOfStream / EOF / Ctrl+D
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

        var lines = ArrayList(u8).init(self.allocator);
        defer lines.deinit();

        const writer = lines.writer();
        while (input_stream.streamUntilDelimiter(writer, '\n', null)) {
            defer lines.clearRetainingCapacity();
        } else |err| switch (err) {
            error.EndOfStream => try self.run(lines), // Run the file once we reach the end of the stream
            else => return err,
        }

        return ExitStatus.EX_OK;
    }

    fn run(self: Self, lines: ArrayList(u8)) !void {
        _ = self;
        std.debug.print("Lines to tokenize: {}", .{lines});
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
    const lines = ArrayList(u8).init(allocator);
    try std.testing.expect(@TypeOf(try lex.run(lines)) == void);
}