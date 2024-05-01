const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;

const str = []const u8;

pub const ExitStatus = enum(u8) {
    EX_OK = 0,
    EX_USAGE = 64,
    EX_NOINPUT = 66,
    EX_SOFTWARE = 70,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const status = parseArgs(allocator, args) catch |err| switch (err) {
        else => ExitStatus.EX_SOFTWARE, // Generic internal error
    };

    switch (status) {
        ExitStatus.EX_USAGE => std.debug.print("Usage: zlox [source file]\n", .{}),
        ExitStatus.EX_NOINPUT => std.debug.print("File not found: {s}\n", .{args[1]}),
        ExitStatus.EX_SOFTWARE => std.debug.print("Unknown error occurred!\n", .{}),
        else => std.log.debug("ExitStatus: {}\n", .{status}),
    }

    std.process.exit(@intFromEnum(status));
}

fn parseArgs(allocator: Allocator, args: []const str) !ExitStatus {
    const lex = Lexer.init(allocator);

    return switch (args.len) {
        1 => try lex.runPrompt(),
        2 => try lex.runFile(args[1]),
        else => ExitStatus.EX_USAGE,
    };
}

test "run prompt" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args: []const str = &.{"zlox"};
    try std.testing.expect(try parseArgs(allocator, args) == ExitStatus.EX_OK);
}

test "run file" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args: []const str = &.{ "zlox", "src/main.zig" };
    try std.testing.expect(try parseArgs(allocator, args) == ExitStatus.EX_OK);
}

test "too many arguments" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args: []const str = &.{ "zlox", "one", "too_many" };
    try std.testing.expect(try parseArgs(allocator, args) == ExitStatus.EX_USAGE);
}

test "file not found" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args: []const str = &.{ "zlox", "not_found.zlox" };
    try std.testing.expect(try parseArgs(allocator, args) == ExitStatus.EX_NOINPUT);
}
