const std = @import("std");
const Token = @import("token.zig").Token;

const Allocator = std.mem.Allocator;
const str = []const u8;

pub fn Result(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        ok: *T,
        err: Error,

        pub fn ok(payload: *T) Self {
            return Self {
                .ok = payload,
            };
        }

        pub fn err(payload: Error) Self {
            return Self {
                .err = payload,
            };
        }

        pub fn unwrap(self: Self) ResultError!*T {
            switch (self) {
                .ok => |k| {
                    //std.debug.print("\nUnwrap: {s}", .{k.*});
                    return k;
                },
                .err => return ResultError.UnwrapError,
            }
        }

        pub fn deinit(self: Self) void {
            switch (self) {
                .err => {},
                .ok => |k| {
                    if (comptime std.meta.hasFn(T, "deinit")) {
                        k.deinit();
                    }
                }
            }
        }

        pub fn format(self: Self, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            switch (self) {
                .ok => |k| try writer.print("{s}", .{k.*}),
                .err => |e| try writer.print("{s}", .{e}),
            }
        }
    };
}

pub const ResultError = error {
    UnwrapError,
    PrintError,
};

pub const Error = struct {
    token: ?Token,
    message: str,

    pub fn init(token: ?Token, message: str) Error {
        return Error{
            .token = token,
            .message = message,
        };
    }

    pub fn format(self: Error, comptime fmt: str, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("({?}) {s}", .{ self.token, self.message });
    }
};
