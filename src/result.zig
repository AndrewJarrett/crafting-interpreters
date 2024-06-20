const std = @import("std");
const Token = @import("token.zig").Token;

const str = []const u8;

pub fn Result(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        ok: T,
        err: Error,

        pub fn ok(payload: T) Self {
            return Self {
                .ok = payload,
            };
        }

        pub fn err(payload: Error) Self {
            return Self {
                .err = payload,
            };
        }

        pub fn unwrap(self: Self) ResultError!T {
            switch (self) {
                .ok => |okay| return okay,
                .err => return ResultError.UnwrapError,
            }
        }

        pub fn deinit(self: Self) void {
            switch (self) {
                .err => {},
                .ok => |value| {
                    if (comptime std.meta.hasFn("deinit")(T)) {
                        value.deinit();
                    }
                }
            }
        }
    };
}

pub const ResultError = error {
    UnwrapError,
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
