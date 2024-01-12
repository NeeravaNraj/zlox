const std = @import("std");
const String = @import("string.zig").String;

pub const Object = union (enum) {
    const Self = @This();
    Float: f64,
    Int: i32,
    Str: String,

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            Self.Float => |v| try writer.print("{d}", .{v}),
            Self.Int => |v| try writer.print("{}", .{v}),
            Self.Str => |v| try writer.print("'{}'", .{v}),
        }
    }
};
