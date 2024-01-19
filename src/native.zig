const std = @import("std");
const Object = @import("object.zig").Object;

pub const Native = struct {
    const Self = @This();
    pub const NativeFn = *const fn(args: []const Object) Object;
    name: []const u8,
    function: NativeFn,

    pub fn init(name: []const u8, function: NativeFn) Self {
        return Self { 
            .name = name,
            .function = function, 
        };
    }


    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("<fn '{s}'>", .{self.name});
    }
};
