const std = @import("std");
const Object = @import("object.zig").Object;

pub fn clock(args: []const Object) Object {
    _ = args;
    return Object.float(@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000);
}

pub fn print(args: []const Object) Object {
    const writer = std.io.getStdOut().writer();

    for (args, 0..) |arg, i| {
        writer.print("{}", .{arg}) catch return Object.None;

        if (i < args.len - 1) 
            writer.print(" ", .{}) catch return Object.None;
    }
    writer.print("\n", .{}) catch return Object.None;

    return Object.None;
}
