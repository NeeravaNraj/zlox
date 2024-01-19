const std = @import("std");
const Object = @import("object.zig").Object;

pub fn clock(args: []const Object) Object {
    _ = args;
    return Object.float(@as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000);
}
