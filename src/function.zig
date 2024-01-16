const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const String = @import("string.zig").String;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Span = tokens.Span;

pub const Function = struct {
    const Self = @This();
    arity: u8,
    chunk: Chunk,
    name: String,
    source_map: ArrayList(Span),
    allocator: Allocator,

    pub fn init(name: []const u8, arity: u8, allocator: Allocator) !Self {
        return Self {
            .arity = arity,
            .name = try String.init(name, allocator),
            .chunk = Chunk.init(allocator),
            .source_map = ArrayList(Span).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn default(allocator: Allocator) !Self {
        return Self {
            .arity = 0,
            .name = try String.init("", allocator),
            .chunk = Chunk.init(allocator),
            .source_map = ArrayList(Span).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.name.len > 0) {
            try writer.print("<fn '{s}'>", .{self.name});
        }
        else try writer.print("<script>", .{});
    }
};
