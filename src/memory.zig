const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn alloc(comptime T: type, size: usize, allocator: Allocator) ![]T {
    return allocator.alloc(T, size);
}

pub fn realloc(old: anytype, size: usize, allocator: Allocator) !@TypeOf(old) {
    return allocator.realloc(old, size);
}

pub fn free(memory: anytype, allocator: Allocator) void {
    return allocator.free(memory);
}

pub inline fn align_len(length: usize) usize {
    return (length - (length % 4));
}
