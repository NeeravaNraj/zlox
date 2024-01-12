const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Object = @import("object.zig").Object;
const Opcodes = @import("opcodes.zig").Opcodes;

pub const Line = struct {
    line: usize,
    start: usize,
    end: ?usize,
};

pub const Chunk = struct {
    const Self = @This();
    code: ArrayList(u8),
    constants: ArrayList(Object),
    lines: ArrayList(Line),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self {
            .code = ArrayList(u8).init(allocator),
            .constants = ArrayList(Object).init(allocator),
            .lines = ArrayList(Line).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn write(self: *Self, op_code: Opcodes, line: usize) !void {
        try self.code.append(@intFromEnum(op_code));
        if (self.lines.getLastOrNull()) |last| {
            const line_end = &self.lines.items[self.line_len() - 1];
            line_end.*.end = self.code_len() - 1;

            if (line > last.line) try self.lines.append(Line {
                .line = line,
                .start = self.code_len() - 1,
                .end = null
            });

        } else {
            try self.lines.append(Line {
                .line = line,
                // start stores the starting index of a instruction for the particular Line
                .start = self.code_len() - 1,
                // end stores the ending index
                .end = null,
            });
        }
    }

    pub fn write_constant(self: *Self, value: Object, line: usize) !void {
        const size: usize = @intCast(@bitSizeOf(usize) / 8);
        const index = self.constants_len();
        try self.constants.append(value);
        try self.write(Opcodes.Constant, line);
        // we store the size of how many bytes we want to read
        // one byte before the actual value
        // might be unnecessary but I will roll with this for now
        // a maximum of 255 bytes of size is plenty for this
        try self.code.append(size);
        // encode a usize into u8's to store in the code section
        // as indexing into arrays is best done with usize
        for (0..size) |i| {
            const shift_by: u6 = @intCast(size * i);
            const byte: u8 = @intCast((index >> shift_by) & 0xFF);
            try self.code.append(byte);
        }
    }

    pub fn read_constant(self: *Self, offset: usize, change: *usize) usize {
        const size = self.code.items[offset + 1];
        var value: usize = 0;
        const pointer = offset + 2;

        for (0..size) |i| {
            const byte: usize = @intCast(self.code.items[pointer + i]);
            const shift_by: u6 = @intCast(size * i);
            value |= byte << shift_by;
        }

        change.* += @intCast(size + 2);
        return value;
    }

    pub fn get_line(self: *Self, index: usize) ?usize {
        for (self.lines.items, 0..) |line, i| {
            if (line.end) |end| {
                if (index >= line.start and index < end) return line.line;
                if (self.line_len() - 1 == i and index == end) return line.line;
                continue;
            }

            if (index >= line.start) return line.line;
        }

        return null;
    }


    pub fn check_previous(self: *Self, offset: usize) bool {
        var current: ?usize = null;
        var previous: ?usize = null;
        if (self.get_line(offset)) |c| {
            current = c;
        } else return false;

        if (self.get_line(offset - 1)) |p| {
            previous = p;
        } else return false;

        if (current == previous) {
            return true;
        }

        return false;
    }


    pub fn code_len(self: *Self) usize {
        return self.code.items.len;
    }

    pub fn constants_len(self: *Self) usize {
        return self.constants.items.len;
    }

    fn line_len(self: *Self) usize {
        return self.lines.items.len;
    }

    fn deinit(self: *Self) void {
        self.lines.deinit();
        self.code.deinit();
        self.constants.deinit();
    }
};
