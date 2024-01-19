const std = @import("std");
const String = @import("string.zig").String;
const Span = @import("tokens.zig").Span;
const GPA = std.heap.GeneralPurposeAllocator;
const Allocator = std.mem.Allocator;

const DECORATION: []const u8 = "\x1B[1m\x1B[38;5;255m";

pub const Level = enum {
    const Self = @This();
    Info,
    Warn,
    Error,
    Fatal,
    Debug,

    pub fn is_error(self: Self) bool {
        switch (self) {
            Self.Error, Self.Fatal => return true,
            else => return false,
        }
    }

    pub fn get_level_color(self: Self) []const u8 {
        switch (self) {
            Self.Info => return "\x1B[1m\x1B[38;2;70;190;255m",
            Self.Warn => return "\x1B[1m\x1B[38;2;255;230;105m",
            Self.Error => return "\x1B[1m\x1B[38;2;255;115;115m",
            Self.Fatal => return "\x1B[1m\x1B[38;2;255;50;50m",
            Self.Debug => return "\x1B[1m\x1B[38;2;165;140;255m",
        }
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        const s = switch (self) {
            Self.Info => "info\x1B[0m",
            Self.Warn => "warning\x1B[0m",
            Self.Error => "error\x1B[0m",
            Self.Fatal => "fatal\x1B[0m",
            Self.Debug => "debug\x1B[0m",
        };

        try writer.print("{s}{s}", .{ self.get_level_color(), s });
    }
};

pub const Logger = struct {
    const Self = @This();
    const stderr = std.io.getStdErr().writer();
    const stdout = std.io.getStdOut().writer();
    pub fn log(level: Level, message: []const u8) !void {
        if (level.is_error()) {
            try stderr.print("{}: {s}{s}", .{ level, DECORATION, message });
            try stderr.print("\x1B[0m\n", .{}); // reset
        } else {
            try stdout.print("{}: {s}{s}", .{ level, DECORATION, message });
            try stdout.print("\x1B[0m\n", .{}); // reset
        }
    }

    pub fn get_file_line(path: []const u8, line: usize, allocator: Allocator) !String {
        var file = try std.fs.cwd().openFile(path, .{});
        const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        var str = String.from_alloced(contents, allocator);
        const lines = try str.split('\n');
        defer {
            lines.deinit();
            str.deinit();
        }
        return String.init(lines.items[line-1], allocator);
    }

    pub fn interpreter_log(level: Level, span: *Span, message: []const u8, line_mode: bool) !void {
        // Initial message
        const writer = if (level.is_error()) stderr else stdout;
        var gpa = GPA(.{}){};
        const allocator = gpa.allocator();
        try std.fmt.format(writer, "{s}: {s}{s}\x1B[0m\n", .{ level, DECORATION, message });

        // src file path and line number
        try std.fmt.format(writer, " \x1B[1m\x1B[38;5;012m-->\x1B[38;5;255m {s}:{}:{}\x1B[0m\n", .{ span.file_path, span.line, span.start });

        if (!line_mode) {
            if (Self.get_file_line(span.file_path, span.line, allocator)) |line| {
                try std.fmt.format(writer, "\x1B[1m\x1B[38;5;012m   |\n{:<3}|\x1B[0m {}\n", .{ span.line, line });
            } else |_| {
                try std.fmt.format(writer, "\x1B[1m\x1B[38;5;255mCould not read source for path `{s}`.", .{span.file_path});
                return;
            }

            // source code line
            // pointers
        }
    }

    pub fn interpreter_error(span: *Span, message: []const u8) !void {
        try Self.interpreter_log(Level.Error, span, message, false);
    }

    pub fn interpreter_traceback_call(spans: []Span, fn_names: [][]const u8, comptime message: []const u8, args: anytype) !void {
        const writer = stderr;
        var gpa = GPA(.{}){};
        const allocator = gpa.allocator();
        try std.fmt.format(writer, "Traceback (most recent function call last)\n\n", .{});
        for (spans, fn_names) |span, name| {
            try std.fmt.format(writer, "File \"{s}\", line: {d} in {s}: \n", .{span.file_path, span.line, name});
            if (Self.get_file_line(span.file_path, span.line, allocator)) |line| {
                try std.fmt.format(writer, "\x1B[1m\x1B[38;5;012m{s:>4}|\n{:>3} |\x1B[0m {}\n", .{ " ", span.line, line });
            } else |_| {
                try std.fmt.format(writer, "\x1B[1m\x1B[38;5;255m   Could not read source for path `{s}`.", .{span.file_path});
                return;
            }
            var padding = try String.init(" ", allocator);
            var pointers = try String.init("^", allocator);
            try padding.repeat(span.start);
            try pointers.repeat(span.end - span.start);
            try std.fmt.format(writer, "\x1B[1m\x1B[38;5;012m{s:<4}| {s}{s}{s}\n\n", .{
                " ",
                Level.Error.get_level_color(),
                padding.as_slice(),
                pointers.as_slice(),
            });
            try std.fmt.format(writer, "\x1B[0m", .{}); // reset
        }

        try stderr.print("{}: {s}", .{ Level.Error, DECORATION });
        try stderr.print(message, args);
        try stderr.print("\x1B[0m\n", .{}); // reset
    }
};
