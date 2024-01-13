const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;
const Allocator = std.mem.Allocator;
const arg_parser = @import("arg_parser.zig");
const Options = arg_parser.Options;
const Option = arg_parser.Option;

pub const Vm = struct {
    const Self = @This();
    const InterpreterError = error {
        Compile,
        Interpreter,
    };

    file: []const u8,
    chunk: Chunk,
    options: Options,
    ip: usize,
    allocator: Allocator,

    pub fn init(file: []const u8, options: Options, allocator: Allocator) Self {
        return Self {
            .file = file,
            .chunk = Chunk.init(allocator),
            .ip = 0,
            .options = options,
            .allocator = allocator,
        };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        defer self.deinit();

        var compiler = Compiler.init(self.file, &self.chunk, self.options, self.allocator);
        compiler.compile(source) catch return InterpreterError.Compile;

        try self.run();
    }

    fn run(self: *Self) InterpreterError!void {
        _ = self;
    }

    fn deinit(self: *Self) void {
        self.chunk.deinit();
    }
};
