const std = @import("std");
const Span = @import("tokens.zig").Span;
const Lexer = @import("lexer.zig").Lexer;
const String = @import("string.zig").String;
const logger = @import("logger.zig");
const arg_parser = @import("arg_parser.zig");
const fs = std.fs;
const Allocator = std.mem.Allocator;
const GPA = std.heap.GeneralPurposeAllocator;
const Logger = logger.Logger;
const Level = logger.Level;

fn readFile(path: []const u8, allocator: Allocator) ![]u8 {
    var file = try std.fs.cwd().openFile(path, .{});
    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    return contents;
}

pub fn main() !void {
    var gpa = GPA(.{}){};
    const allocator = gpa.allocator();
    var args = std.process.args();
    _ = args.skip();

    const options = try arg_parser.parse_args(&args, allocator);
    if (options.file) |file| {
        const contents = readFile(file, allocator) catch {
            const message = try std.fmt.allocPrint(allocator, "could not open file '{s}'", .{file});
            try Logger.log(Level.Fatal, message);
            allocator.free(message);
            std.process.exit(1);
        };
        var lexer = Lexer.init(file, contents[0..contents.len-1], options, allocator);
        const tokens = try lexer.tokenize();
        defer {
            allocator.free(contents);
            tokens.deinit();
        }
    } else {
        std.debug.print("repl\n", .{});
    }
}
