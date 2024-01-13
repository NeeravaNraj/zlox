const std = @import("std");
const logger = @import("logger.zig");
const arg_parser = @import("arg_parser.zig");
const Vm = @import("vm.zig").Vm;
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
    // var chunk = Chunk.init(allocator);
    // defer chunk.deinit();
    // try chunk.write(Opcodes.Add, 1);
    // for (1..20) |i| {
    //     try chunk.write_constant(Object { .Int = @intCast(i) }, i);
    // }
    // var disasm = Disassembler.init();
    // try disasm.disassemble_chunk(&chunk);
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

        var vm = Vm.init(file, options, allocator);
        try vm.interpret(contents);

    } else {
        std.debug.print("repl\n", .{});
    }
}
