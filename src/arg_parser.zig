const std = @import("std");
const logger = @import("logger.zig");
const String = @import("string.zig").String;
const Logger = logger.Logger;
const Level = logger.Level;
const Allocator = std.mem.Allocator;
const HashMap = std.StringHashMap;
const Args = std.process.ArgIterator;

const HELP = 
\\USAGE: lox [OPTIONS] [FILE] 

\\Options:
\\  -h,  --help         Displays this screen.
\\  -d,  --debug        Displays current instruction and stack at runtime.
\\  -op, --opcodes      Displays the all the opcodes after compilation.
\\  -t,  --tokens       Displays lexed tokens.
\\  -gc, --log-gc       Log garbage collector.
\\
;

pub const Option = enum(u16) {
    None,                       // No option
    Help            = 0x1,      // Help    
    DebugOpcodes    = 0x2,      // Log opcodes
    DebugTokens     = 0x4,      // Log tokens  
    DebugGc         = 0x8,      // Log gc   
    DebugTrace      = 0x10,      // Show debug trace
};

pub const Options = struct {
    file: ?[]const u8,
    args: u16,
};


pub fn parse_args(args: *Args, allocator: Allocator) !Options {
    var opts = HashMap(Option).init(allocator);
    try opts.put("-h", Option.Help);
    try opts.put("--help", Option.Help);
    try opts.put("-d", Option.DebugTrace);
    try opts.put("--debug", Option.DebugTrace);
    try opts.put("-op", Option.DebugOpcodes);
    try opts.put("--opcodes", Option.DebugOpcodes);
    try opts.put("-t", Option.DebugTokens);
    try opts.put("--tokens", Option.DebugTokens);
    try opts.put("-gc", Option.DebugGc);
    try opts.put("--log-gc", Option.DebugGc);

    var error_occurred = false;
    var options = Options {
        .file = null,
        .args = 0,
    };

    while (args.next()) |arg| {
        if (options.file != null) {
            const message = try std.fmt.allocPrint(allocator, "invalid argument location '{s}'", .{arg});
            try Logger.log(Level.Error, message);
            allocator.free(message);
            error_occurred = true;
            continue;
        }

        if (opts.get(arg)) |opt| {
            if (opt == Option.Help) {
                try std.io.getStdOut().writer().print("{s}", .{HELP});
                std.process.exit(0);
            }
            options.args |= @intFromEnum(opt);
            continue;
        } else if (arg[0] == '-') {
            const message = try std.fmt.allocPrint(allocator, "unknown argument '{s}'", .{arg});
            try Logger.log(Level.Error, message);
            allocator.free(message);
            error_occurred = true;
            break;
        }

        options.file = arg;
    }

    if (error_occurred) {
        try Logger.log(Level.Error, "Try `-h` or `--help` for usage.");
        std.process.exit(0);
    }

    return options;
}
