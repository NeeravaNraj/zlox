const std = @import("std");
const String = @import("string.zig").String;
const Function = @import("function.zig").Function;
const Native = @import("native.zig").Native;

pub const Object = union(enum) {
    const Self = @This();
    const OperationError = error{ Negation, Add, Sub, Mul, Div, Equals, Comparison };

    None: void,
    Bool: bool,
    Float: f64,
    Int: i32,
    Str: String,
    Fn: Function,
    Native: Native,

    pub const Tag = std.meta.Tag(Self);
    // Get the type that is used to store the underlying enum int
    pub const EnumInt = @typeInfo(Tag).Enum.tag_type;
    // Make a new type that is twice as large as the underlying enum int
    // This is so that we can store two tags in one int
    pub const Single = std.meta.Int(.unsigned, @bitSizeOf(EnumInt) * 2);
    pub const Pair = std.meta.Int(.unsigned, @bitSizeOf(EnumInt) * 2);

    pub fn asInt(tag: Tag) EnumInt {
        return @intFromEnum(tag);
    }

    pub fn single(self: Tag) Single {
        return @as(Single, asInt(self));
    }

    // if we had only float and int
    // that would make `EnumInt` a u1, we can represent int with 0 and float with 1
    // now we get a pair (int, float) => int = 0, float = 1
    // shift int left by size of bits we get 00 (because pair is 2 * EnumInt.bits)
    // finally or the float into 00 we get 01 we can compare this now using switch
    pub fn pair(self: Tag, other: Tag) Pair {
        return @as(Pair, asInt(self)) << @bitSizeOf(EnumInt) | asInt(other);
    }

    pub fn negate(self: Self) OperationError!Self {
        return switch (self) {
            Self.Int => Self{ .Int = -self.Int },
            Self.Float => Self{ .Float = -self.Float },
            else => OperationError.Negation,
        };
    }

    pub fn not(self: Self) Self {
        return switch (self) {
            Self.Int => |v| Self{ .Bool = v == 0 },
            Self.Float => |v| Self{ .Bool = v == 0 },
            Self.Bool => |v| Self{ .Bool = !v },
            Self.Str => |v| Self{ .Bool = v.len == 0 },
            Self.None => Self{ .Bool = true },
            Self.Fn => Self{ .Bool = false },
            Self.Native => Self{ .Bool = false },
        };
    }

    pub fn type_name(self: Self) []const u8{
        return switch (self) {
            Self.Int => "int",
            Self.Float => "float",
            Self.Bool => "bool",
            Self.Str => "str",
            Self.None => "none",
            Self.Fn, Self.Native => "fn"
        };
    }

    pub fn add(self: Self, rhs: Self) OperationError!Self {
        switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => return Self{ .Int = self.Int + rhs.Int },
            Self.pair(.Int, .Float) => return Self{ .Float = @as(f64, @floatFromInt(self.Int)) + rhs.Float },
            Self.pair(.Float, .Int) => return Self{ .Float = self.Float + @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Float, .Float) => return Self{ .Float = self.Float + rhs.Float },
            Self.pair(.Str, .Str) => {
                var str: *String = @constCast(&self.Str);
                str.concat(rhs.Str.data[0..rhs.Str.len]) catch return OperationError.Add;
                return Object { .Str = str.* };
            },
            else => return OperationError.Add,
        }
    }

    pub fn sub(self: Self, rhs: Self) OperationError!Self {
        switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => return Self{ .Int = self.Int - rhs.Int },
            Self.pair(.Int, .Float) => return Self{ .Float = @as(f64, @floatFromInt(self.Int)) - rhs.Float },
            Self.pair(.Float, .Int) => return Self{ .Float = self.Float - @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Float, .Float) => return Self{ .Float = self.Float - rhs.Float },
            else => return OperationError.Sub,
        }
    }

    pub fn mul(self: Self, rhs: Self) OperationError!Self {
        switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => return Self{ .Int = self.Int * rhs.Int },
            Self.pair(.Int, .Float) => return Self{ .Float = @as(f64, @floatFromInt(self.Int)) * rhs.Float },
            Self.pair(.Float, .Int) => return Self{ .Float = self.Float * @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Float, .Float) => return Self{ .Float = self.Float * rhs.Float },
            else => return OperationError.Mul,
        }
    }

    pub fn div(self: Self, rhs: Self) OperationError!Self {
        switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => return Self{ .Int = @divTrunc(self.Int, rhs.Int) },
            Self.pair(.Int, .Float) => return Self{ .Float = @as(f64, @floatFromInt(self.Int)) / rhs.Float },
            Self.pair(.Float, .Int) => return Self{ .Float = self.Float / @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Float, .Float) => return Self{ .Float = self.Float / rhs.Float },
            else => return OperationError.Div,
        }
    }

    pub fn equals(self: Self, rhs: Self) OperationError!Self {
        if (self == .None and rhs != .None) return Self{ .Bool = false };
        if (self != .None and rhs == .None) return Self{ .Bool = false };
        switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => return Self{ .Bool = self.Int == rhs.Int },
            Self.pair(.Int, .Float) => return Self{ .Bool = @as(f64, @floatFromInt(self.Int)) == rhs.Float },
            Self.pair(.Float, .Int) => return Self{ .Bool = self.Float == @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Float, .Float) => return Self{ .Bool = self.Float == rhs.Float },
            Self.pair(.Int, .Bool) => return Self{ .Bool = self.Int == @as(i32, @intFromBool(rhs.Bool)) },
            Self.pair(.Bool, .Int) => return Self{ .Bool = @as(i32, @intFromBool(self.Bool)) == rhs.Int },
            Self.pair(.Str, .Str) => return Self{ .Bool = std.mem.eql(u8, self.Str.data[0..self.Str.len], rhs.Str.data[0..rhs.Str.len]) },
            Self.pair(.Bool, .Bool) => return Self{ .Bool = self.Bool == rhs.Bool },
            Self.pair(.None, .None) => return Self{ .Bool = true },
            else => return OperationError.Equals,
        }
    }

    pub fn lt(self: Self, rhs: Self) OperationError!Self {
        return switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => Self{ .Bool = self.Int < rhs.Int },
            Self.pair(.Float, .Float) => Self{ .Bool = self.Float < rhs.Float },
            Self.pair(.Float, .Int) => Self{ .Bool = self.Float < @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Int, .Float) => Self{ .Bool = @as(f64, @floatFromInt(self.Int)) < rhs.Float },
            Self.pair(.Bool, .Bool) => Self{ .Bool = @as(u8, @intFromBool(self.Bool)) < @as(u8, @intFromBool(rhs.Bool)) },
            Self.pair(.Bool, .Int) => Self{ .Bool = @as(i32, @intFromBool(self.Bool)) < rhs.Int },
            Self.pair(.Int, .Bool) => Self{ .Bool = self.Int < @as(i32, @intFromBool(rhs.Bool)) },
            Self.pair(.Str, .Str) => Self{ .Bool = self.Str.len < rhs.Str.len },
            else => OperationError.Comparison,
        };
    }

    pub fn le(self: Self, rhs: Self) OperationError!Self {
        return switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => Self{ .Bool = self.Int <= rhs.Int },
            Self.pair(.Float, .Float) => Self{ .Bool = self.Float <= rhs.Float },
            Self.pair(.Float, .Int) => Self{ .Bool = self.Float <= @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Int, .Float) => Self{ .Bool = @as(f64, @floatFromInt(self.Int)) <= rhs.Float },
            Self.pair(.Bool, .Bool) => Self{ .Bool = @as(u8, @intFromBool(self.Bool)) <= @as(u8, @intFromBool(rhs.Bool)) },
            Self.pair(.Bool, .Int) => Self{ .Bool = @as(i32, @intFromBool(self.Bool)) <= rhs.Int },
            Self.pair(.Int, .Bool) => Self{ .Bool = self.Int <= @as(i32, @intFromBool(rhs.Bool)) },
            Self.pair(.Str, .Str) => Self{ .Bool = self.Str.len <= rhs.Str.len },
            else => OperationError.Comparison,
        };
    }

    pub fn gt(self: Self, rhs: Self) OperationError!Self {
        return switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => Self{ .Bool = self.Int > rhs.Int },
            Self.pair(.Float, .Float) => Self{ .Bool = self.Float > rhs.Float },
            Self.pair(.Float, .Int) => Self{ .Bool = self.Float > @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Int, .Float) => Self{ .Bool = @as(f64, @floatFromInt(self.Int)) > rhs.Float },
            Self.pair(.Bool, .Bool) => Self{ .Bool = @as(u8, @intFromBool(self.Bool)) > @as(u8, @intFromBool(rhs.Bool)) },
            Self.pair(.Bool, .Int) => Self{ .Bool = @as(i32, @intFromBool(self.Bool)) > rhs.Int },
            Self.pair(.Int, .Bool) => Self{ .Bool = self.Int > @as(i32, @intFromBool(rhs.Bool)) },
            Self.pair(.Str, .Str) => Self{ .Bool = self.Str.len > rhs.Str.len },
            else => OperationError.Comparison,
        };
    }

    pub fn ge(self: Self, rhs: Self) OperationError!Self {
        return switch (Self.pair(self, rhs)) {
            Self.pair(.Int, .Int) => Self{ .Bool = self.Int >= rhs.Int },
            Self.pair(.Float, .Float) => Self{ .Bool = self.Float >= rhs.Float },
            Self.pair(.Float, .Int) => Self{ .Bool = self.Float >= @as(f64, @floatFromInt(rhs.Int)) },
            Self.pair(.Int, .Float) => Self{ .Bool = @as(f64, @floatFromInt(self.Int)) >= rhs.Float },
            Self.pair(.Bool, .Bool) => Self{ .Bool = @as(u8, @intFromBool(self.Bool)) >= @as(u8, @intFromBool(rhs.Bool)) },
            Self.pair(.Bool, .Int) => Self{ .Bool = @as(i32, @intFromBool(self.Bool)) >= rhs.Int },
            Self.pair(.Int, .Bool) => Self{ .Bool = self.Int >= @as(i32, @intFromBool(rhs.Bool)) },
            Self.pair(.Str, .Str) => Self{ .Bool = self.Str.len >= rhs.Str.len },
            else => OperationError.Comparison,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            Self.Float => |v| try writer.print("{d}", .{v}),
            Self.Int => |v| try writer.print("{}", .{v}),
            Self.Str => |v| try writer.print("{}", .{v}),
            Self.Bool => |v| try writer.print("{}", .{v}),
            Self.None => try writer.print("none", .{}),
            Self.Fn => |v| try writer.print("{}", .{v}),
            Self.Native => |v| try writer.print("{}", .{v}),
        }
    }

    pub fn is_fn(self: Self) bool {
        return single(self) == single(.Fn);
    }
};
