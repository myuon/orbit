const std = @import("std");
const compiler = @import("compiler.zig");

pub fn main() !void {
    var gpallocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        switch (gpallocator.deinit()) {
            .leak => {
                std.debug.print("**Leaked memory**", .{});
            },
            else => {},
        }
    }

    var c = compiler.Compiler.init(gpallocator.allocator());
    defer c.deinit();

    const argv = std.os.argv;
    if (argv.len < 2) {
        return std.debug.print("Usage: orbit [fib, prime, debugger]", .{});
    }

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const command = argv[1][0..std.mem.len(argv[1])];
    if (std.mem.eql(u8, command, "fib")) {
        const result = try c.evalModule(
            \\fun fib(n) do
            \\  if (n == 0) do
            \\    return 1;
            \\  end
            \\  if (n == 1) do
            \\    return 1;
            \\  end
            \\
            \\  return fib(n - 1) + fib(n - 2);
            \\end
            \\
            \\fun main() do
            \\  return fib(35);
            \\end
        );

        try stdout.print("Result: {any}\n", .{result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "prime")) {
        const result = try c.evalModule(
            \\fun is_prime(n) do
            \\  if (n < 2) do
            \\    return false;
            \\  end
            \\
            \\  let i = 2;
            \\  while (i * i <= n) do
            \\    if (n % i == 0) do
            \\      return false;
            \\    end
            \\    i = i + 1;
            \\  end
            \\
            \\  return true;
            \\end
            \\
            \\fun main() do
            \\  let n = 1000;
            \\  let sum = 0;
            \\
            \\  while (n > 0) do
            \\    n = n - 1;
            \\
            \\    if (is_prime(n)) do
            \\      sum = sum + n;
            \\    end
            \\  end
            \\
            \\  return sum;
            \\end
        );

        try stdout.print("Result: {any}\n", .{result});

        try bw.flush(); // don't forget to flush!
    } else {
        return std.debug.print("Unknown command: {s}", .{command});
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test {
    _ = @import("utils.zig");
    _ = @import("ast.zig");
    _ = @import("compiler.zig");
}
