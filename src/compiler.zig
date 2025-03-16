const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typecheck = @import("typecheck.zig");
const vm = @import("vm.zig");
const monomorphization = @import("monomorphization.zig");
const runtime = @import("runtime.zig");
const jit = @import("jit.zig");
const utils = @import("utils.zig");
const P = @import("profiler");

pub const EvalError = error{
    VariableNotFound,
    UnexpectedNilValue,
};

pub const CompilerError = error{
    CannotCompileToIr,
};

pub const Compiler = struct {
    jit_cache: std.StringHashMap(jit.CompiledFn),
    allocator: std.mem.Allocator,
    arena_allocator: std.heap.ArenaAllocator,
    tc: typecheck.Typechecker,
    vmc: vm.VmCompiler,
    mono: monomorphization.Monomorphization,
    enable_jit: bool,
    dump_ir_path: ?[]const u8 = null,
    dump_mono_ast_path: ?[]const u8 = null,
    enable_optimize_ir: bool = true,
    ir: ?[]ast.Instruction,
    result: ?ast.Value,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .allocator = allocator,
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .tc = typecheck.Typechecker.init(allocator),
            .vmc = vm.VmCompiler.init(allocator),
            .mono = monomorphization.Monomorphization.init(allocator),
            .enable_jit = true,
            .enable_optimize_ir = true,
            .ir = null,
            .result = null,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.jit_cache.deinit();
        self.tc.deinit();
        self.vmc.deinit();
        self.mono.deinit();
        self.arena_allocator.deinit();
    }

    pub fn compile(
        self: *Compiler,
        str: []const u8,
        execute: bool,
    ) anyerror!void {
        const zone = P.begin(@src(), "Compiler.compile");
        defer zone.end();

        var stdlib = std.ArrayList(u8).init(self.allocator);
        defer stdlib.deinit();

        try utils.readFile(self.allocator, "./lib/std.ob", &stdlib);

        const input = try std.fmt.allocPrint(self.arena_allocator.allocator(), "{s}\n{s}", .{ stdlib.items, str });

        var l = lexer.Lexer.init(self.allocator, input);
        defer l.deinit();

        const tokens = try l.run();

        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        var module = try p.module();

        try self.tc.typecheck(&module);

        const entrypoint = "main";

        try self.mono.execute(entrypoint, &module);
        if (self.dump_mono_ast_path) |path| {
            const file = try std.fs.cwd().createFile(path, .{});
            defer file.close();

            try std.fmt.format(file.writer(), "{any}\n", .{module});
        }

        var ir = try self.vmc.compile(entrypoint, module);

        if (self.enable_optimize_ir) {
            ir = try self.vmc.optimize(ir);
        }

        self.ir = ir;

        if (self.dump_ir_path) |path| {
            const file = try std.fs.cwd().createFile(path, .{});
            defer file.close();

            for (self.ir.?) |inst| {
                switch (inst) {
                    .label => |label| {
                        try std.fmt.format(file.writer(), "{s}:\n", .{label});
                    },
                    else => {
                        try std.fmt.format(file.writer(), "    {s}\n", .{inst});
                    },
                }
            }
        }

        if (execute) {
            var stack = try std.ArrayList(i64).initCapacity(self.allocator, 1024);
            defer stack.deinit();

            try stack.append(-2); // return value
            try stack.append(-1); // pc
            try stack.append(0); // bp

            var bp: i64 = @intCast(stack.items.len);

            var vmr = runtime.VmRuntime.init(self.allocator);
            defer vmr.deinit();

            vmr.enable_jit = self.enable_jit;

            try vmr.run(self.ir.?, &stack, &bp);

            self.result = ast.Value{ .i64_ = stack.items[0] };
        }
    }
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("vm.zig");
    _ = @import("jit.zig");
    _ = @import("runtime.zig");
}

test "compiler.parse_err" {
    const cases = comptime [_]struct {
        program: []const u8,
        err: ?anyerror,
    }{
        .{
            .program =
            \\fun f(x) do
            \\  let y = 10;
            \\
            \\  return x + y;
            \\end
            \\
            \\fun main() do
            \\  return f(3);
            \\end
            ,
            .err = null,
        },
        .{
            .program =
            \\fun f(x) do
            \\  if (x == 0) do
            \\    return 1;
            \\  end
            \\end
            ,
            .err = null,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(std.testing.allocator, case.program);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(std.testing.allocator, tokens.items);
        defer p.deinit();

        if (case.err != null) {
            _ = p.module() catch |err| {
                try std.testing.expectEqual(err, case.err);
            };
        } else {
            _ = p.module() catch |err| {
                std.debug.print("Unexpected error: {any}\n", .{err});
                try std.testing.expect(false);
            };
        }
    }
}

test "compiler.parseStatement" {
    const cases = comptime [_]struct {
        program: []const u8,
        expr: ast.Statement,
    }{
        .{
            .program =
            \\if (x == 1) do
            \\  return 1;
            \\end
            ,
            .expr = ast.Statement{ .if_ = .{
                .cond = @constCast(&ast.Expression{ .binop = .{
                    .op = ast.Operator.eqeq,
                    .lhs = @constCast(&ast.Expression{ .var_ = "x" }),
                    .rhs = @constCast(&ast.Expression{ .literal = ast.Literal{ .number = 1 } }),
                } }),
                .then_ = .{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 1 } } },
                    }),
                    .expr = null,
                },
                .else_ = null,
            } },
        },
        .{
            .program =
            \\if (x == 1) do
            \\  return 1;
            \\else
            \\  return 2;
            \\end
            ,
            .expr = ast.Statement{ .if_ = .{
                .cond = @constCast(&ast.Expression{ .binop = .{
                    .op = ast.Operator.eqeq,
                    .lhs = @constCast(&ast.Expression{ .var_ = "x" }),
                    .rhs = @constCast(&ast.Expression{ .literal = ast.Literal{ .number = 1 } }),
                } }),
                .then_ = .{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 1 } } },
                    }),
                    .expr = null,
                },
                .else_ = ast.Block{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 2 } } },
                    }),
                    .expr = null,
                },
            } },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(std.testing.allocator, case.program);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(std.testing.allocator, tokens.items);
        defer p.deinit();

        const e = try p.statement();

        try std.testing.expectEqualDeep(case.expr, e);
    }
}

test "compiler.compileLabel" {
    const cases = [_]struct {
        program: []const u8,
        expected: [][]const u8,
    }{
        .{
            .program =
            \\fun get_first(A: type, B: type, a: A, b: B): A do
            \\  return a;
            \\end
            \\
            \\fun main() do
            \\  get_first(type int, type [*]byte, 1, "Hello, ");
            \\  get_first(type [*]int, type int, 1, 2);
            \\end
            ,
            .expected = @constCast(&[_][]const u8{ "get_first_int_[*]byte", "get_first_[*]int_int" }),
        },
        .{
            .program =
            \\fun generic(A: type) do
            \\  return 0;
            \\end
            \\
            \\fun main() do
            \\  generic(type int);
            \\  generic(type [*]byte);
            \\end
            ,
            .expected = @constCast(&[_][]const u8{ "generic_int", "generic_[*]byte" }),
        },
        .{
            .program =
            \\type Pair(A: type, B: type) = struct {
            \\  first: A,
            \\  second: B,
            \\
            \\  fun set_first(self: Pair(A, B), a: A) do
            \\    self.first = a;
            \\  end
            \\};
            \\
            \\fun main() do
            \\  let p = new Pair(int, [*]byte) { .first = 1, .second = "hello, world!" };
            \\  p.set_first(3);
            \\end
            ,
            .expected = @constCast(&[_][]const u8{"Pair_int_[*]byte_set_first"}),
        },
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try c.compile(case.program, false);

        var labels = std.ArrayList([]const u8).init(std.testing.allocator);
        defer labels.deinit();

        for (c.ir.?) |inst| {
            switch (inst) {
                .label => |label| {
                    try labels.append(label);
                },
                else => {},
            }
        }

        for (case.expected) |expected| {
            var found = false;
            for (labels.items) |label| {
                if (std.mem.eql(u8, label, expected)) {
                    found = true;
                    break;
                }
            }

            std.testing.expect(found) catch |err| {
                std.log.err("Expected label {s} not found in {s}\n", .{ expected, labels.items });
                return err;
            };
        }
    }
}

test "compiler.evalModule" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: i64,
    }{
        .{
            .program =
            \\fun f(x) do
            \\  let y = 10;
            \\
            \\  return x + y;
            \\end
            \\
            \\fun main() do
            \\  return f(3);
            \\end
            ,
            .expected = 13,
        },
        .{
            .program =
            \\fun sum(x) do
            \\  if (x == 0) do
            \\    return 0;
            \\  end
            \\
            \\  return sum(x - 1) + x;
            \\end
            \\
            \\fun main() do
            \\  return sum(10);
            \\end
            ,
            .expected = 55,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = 0;
            \\  let n = 0;
            \\  while (n < 10) do
            \\    s = s + n;
            \\    n = n + 1;
            \\  end
            \\
            \\  return s;
            \\end
            ,
            .expected = 45,
        },
        .{
            .program =
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
            \\  return fib(15);
            \\end
            ,
            .expected = 987,
        },
        .{
            .program =
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
            ,
            .expected = 76127,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = "Hello, World!";
            \\
            \\  return s[5];
            \\end
            ,
            .expected = 44,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = new slice(int) { .ptr = allocate_memory(100 * 8) as [*]int, .len = 100 };
            \\  let n = 0;
            \\  while (n < 100) do
            \\    s[n] = n;
            \\    n = n + 1;
            \\  end
            \\
            \\  return s[50];
            \\end
            ,
            .expected = 50,
        },
        .{
            .program =
            \\fun main() do
            \\  let n = 0;
            \\  let a = 1;
            \\  let b = 1;
            \\  while (n < 30) do
            \\    let c = a + b;
            \\    a = b;
            \\    b = c;
            \\    n = n + 1;
            \\  end
            \\
            \\  return a;
            \\end
            ,
            .expected = 1346269,
        },
        .{
            .program =
            \\fun main() do
            \\  let k = new map([*]byte, int) {};
            \\  k["hello"] = 1;
            \\  k["world"] = 2;
            \\
            \\  return k["hello"] + k["world"];
            \\end
            ,
            .expected = 3,
        },
        .{
            .program =
            \\fun main() do
            \\  let k = new map([*]byte, int) {};
            \\  k["a"] = 1;
            \\  k["b"] = 2;
            \\  k["a"] = 3;
            \\
            \\  return k["a"] + k["b"];
            \\end
            ,
            .expected = 5,
        },
        .{
            .program =
            \\fun main() do
            \\  let vec = new vec(int) {};
            \\  let n = 0;
            \\  while (n < 100) do
            \\    vec <- n * n;
            \\    n = n + 1;
            \\  end
            \\
            \\  vec[77] = vec[77] + 10;
            \\
            \\  return vec[77];
            \\end
            ,
            .expected = 5939,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = new struct{ a: int, b: int } { .a = 10, .b = 20 };
            \\  s.a = 30;
            \\
            \\  return s.a - s.b;
            \\end
            ,
            .expected = 10,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = new slice(int) { .ptr = allocate_memory(10 * 8) as [*]int, .len = 10 };
            \\  let n = 0;
            \\  while (n < 10) do
            \\    if (n == 0) do
            \\      s[n] = 1;
            \\    else
            \\      if (n == 1) do
            \\        s[n] = 1;
            \\      else
            \\        s[n] = s[n-1] + s[n-2];
            \\      end
            \\    end
            \\    n = n + 1;
            \\  end
            \\
            \\  return s[9];
            \\end
            ,
            .expected = 55,
        },
        .{
            .program =
            \\let global = 100;
            \\
            \\fun increment() do
            \\  global = global + 50;
            \\  return 0;
            \\end
            \\
            \\fun main() do
            \\  let local = 200;
            \\  increment();
            \\
            \\  return global + local;
            \\end
            ,
            .expected = 350,
        },
        .{
            .program =
            \\type Point = struct { x: int, y: int };
            \\
            \\fun main() do
            \\  let p = new Point { .x = 10, .y = 20 };
            \\  p.x = 33;
            \\
            \\  return p.x - p.y;
            \\end
            ,
            .expected = 13,
        },
        .{
            .program =
            \\fun main() do
            \\  return 0x12345678;
            \\end
            ,
            .expected = 305419896,
        },
        .{
            .program =
            \\fun get_first(A: type, B: type, a: A, b: B): A do
            \\  return a;
            \\end
            \\
            \\fun get_second(A: type, B: type, a: A, b: B): B do
            \\  return b;
            \\end
            \\
            \\fun main() do
            \\  let a = get_first(type int, type [*]byte, 1, "Hello, ");
            \\  let b = get_second(type [*]byte, type int, "World!", 3);
            \\
            \\  return a + b;
            \\end
            ,
            .expected = 4,
        },
        .{
            .program =
            \\type Pair(A: type, B: type) = struct {
            \\  first: A,
            \\  second: B,
            \\};
            \\
            \\fun main() do
            \\  let p = new Pair(int, int) { .first = 1, .second = 2 };
            \\
            \\  return p.first + p.second;
            \\end
            ,
            .expected = 3,
        },
        .{
            .program =
            \\type Point = struct {
            \\  x: int,
            \\  y: int,
            \\
            \\  fun sum(self: Point) do
            \\    return self.x + self.y;
            \\  end
            \\
            \\  fun move(self: Point, dx: int, dy: int) do
            \\    self.x = self.x + dx;
            \\    self.y = self.y + dy;
            \\
            \\    return 0;
            \\  end
            \\};
            \\
            \\fun main() do
            \\  let p = new Point { .x = 10, .y = 20 };
            \\  p.move(5, 6);
            \\
            \\  return p.sum();
            \\end
            ,
            .expected = 41,
        },
        .{
            .program =
            \\type Pair(A: type, B: type) = struct {
            \\  first: A,
            \\  second: B,
            \\
            \\  fun set_first(self: Pair(A, B), a: A): int do
            \\    self.first = a;
            \\
            \\    return 0;
            \\  end
            \\};
            \\
            \\fun main() do
            \\  let p = new Pair(int, [*]byte) { .first = 1, .second = "hello, world" };
            \\  p.set_first(3);
            \\
            \\  return p.first + (p.second[3] as int);
            \\end
            ,
            .expected = 111,
        },
        .{
            .program =
            \\fun main() do
            \\  return sizeof [*]byte;
            \\end
            ,
            .expected = 8,
        },
        .{
            .program =
            \\fun main() do
            \\  return sizeof byte;
            \\end
            ,
            .expected = 1,
        },
        .{
            .program =
            \\type Container(A: type) = struct {
            \\  value: A,
            \\
            \\  fun get_size(self: Container(A)) do
            \\    return sizeof A;
            \\  end
            \\};
            \\
            \\fun main() do
            \\  let c = new Container(int) { .value = 10 };
            \\
            \\  return c.get_size();
            \\end
            ,
            .expected = 8,
        },
        .{
            .program =
            \\type Container(A: type) = struct {
            \\  value: A,
            \\
            \\  fun get_size(self: Container(A)) do
            \\    return sizeof A;
            \\  end
            \\};
            \\
            \\fun main() do
            \\  let c = new Container(byte) { .value = "Hello, World!"[0] };
            \\
            \\  return c.get_size();
            \\end
            ,
            .expected = 1,
        },
    };

    for (cases, 0..) |case, i| {
        for ([_]bool{ true, false }) |flag| {
            std.debug.print("#{}\n", .{i});

            var c = Compiler.init(std.testing.allocator);
            defer c.deinit();

            c.enable_jit = flag;
            c.compile(case.program, true) catch |err| {
                std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
            };
            std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, c.result.?) catch |err| {
                std.debug.panic("Unexpected error: {any}\n--enable_jit:{}\n==INPUT==\n{s}\n", .{ err, flag, case.program });
            };
        }
    }
}
