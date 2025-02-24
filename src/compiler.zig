const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typecheck = @import("typecheck.zig");
const vm = @import("vm.zig");
const runtime = @import("runtime.zig");
const jit = @import("jit.zig");
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
    vmc: vm.VmCompiler,
    enable_jit: bool,
    dump_ir_path: ?[]const u8 = null,
    enable_optimize_ir: bool = true,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .allocator = allocator,
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .vmc = vm.VmCompiler.init(allocator),
            .enable_jit = true,
            .enable_optimize_ir = true,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.jit_cache.deinit();
        self.vmc.deinit();
        self.arena_allocator.deinit();
    }

    pub fn compileInIr(self: *Compiler, str: []const u8) anyerror![]ast.Instruction {
        const stdlib =
            \\let hp;
            \\
            \\fun allocate_memory(size: int): ptr(byte) do
            \\  let p = hp;
            \\  hp = hp + size;
            \\
            \\  return p as ptr(byte);
            \\end
            \\
            \\fun new_slice(size: int, len: int) do
            \\  let pair = new struct{ptr: ptr(byte), len: int} { .ptr = allocate_memory(size * len), .len = len };
            \\
            \\  return pair;
            \\end
            \\
            \\fun get_slice_int(data: struct{ptr: ptr(int), len: int}, index: int): int do
            \\  let ptr = data.ptr;
            \\  return ptr[index];
            \\end
            \\
            \\fun get_slice_byte(data: struct{ptr: ptr(byte), len: int}, index: int): byte do
            \\  let ptr = data.ptr;
            \\  return ptr[index];
            \\end
            \\
            \\fun new_vec(size: int, capacity: int): struct{ptr: ptr(byte), len: int, capacity: int} do
            \\  let vec = new struct{ptr: ptr(byte), len: int, capacity: int} { .ptr = allocate_memory(size * capacity), .len = 0, .capacity = capacity };
            \\  return vec;
            \\end
            \\
            \\fun get_vec_int(data: struct{ptr: ptr(int), len: int, capacity: int}, index: int) do
            \\  let ptr = data.ptr;
            \\  return ptr[index];
            \\end
            \\
            \\fun set_vec_int(data: struct{ptr: ptr(int), len: int, capacity: int}, index: int, value: int) do
            \\  let ptr = data.ptr;
            \\  ptr[index] = value;
            \\  return 0;
            \\end
            \\
            \\fun push_vec_int(data: struct{ptr: ptr(int), len: int, capacity: int}, value: int) do
            \\  if (data.len + 1 < data.capacity) do
            \\    let ptr = data.ptr;
            \\    ptr[data.len] = value;
            \\    data.len = data.len + 1;
            \\  else
            \\    let new_data_byte = new_vec(4, data.capacity * 2);
            \\    let new_data = new_data_byte as struct{ptr: ptr(int), len: int, capacity: int};
            \\    let i = 0;
            \\    while (i < data.len) do
            \\      set_vec_int(new_data, i, get_vec_int(data, i));
            \\      i = i + 1;
            \\    end
            \\
            \\    push_vec_int(new_data, value);
            \\  end
            \\
            \\  return 0;
            \\end
        ;
        const input = try std.fmt.allocPrint(self.arena_allocator.allocator(), "{s}\n{s}", .{ stdlib, str });

        const zone = P.begin(@src(), "Compiler.compileInIr");
        defer zone.end();

        var l = lexer.Lexer.init(self.allocator, input);
        defer l.deinit();

        const tokens = try l.run();

        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        var module = try p.module();

        var tc = typecheck.Typechecker.init(self.allocator);
        defer tc.deinit();

        try tc.typecheck(&module);

        var ir = try self.vmc.compile("main", module);

        if (self.enable_optimize_ir) {
            ir = try self.vmc.optimize(ir);
        }

        return ir;
    }

    pub fn startVm(self: *Compiler, ir: []ast.Instruction) anyerror!ast.Value {
        const zone = P.begin(@src(), "Compiler.startVm");
        defer zone.end();

        var stack = try std.ArrayList(i64).initCapacity(self.allocator, 1024);
        defer stack.deinit();

        var address_map = std.StringHashMap(i64).init(self.allocator);
        defer address_map.deinit();

        try address_map.put("return", -2 - 1);
        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var vmr = runtime.VmRuntime.init(self.allocator);
        defer vmr.deinit();

        vmr.enable_jit = self.enable_jit;

        try vmr.run(ir, &stack, &bp);

        return ast.Value{ .i64_ = stack.items[0] };
    }

    pub fn evalModule(self: *Compiler, str: []const u8) anyerror!ast.Value {
        const zone = P.begin(@src(), "Compiler.evalModule");
        defer zone.end();

        const ir = try self.compileInIr(str);
        if (self.dump_ir_path) |path| {
            const file = try std.fs.cwd().createFile(path, .{});
            defer file.close();

            for (ir) |inst| {
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

        return try self.startVm(ir);
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
            \\  let s = new array(int, 100) {};
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
            \\  let k = new map(ptr(byte), int) {};
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
            \\  let k = new map(ptr(byte), int) {};
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
            \\  while (n < 1000) do
            \\    vec <- n * n;
            \\    n = n + 1;
            \\  end
            \\
            \\  vec[777] = vec[777] + 10;
            \\
            \\  return vec[777];
            \\end
            ,
            .expected = 603739,
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
            \\  let s = new slice(int) { .len = 10 };
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
    };

    for (cases) |case| {
        for ([_]bool{ true, false }) |flag| {
            var c = Compiler.init(std.testing.allocator);
            defer c.deinit();

            c.enable_jit = flag;
            std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, c.evalModule(case.program) catch |err| {
                std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
            }) catch |err| {
                std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
            };
        }
    }
}
