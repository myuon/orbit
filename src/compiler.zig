const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typecheck = @import("typecheck.zig");
const vm = @import("vm.zig");
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
    vmc: vm.Vm,
    enable_jit: bool,
    dump_ir_path: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .allocator = allocator,
            .vmc = vm.Vm.init(allocator),
            .enable_jit = false,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.jit_cache.deinit();
        self.vmc.deinit();
    }

    pub fn compileInIr(self: *Compiler, str: []const u8) anyerror![]ast.Instruction {
        const zone = P.begin(@src(), "Compiler.compileInIr");
        defer zone.end();

        var l = lexer.Lexer.init(self.allocator, str);
        defer l.deinit();

        const tokens = try l.run();

        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        var module = try p.module();

        var tc = typecheck.Typechecker.init(self.allocator);
        defer tc.deinit();

        try tc.typecheck(&module);

        const ir = try self.vmc.compile("main", module);

        return ir;
    }

    pub fn startVm(self: *Compiler, ir: []ast.Instruction, options: struct { enable_jit: bool }) anyerror!ast.Value {
        const zone = P.begin(@src(), "Compiler.startVm");
        defer zone.end();

        var stack = std.ArrayList(i64).init(self.allocator);
        defer stack.deinit();

        var address_map = std.StringHashMap(i64).init(self.allocator);
        defer address_map.deinit();

        try address_map.put("return", -2 - 1);
        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var vmr = vm.VmRuntime.init(self.allocator);
        defer vmr.deinit();

        vmr.enable_jit = options.enable_jit;

        try vmr.run(ir, &stack, &bp);

        return ast.Value{ .i64_ = stack.items[0] };
    }

    pub fn evalModule(self: *Compiler, str: []const u8, options: struct { enable_jit: bool }) anyerror!ast.Value {
        const zone = P.begin(@src(), "Compiler.evalModule");
        defer zone.end();

        const ir = try self.compileInIr(str);
        if (self.dump_ir_path) |path| {
            const file = try std.fs.cwd().createFile(path, .{});
            defer file.close();

            for (ir) |inst| {
                try std.fmt.format(file.writer(), "{s}\n", .{inst});
            }
        }

        return try self.startVm(ir, .{ .enable_jit = options.enable_jit });
    }
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("vm.zig");
    _ = @import("jit.zig");
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
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, c.evalModule(case.program, .{ .enable_jit = false }) catch |err| {
            std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
        }) catch |err| {
            std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
        };
        std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalModule(case.program, .{ .enable_jit = true })) catch |err| {
            std.debug.panic("Unexpected error: {any}\n==INPUT==\n{s}\n", .{ err, case.program });
        };
    }
}
