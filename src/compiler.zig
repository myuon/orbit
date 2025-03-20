const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typecheck = @import("typecheck.zig");
const desugar = @import("desugar.zig");
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
    desugar: desugar.Desugarer,
    enable_jit: bool,
    dump_ir_path: ?[]const u8 = null,
    dump_mono_ast_path: ?[]const u8 = null,
    dump_desugared_ast_path: ?[]const u8 = null,
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
            .desugar = desugar.Desugarer.init(allocator),
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
        self.desugar.deinit();
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

        try self.desugar.execute(&module);
        if (self.dump_desugared_ast_path) |path| {
            const file = try std.fs.cwd().createFile(path, .{});
            defer file.close();

            try std.fmt.format(file.writer(), "{any}\n", .{module});
        }

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
        for (case.expected) |expected| {
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

test "compiler.evalFiles" {
    const test_dir = "test";
    var dir = try std.fs.cwd().openDir(test_dir, .{ .iterate = true });
    defer dir.close();

    var dir_iterator = dir.iterate();
    while (try dir_iterator.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.name, ".ob")) continue;

        std.log.warn("Testing {s}", .{entry.name});

        const base_name = entry.name[0 .. entry.name.len - 3];
        const stdout_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/{s}.stdout", .{ test_dir, base_name });
        defer std.testing.allocator.free(stdout_path);

        const stdout_file = try std.fs.cwd().openFile(stdout_path, .{});
        defer stdout_file.close();

        const expected = try stdout_file.reader().readAllAlloc(std.testing.allocator, 1024);
        defer std.testing.allocator.free(expected);

        const program_path = try std.fmt.allocPrint(std.testing.allocator, "{s}/{s}", .{ test_dir, entry.name });
        defer std.testing.allocator.free(program_path);

        const program = try std.fs.cwd().readFileAlloc(std.testing.allocator, program_path, 1024);
        defer std.testing.allocator.free(program);

        // test/heavyディレクトリのファイルはenable_jit=trueの場合のみテストする
        const is_heavy = std.mem.startsWith(u8, entry.name, "heavy/");
        if (is_heavy) {
            // 重いテストはJITのみ実行
            var c = Compiler.init(std.testing.allocator);
            defer c.deinit();

            c.enable_jit = true;
            try c.compile(program, true);

            const actual = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{c.result.?.i64_});
            defer std.testing.allocator.free(actual);

            var expected_trimmed = std.ArrayList(u8).init(std.testing.allocator);
            defer expected_trimmed.deinit();

            var actual_trimmed = std.ArrayList(u8).init(std.testing.allocator);
            defer actual_trimmed.deinit();

            for (expected) |char| {
                if (char != '\n' and char != '\r') {
                    try expected_trimmed.append(char);
                }
            }

            for (actual) |char| {
                if (char != '\n' and char != '\r') {
                    try actual_trimmed.append(char);
                }
            }

            const expected_str = std.mem.trim(u8, expected_trimmed.items, " \t\n\r");
            const actual_str = std.mem.trim(u8, actual_trimmed.items, " \t\n\r");

            if (!std.mem.eql(u8, expected_str, actual_str)) {
                std.debug.panic("Unexpected output\n--enable_jit:{}\n==INPUT==\n{s}\nExpected: {s}\nGot: {s}\n", .{ true, program, expected_str, actual_str });
            }
        } else {
            // 通常のテストはJITありなしの両方で実行
            for ([_]bool{ false, true }) |flag| {
                var c = Compiler.init(std.testing.allocator);
                defer c.deinit();

                c.enable_jit = flag;
                try c.compile(program, true);

                const actual = try std.fmt.allocPrint(std.testing.allocator, "{d}", .{c.result.?.i64_});
                defer std.testing.allocator.free(actual);

                var expected_trimmed = std.ArrayList(u8).init(std.testing.allocator);
                defer expected_trimmed.deinit();

                var actual_trimmed = std.ArrayList(u8).init(std.testing.allocator);
                defer actual_trimmed.deinit();

                for (expected) |char| {
                    if (char != '\n' and char != '\r') {
                        try expected_trimmed.append(char);
                    }
                }

                for (actual) |char| {
                    if (char != '\n' and char != '\r') {
                        try actual_trimmed.append(char);
                    }
                }

                const expected_str = std.mem.trim(u8, expected_trimmed.items, " \t\n\r");
                const actual_str = std.mem.trim(u8, actual_trimmed.items, " \t\n\r");

                if (!std.mem.eql(u8, expected_str, actual_str)) {
                    std.debug.panic("Unexpected output\n--enable_jit:{}\n==INPUT==\n{s}\nExpected: {s}\nGot: {s}\n", .{ flag, program, expected_str, actual_str });
                }
            }
        }
    }
}
