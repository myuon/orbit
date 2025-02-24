const std = @import("std");

const ast = @import("ast.zig");
const jit = @import("jit.zig");
const P = @import("profiler");

pub const data_section_ptr: i64 = 8;
pub const global_section_ptr: i64 = 16;
pub const heap_section_ptr: i64 = 24;

pub const VmError = error{
    VariableNotFound,
    LabelNotFound,
};

// This is a compiler for AOT compilation
pub const VmCompiler = struct {
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    compiling_context: []const u8 = "",
    prng: std.Random.Xoshiro256,
    env: std.StringHashMap(i32),
    env_offset: i32 = 0,
    string_data: std.StringHashMap(i32),
    string_data_offset: usize,
    global_data: std.StringHashMap(i32),
    global_data_offset: usize,
    initialized_statements: std.ArrayList(ast.Statement),

    pub fn init(allocator: std.mem.Allocator) VmCompiler {
        const prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            std.posix.getrandom(std.mem.asBytes(&seed)) catch unreachable;
            break :blk seed;
        });

        return VmCompiler{
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .prng = prng,
            .env = std.StringHashMap(i32).init(allocator),
            .env_offset = 0,
            .string_data = std.StringHashMap(i32).init(allocator),
            .string_data_offset = 0,
            .global_data = std.StringHashMap(i32).init(allocator),
            .global_data_offset = 0,
            .initialized_statements = std.ArrayList(ast.Statement).init(allocator),
        };
    }

    pub fn deinit(self: *VmCompiler) void {
        self.ast_arena_allocator.deinit();
        self.env.deinit();
        self.string_data.deinit();
        self.global_data.deinit();
        self.initialized_statements.deinit();
    }

    /// This function MUST NOT move the label positions.
    pub fn resolveIrLabels(
        self: *VmCompiler,
        prog: []ast.Instruction,
        exit_stub: ?std.StringHashMap(usize),
    ) anyerror!void {
        var labels = std.StringHashMap(usize).init(self.allocator);
        defer labels.deinit();

        for (prog, 0..) |inst, i| {
            switch (inst) {
                .label => |name| {
                    try labels.put(name, i);
                },
                else => {},
            }
        }

        for (prog, 0..) |inst, i| {
            switch (inst) {
                .jump => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (exit_stub.?.get(label)) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_d = target };
                },
                .jump_ifzero => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (exit_stub.?.get(label)) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_ifzero_d = target };
                },
                .label => {
                    prog[i] = ast.Instruction{ .nop = true };
                },
                .call => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (exit_stub.?.get(label)) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .call_d = target };
                },
                else => {},
            }
        }
    }

    fn callFunction(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), name: []const u8, args: []ast.Expression) anyerror!void {
        // return value
        try buffer.append(ast.Instruction{ .push = -2 });

        // order from left to right
        for (args) |arg| {
            try self.compileExprFromAst(buffer, arg);
        }

        // prologue
        try buffer.append(ast.Instruction{ .get_pc = true });
        try buffer.append(ast.Instruction{ .get_bp = true });
        try buffer.append(ast.Instruction{ .get_sp = true });
        try buffer.append(ast.Instruction{ .set_bp = true });

        // call
        try buffer.append(ast.Instruction{ .call = name });

        for (args) |_| {
            try buffer.append(ast.Instruction{ .pop = true });
        }
    }

    fn compileExprFromAst(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
        switch (expr) {
            .var_ => |v| {
                if (self.env.get(v)) |k| {
                    try buffer.append(ast.Instruction{ .get_local_d = k });
                } else if (self.global_data.contains(v)) {
                    try self.compileLhsExprFromAst(buffer, expr);
                    try buffer.append(ast.Instruction{ .load = 8 });
                } else {
                    std.log.err("Variable not found: {s}\n", .{v});
                    return error.VariableNotFound;
                }
            },
            .literal => |lit| {
                switch (lit) {
                    .number => |n| {
                        try buffer.append(ast.Instruction{ .push = @intCast(n) });
                    },
                    .boolean => |b| {
                        if (b) {
                            try buffer.append(ast.Instruction{ .push = 1 });
                        } else {
                            try buffer.append(ast.Instruction{ .push = 0 });
                        }
                    },
                    .string => |s| {
                        var address: i32 = -1;
                        if (self.string_data.get(s)) |i| {
                            address = @intCast(i);
                        } else {
                            const i = self.string_data_offset;
                            try self.string_data.put(s, @intCast(i));
                            self.string_data_offset += s.len + 1; // +1 for null terminator
                            address = @intCast(i);
                        }

                        try buffer.append(ast.Instruction{ .push = @intCast(data_section_ptr) });
                        try buffer.append(ast.Instruction{ .load = 8 });
                        try buffer.append(ast.Instruction{ .push = address });
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                }
            },
            .binop => |binop| {
                try self.compileExprFromAst(buffer, binop.lhs.*);
                try self.compileExprFromAst(buffer, binop.rhs.*);

                switch (binop.op) {
                    .plus => {
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                    .minus => {
                        try buffer.append(ast.Instruction{ .sub = true });
                    },
                    .star => {
                        try buffer.append(ast.Instruction{ .mul = true });
                    },
                    .eqeq => {
                        try buffer.append(ast.Instruction{ .eq = true });
                    },
                    .percent => {
                        try buffer.append(ast.Instruction{ .mod = true });
                    },
                    .langle => {
                        try buffer.append(ast.Instruction{ .lt = true });
                    },
                    .lte => {
                        try buffer.append(ast.Instruction{ .lte = true });
                    },
                    .rangle => {
                        try buffer.append(ast.Instruction{ .gt = true });
                    },
                    .gte => {
                        try buffer.append(ast.Instruction{ .gte = true });
                    },
                    else => {
                        std.log.info("op: {}\n", .{binop.op});
                        unreachable;
                    },
                }
            },
            .call => |call| {
                try self.callFunction(buffer, call.name, call.args);
            },
            .if_ => |if_| {
                try self.compileExprFromAst(buffer, if_.cond.*);

                const id = self.prng.random().int(u32);

                const label_ifthen = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_then_{d}", .{id});
                const label_ifelse = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_else_{d}", .{id});
                const label_ifend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .jump_ifzero = label_ifelse });
                try buffer.append(ast.Instruction{ .label = label_ifthen });
                try self.compileBlockFromAst(buffer, if_.then_);
                try buffer.append(ast.Instruction{ .jump = label_ifend });
                try buffer.append(ast.Instruction{ .label = label_ifelse });
                try self.compileBlockFromAst(buffer, if_.else_);
                try buffer.append(ast.Instruction{ .label = label_ifend });
            },
            .block => {
                unreachable;
            },
            .index => |index| {
                switch (index.type_) {
                    .ptr => {
                        try self.compileLhsExprFromAst(buffer, expr);
                        try buffer.append(ast.Instruction{ .load = (try index.type_.getValueType()).size() });
                    },
                    .array => {
                        try self.compileLhsExprFromAst(buffer, expr);
                        try buffer.append(ast.Instruction{ .load = (try index.type_.getValueType()).size() });
                    },
                    .slice => |slice| {
                        const valueType = slice.elem_type.*;
                        switch (valueType) {
                            .int => {
                                try self.callFunction(buffer, "get_slice_int", @constCast(&[_]ast.Expression{
                                    index.lhs.*,
                                    index.rhs.*,
                                }));
                            },
                            .byte => {
                                try self.callFunction(buffer, "get_slice_byte", @constCast(&[_]ast.Expression{
                                    index.lhs.*,
                                    index.rhs.*,
                                }));
                            },
                            else => {
                                // Deprecated:
                                try self.compileExprFromAst(buffer, index.lhs.*);
                                try buffer.append(ast.Instruction{ .load = 8 });
                                try self.compileExprFromAst(buffer, index.rhs.*);
                                try buffer.append(ast.Instruction{ .push = valueType.size() });
                                try buffer.append(ast.Instruction{ .mul = true });
                                try buffer.append(ast.Instruction{ .add = true });
                                try buffer.append(ast.Instruction{ .load = valueType.size() });
                            },
                        }
                    },
                    .vec => {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try self.compileExprFromAst(buffer, index.rhs.*);
                        try buffer.append(ast.Instruction{ .vec_get = true });
                    },
                    .map => {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try self.compileExprFromAst(buffer, index.rhs.*);
                        try buffer.append(ast.Instruction{ .table_get = true });
                    },
                    else => {
                        std.log.err("Invalid index type: {any}\n", .{index.type_});
                        unreachable;
                    },
                }
            },
            .new => |new| {
                switch (new.type_) {
                    .array => |array| {
                        std.debug.assert(new.initializers.len == 0);

                        try self.callFunction(buffer, "allocate_memory", @constCast(&[_]ast.Expression{
                            .{ .literal = .{ .number = @intCast(array.size * @as(usize, array.elem_type.size())) } },
                        }));
                    },
                    .map => |map| {
                        std.debug.assert(new.initializers.len == 0);

                        // TODO: growable capacity
                        try self.callFunction(buffer, "allocate_memory", @constCast(&[_]ast.Expression{
                            .{ .literal = .{ .number = @intCast(128 * @as(usize, map.value_type.size())) } },
                        }));
                    },
                    .vec => |vec| {
                        std.debug.assert(new.initializers.len == 0);

                        try buffer.append(ast.Instruction{ .allocate_vec = vec.elem_type.size() });
                    },
                    .struct_ => |struct_| {
                        std.debug.assert(new.initializers.len == struct_.fields.len);

                        // TODO: calculate the size of each field
                        try self.callFunction(buffer, "allocate_memory", @constCast(&[_]ast.Expression{
                            .{ .literal = .{ .number = @intCast(struct_.fields.len * 8) } },
                        }));

                        var fields = try self.ast_arena_allocator.allocator().alloc(ast.Expression, struct_.fields.len);
                        for (new.initializers) |sinit| {
                            const offset = try struct_.getFieldOffset(sinit.field);
                            fields[offset] = sinit.value;
                        }

                        for (fields, 0..) |e, i| {
                            try buffer.append(ast.Instruction{ .get_local_d = self.env_offset });
                            try buffer.append(ast.Instruction{ .push = @intCast(i * 8) });
                            try buffer.append(ast.Instruction{ .add = true });
                            try self.compileExprFromAst(buffer, e);
                            try buffer.append(ast.Instruction{ .store = 8 });
                        }
                    },
                    .slice => |slice| {
                        std.debug.assert(new.initializers.len == 1);
                        std.debug.assert(std.mem.eql(u8, new.initializers[0].field, "len"));

                        try self.callFunction(buffer, "new_slice", @constCast(&[_]ast.Expression{
                            .{ .literal = .{ .number = @intCast(slice.elem_type.size()) } },
                            new.initializers[0].value,
                        }));
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .project => |project| {
                try self.compileLhsExprFromAst(buffer, expr);

                const valueType = try project.struct_.getFieldType(project.rhs);
                try buffer.append(ast.Instruction{ .load = valueType.size() });
            },
            .as => |as| {
                try self.compileExprFromAst(buffer, as.lhs.*);
            },
        }
    }

    fn compileLhsExprFromAst(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
        switch (expr) {
            .var_ => |name| {
                if (self.env.get(name)) |k| {
                    try buffer.append(ast.Instruction{ .set_local_d = k });
                } else if (self.global_data.get(name)) |k| {
                    try buffer.append(ast.Instruction{ .push = global_section_ptr });
                    try buffer.append(ast.Instruction{ .load = 8 });
                    try buffer.append(ast.Instruction{ .push = @intCast(k * 8) });
                    try buffer.append(ast.Instruction{ .add = true });
                } else {
                    std.log.err("Variable not found: {s}\n", .{name});
                    return error.VariableNotFound;
                }
            },
            .index => |index| {
                switch (index.type_) {
                    .ptr => {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try self.compileExprFromAst(buffer, index.rhs.*);
                        try buffer.append(ast.Instruction{ .push = (try index.type_.getValueType()).size() });
                        try buffer.append(ast.Instruction{ .mul = true });
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                    .array => {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try self.compileExprFromAst(buffer, index.rhs.*);
                        try buffer.append(ast.Instruction{ .push = (try index.type_.getValueType()).size() });
                        try buffer.append(ast.Instruction{ .mul = true });
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                    .vec => {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try buffer.append(ast.Instruction{ .vec_get = true });
                    },
                    else => {
                        std.log.err("Invalid index type: {any}\n", .{index.type_});
                        unreachable;
                    },
                }
            },
            .project => |project| {
                try self.compileExprFromAst(buffer, project.lhs.*);
                try buffer.append(ast.Instruction{ .push = @intCast(try project.struct_.getFieldOffset(project.rhs)) });
                try buffer.append(ast.Instruction{ .push = 8 });
                try buffer.append(ast.Instruction{ .mul = true });
                try buffer.append(ast.Instruction{ .add = true });
            },
            else => {
                std.log.err("Invalid LHS expression: {any}\n", .{expr});
                unreachable;
            },
        }
    }

    fn compileStatementFromAst(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), stmt: ast.Statement) anyerror!void {
        switch (stmt) {
            .let => |let| {
                try self.compileExprFromAst(buffer, let.value);

                const i = self.env_offset;
                try self.env.put(let.name, i);
                self.env_offset += 1;
            },
            .return_ => |val| {
                try self.compileExprFromAst(buffer, val);

                // epilogue
                if (self.env.get("return")) |k| {
                    try buffer.append(ast.Instruction{ .set_local_d = k });
                } else {
                    var keys = std.ArrayList([]const u8).init(self.allocator);
                    defer keys.deinit();

                    var iter = self.env.keyIterator();
                    while (iter.next()) |k| {
                        try keys.append(k.*);
                    }

                    std.log.err("Cannot find return value position in {any}", .{keys.items});
                    return error.VariableNotFound;
                }

                try buffer.append(ast.Instruction{ .get_bp = true });
                try buffer.append(ast.Instruction{ .set_sp = true });
                try buffer.append(ast.Instruction{ .set_bp = true });
                try buffer.append(ast.Instruction{ .ret = true });
            },
            .expr => |expr| {
                try self.compileExprFromAst(buffer, expr);
                try buffer.append(ast.Instruction{ .pop = true });
            },
            .if_ => |if_| {
                try self.compileExprFromAst(buffer, if_.cond.*);

                const id = self.prng.random().int(u32);

                const label_ifthen = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_then_{d}", .{id});
                const label_ifelse = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_else_{d}", .{id});
                const label_ifend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .jump_ifzero = label_ifelse });
                try buffer.append(ast.Instruction{ .label = label_ifthen });
                try self.compileBlockFromAst(buffer, if_.then_);
                try buffer.append(ast.Instruction{ .jump = label_ifend });
                try buffer.append(ast.Instruction{ .label = label_ifelse });
                if (if_.else_) |else_| {
                    try self.compileBlockFromAst(buffer, else_);
                }
                try buffer.append(ast.Instruction{ .label = label_ifend });
            },
            .while_ => |while_| {
                const id = self.prng.random().int(u32);

                const label_whilecond = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_cond_{d}", .{id});
                const label_whilebody = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_body_{d}", .{id});
                const label_whileend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .label = label_whilecond });
                try self.compileExprFromAst(buffer, while_.cond.*);
                try buffer.append(ast.Instruction{ .jump_ifzero = label_whileend });
                try buffer.append(ast.Instruction{ .label = label_whilebody });
                try self.compileBlockFromAst(buffer, while_.body);
                try buffer.append(ast.Instruction{ .jump = label_whilecond });
                try buffer.append(ast.Instruction{ .label = label_whileend });
            },
            .assign => |assign| {
                switch (assign.lhs) {
                    .var_ => |name| {
                        if (self.env.get(name)) |k| {
                            try self.compileExprFromAst(buffer, assign.rhs);
                            try buffer.append(ast.Instruction{ .set_local_d = k });
                        } else if (self.global_data.contains(name)) {
                            try self.compileLhsExprFromAst(buffer, assign.lhs);
                            try self.compileExprFromAst(buffer, assign.rhs);
                            try buffer.append(ast.Instruction{ .store = 8 });
                        } else {
                            std.log.err("Variable not found: {s}\n", .{name});
                            return error.VariableNotFound;
                        }
                    },
                    .index => |index| {
                        switch (index.type_) {
                            .map => {
                                try self.compileExprFromAst(buffer, assign.lhs.index.lhs.*);
                                try self.compileExprFromAst(buffer, assign.lhs.index.rhs.*);
                                try self.compileExprFromAst(buffer, assign.rhs);
                                try buffer.append(ast.Instruction{ .table_set = true });
                            },
                            .vec => {
                                try self.compileExprFromAst(buffer, assign.lhs.index.lhs.*);
                                try self.compileExprFromAst(buffer, assign.lhs.index.rhs.*);
                                try self.compileExprFromAst(buffer, assign.rhs);
                                try buffer.append(ast.Instruction{ .vec_set = true });
                            },
                            .slice => {
                                try self.compileExprFromAst(buffer, assign.lhs.index.lhs.*);
                                try buffer.append(ast.Instruction{ .load = 8 });
                                try self.compileExprFromAst(buffer, assign.lhs.index.rhs.*);
                                try buffer.append(ast.Instruction{ .push = (try index.type_.getValueType()).size() });
                                try buffer.append(ast.Instruction{ .mul = true });
                                try buffer.append(ast.Instruction{ .add = true });
                                try self.compileExprFromAst(buffer, assign.rhs);
                                try buffer.append(ast.Instruction{ .store = (try index.type_.getValueType()).size() });
                            },
                            else => {
                                try self.compileLhsExprFromAst(buffer, assign.lhs);
                                try self.compileExprFromAst(buffer, assign.rhs);
                                try buffer.append(ast.Instruction{ .store = assign.type_.size() });
                            },
                        }
                    },
                    else => {
                        try self.compileLhsExprFromAst(buffer, assign.lhs);
                        try self.compileExprFromAst(buffer, assign.rhs);
                        try buffer.append(ast.Instruction{ .store = assign.type_.size() });
                    },
                }
            },
            .push => |push| {
                switch (push.type_) {
                    .vec => {
                        try self.compileExprFromAst(buffer, push.lhs);
                        try self.compileExprFromAst(buffer, push.rhs);
                        try buffer.append(ast.Instruction{ .vec_push = true });
                    },
                    else => {
                        unreachable;
                    },
                }
            },
        }
    }

    fn compileBlockFromAst(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), block: ast.Block) anyerror!void {
        const offset = self.env_offset;

        for (block.statements) |stmt| {
            try self.compileStatementFromAst(buffer, stmt);
        }

        std.debug.assert(self.env_offset >= offset);
        for (0..@intCast(self.env_offset - offset)) |_| {
            try buffer.append(ast.Instruction{ .pop = true });
        }
        self.env_offset = offset;
    }

    pub fn compileBlock(
        self: *VmCompiler,
        entrypoint: []const u8,
        body: ast.Block,
    ) anyerror![]ast.Instruction {
        self.compiling_context = entrypoint;

        var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        try self.compileBlockFromAst(&buffer, body);

        self.compiling_context = "";

        return buffer.items;
    }

    fn compileModule(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), module: ast.Module) anyerror!void {
        for (module.decls) |decl| {
            switch (decl) {
                .fun => |f| {
                    self.env_offset = 0;
                    self.env.clearAndFree();

                    try buffer.append(ast.Instruction{ .label = f.name });

                    try self.env.put("return", -@as(i32, @intCast(f.params.len)) - 3);

                    // register names in the reverse order
                    for (f.params, 0..) |param, i| {
                        try self.env.put(param.name, -@as(i32, @intCast(f.params.len - i)) - 2);
                    }

                    self.compiling_context = f.name;

                    try buffer.appendSlice(try self.compileBlock(f.name, f.body));

                    self.compiling_context = "";

                    const end_of_f = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "end_of_{s}", .{f.name});
                    try buffer.append(ast.Instruction{ .label = end_of_f });
                },
                .let => |let| {
                    const i = self.global_data_offset;
                    try self.global_data.put(let.name, @intCast(i));
                    self.global_data_offset += 1;

                    if (let.value) |value| {
                        try self.initialized_statements.append(ast.Statement{
                            .assign = .{
                                // FIXME: other types
                                .type_ = .{ .int = true },
                                .lhs = .{ .var_ = let.name },
                                .rhs = value,
                            },
                        });
                    }
                },
            }
        }
    }

    fn compileProgramSection(
        self: *VmCompiler,
        buffer: *std.ArrayList(ast.Instruction),
        entrypoint: []const u8,
        module: ast.Module,
    ) anyerror!void {
        try self.env.put("return", -3);
        try self.compileBlockFromAst(buffer, ast.Block{
            .statements = @constCast(&[_]ast.Statement{
                .{
                    .return_ = .{
                        .call = .{
                            .name = entrypoint,
                            .args = &[_]ast.Expression{},
                        },
                    },
                },
            }),
            .expr = null,
        });

        try self.compileModule(buffer, module);
    }

    fn compileDataSection(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction)) anyerror!usize {
        var data_offset: usize = 0;

        var iter = self.string_data.keyIterator();
        while (iter.next()) |k| {
            const offset = self.string_data.get(k.*).?;

            try buffer.append(ast.Instruction{ .push = @intCast(data_section_ptr) });
            try buffer.append(ast.Instruction{ .load = 8 });
            try buffer.append(ast.Instruction{ .push = offset });
            try buffer.append(ast.Instruction{ .add = true });
            try buffer.append(ast.Instruction{ .set_memory = .{ .data = k.* } });

            data_offset = @as(usize, @intCast(offset)) + k.*.len + 1;
        }

        return data_offset;
    }

    fn compileGlobalSection(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction)) anyerror!usize {
        var global_offset: usize = 0;

        var iter = self.global_data.keyIterator();
        while (iter.next()) |_| {
            global_offset += 8;
        }

        for (self.initialized_statements.items) |stmt| {
            try self.compileStatementFromAst(buffer, stmt);
        }

        return global_offset;
    }

    pub fn compile(
        self: *VmCompiler,
        entrypoint: []const u8,
        module: ast.Module,
    ) anyerror![]ast.Instruction {
        const zone = P.begin(@src(), "Vm.compile");
        defer zone.end();

        self.env_offset = 0;
        self.env.clearAndFree();

        var initBuffer = std.ArrayList(ast.Instruction).init(self.allocator);
        defer initBuffer.deinit();

        // ==== global section
        //  0: null_ptr
        //  8: data_section_ptr
        // 16: global_section_ptr
        // 24: heap_ptr
        // ... user-defined globals
        // ==== static data [32+sizeGlobalSection-32+sizeGlobalSection+sizeDataSection]
        // ...

        var progBuffer = std.ArrayList(ast.Instruction).init(self.allocator);
        defer progBuffer.deinit();
        try self.compileProgramSection(&progBuffer, entrypoint, module);

        var globalBuffer = std.ArrayList(ast.Instruction).init(self.allocator);
        defer globalBuffer.deinit();
        const sizeGlobalSection = try self.compileGlobalSection(&globalBuffer);

        var dataBuffer = std.ArrayList(ast.Instruction).init(self.allocator);
        defer dataBuffer.deinit();
        const sizeDataSection = try self.compileDataSection(&dataBuffer);

        const sizeBuiltinGlobalSection = 24;

        // set global_section_ptr
        try initBuffer.append(ast.Instruction{ .push = @intCast(global_section_ptr) });
        try initBuffer.append(ast.Instruction{ .push = @intCast(sizeBuiltinGlobalSection) });
        try initBuffer.append(ast.Instruction{ .store = 8 });

        // set data_section_ptr
        try initBuffer.append(ast.Instruction{ .push = @intCast(data_section_ptr) });
        try initBuffer.append(ast.Instruction{ .push = @intCast(sizeBuiltinGlobalSection + sizeGlobalSection) });
        try initBuffer.append(ast.Instruction{ .store = 8 });

        // set heap_section_ptr
        try initBuffer.append(ast.Instruction{ .push = @intCast(heap_section_ptr) });
        try initBuffer.append(ast.Instruction{ .push = @intCast(sizeBuiltinGlobalSection + sizeGlobalSection + sizeDataSection) });
        try initBuffer.append(ast.Instruction{ .store = 8 });

        var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        try buffer.appendSlice(initBuffer.items);
        try buffer.appendSlice(globalBuffer.items);
        try buffer.appendSlice(dataBuffer.items);
        try buffer.appendSlice(progBuffer.items);

        return buffer.items;
    }

    pub fn optimize(self: *VmCompiler, program: []ast.Instruction) anyerror![]ast.Instruction {
        var result = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());

        var i: usize = 0;
        while (i < program.len) {
            switch (program[i]) {
                .nop => {},
                .lt => {
                    if (program[i - 1].is_get_local_d() and program[i - 2].is_get_local_d()) {
                        const rhs = result.pop().get_local_d;
                        const lhs = result.pop().get_local_d;
                        try result.append(ast.Instruction{ .lt_d = .{ .lhs = lhs, .rhs = rhs } });
                    } else {
                        try result.append(program[i]);
                    }
                },
                .add => {
                    if (program[i - 1].is_push() and program[i - 2].is_get_local_d()) {
                        const rhs = result.pop().push;
                        const lhs = result.pop().get_local_d;
                        try result.append(ast.Instruction{ .add_di = .{ .lhs = lhs, .imm = rhs, .target = null } });
                    } else if (program[i - 1].is_mul() and program[i - 2].is_get_local_d() and program[i - 3].is_get_local_d() and program[i - 4].is_get_local_d()) {
                        _ = result.pop().mul;
                        const rhs = result.pop().get_local_d;
                        const lhs = result.pop().get_local_d;
                        const base = result.pop().get_local_d;

                        try result.append(ast.Instruction{ .madd_d = .{ .lhs = lhs, .rhs = rhs, .base = base, .target = null } });
                    } else {
                        try result.append(program[i]);
                    }
                },
                .set_local_d => {
                    if (program[i - 1].is_add()) {
                        if (result.items[result.items.len - 1].is_add_di()) {
                            const offset = program[i].set_local_d;
                            const add_di = result.pop().add_di;
                            try result.append(ast.Instruction{ .add_di = .{ .lhs = add_di.lhs, .imm = add_di.imm, .target = @intCast(offset) } });
                        } else if (result.items[result.items.len - 1].is_madd_d()) {
                            const offset = program[i].set_local_d;
                            const madd_d = result.pop().madd_d;
                            try result.append(ast.Instruction{ .madd_d = .{ .lhs = madd_d.lhs, .rhs = madd_d.rhs, .base = madd_d.base, .target = @intCast(offset) } });
                        } else {
                            try result.append(program[i]);
                        }
                    } else {
                        try result.append(program[i]);
                    }
                },
                else => {
                    try result.append(program[i]);
                },
            }

            i += 1;
        }

        return result.items;
    }
};
