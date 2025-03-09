const std = @import("std");

const ast = @import("ast.zig");
const jit = @import("jit.zig");
const P = @import("profiler");

pub const data_section_ptr: i64 = 8;
pub const global_section_ptr: i64 = 16;
pub const heap_section_ptr: i64 = 24;
pub const heap_ptr: i64 = 32;

pub const VmError = error{
    VariableNotFound,
    LabelNotFound,
};

// This is a compiler for AOT compilation
pub const VmCompiler = struct {
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    prng: std.Random.Xoshiro256,
    env: std.StringHashMap(i32),
    env_types: std.StringHashMap(ast.Type),
    env_offset: i32 = 0,
    string_data: std.StringHashMap(i32),
    string_data_offset: usize,
    global_data: std.StringHashMap(i32),
    global_data_offset: usize,
    initialized_statements: std.ArrayList(ast.Statement),
    type_defs: ?ast.TypeDefs,
    generic_calls: std.ArrayList(ast.GenericCallInfo),

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
            .env_types = std.StringHashMap(ast.Type).init(allocator),
            .env_offset = 0,
            .string_data = std.StringHashMap(i32).init(allocator),
            .string_data_offset = 0,
            .global_data = std.StringHashMap(i32).init(allocator),
            .global_data_offset = 0,
            .initialized_statements = std.ArrayList(ast.Statement).init(allocator),
            .type_defs = null,
            .generic_calls = std.ArrayList(ast.GenericCallInfo).init(allocator),
        };
    }

    pub fn deinit(self: *VmCompiler) void {
        self.ast_arena_allocator.deinit();
        self.env.deinit();
        self.env_types.deinit();
        self.string_data.deinit();
        self.global_data.deinit();
        self.initialized_statements.deinit();
        self.generic_calls.deinit();
    }

    /// This function MUST NOT move the label positions.
    pub fn resolveIrLabels(
        self: *VmCompiler,
        prog: []ast.Instruction,
        exit_stub: std.StringHashMap(usize),
        vtable: std.StringHashMap(usize),
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
                    } else if (exit_stub.get(label)) |fallback| {
                        target = fallback;
                    } else {
                        std.log.warn("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_d = target };
                },
                .jump_ifzero => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (exit_stub.get(label)) |fallback| {
                        target = fallback;
                    } else {
                        std.log.warn("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_ifzero_d = target };
                },
                .label => {
                    prog[i] = ast.Instruction{ .nop = true };
                },
                .call => |label| {
                    if (labels.get(label)) |t| {
                        prog[i] = ast.Instruction{ .call_d = t };
                    } else if (exit_stub.get(label)) |fallback| {
                        prog[i] = ast.Instruction{ .call_d = fallback };
                    } else if (vtable.get(label)) |t| {
                        prog[i] = ast.Instruction{ .call_vtable = t };
                    } else {
                        std.log.warn("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                },
                else => {},
            }
        }
    }

    fn callFunction(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), callee: ast.Expression, args: []ast.Expression) anyerror!void {
        var argCount: usize = 0;

        // return value
        try buffer.append(ast.Instruction{ .push = -2 });

        // self
        switch (callee) {
            .project => |project| {
                try self.compileExprFromAst(buffer, project.lhs.*);
                argCount += 1;
            },
            else => {},
        }

        // order from left to right
        for (args) |arg| {
            try self.compileExprFromAst(buffer, arg);
            argCount += 1;
        }

        // prologue
        try buffer.append(ast.Instruction{ .get_pc = true });
        try buffer.append(ast.Instruction{ .get_bp = true });
        try buffer.append(ast.Instruction{ .get_sp = true });
        try buffer.append(ast.Instruction{ .set_bp = true });

        // call
        switch (callee) {
            .var_ => |name| {
                try buffer.append(ast.Instruction{ .call = name });
            },
            .project => |project| {
                try buffer.append(ast.Instruction{ .call = project.rhs });
            },
            else => {
                std.log.err("Invalid callee: {any}\n", .{callee});
                unreachable;
            },
        }

        for (0..argCount) |_| {
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
                try self.callFunction(buffer, call.callee.*, call.args);
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
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "array")) {
                            try self.compileExprFromAst(buffer, index.lhs.*);
                            try self.compileExprFromAst(buffer, index.rhs.*);
                            try buffer.append(ast.Instruction{ .push = index.elem_type.size() });
                            try buffer.append(ast.Instruction{ .mul = true });
                            try buffer.append(ast.Instruction{ .add = true });
                        } else if (std.mem.eql(u8, apply.name, "vec")) {
                            try self.callFunction(buffer, ast.Expression{ .var_ = "get_vec_int" }, @constCast(&[_]ast.Expression{
                                index.lhs.*,
                                index.rhs.*,
                            }));
                        } else if (std.mem.eql(u8, apply.name, "slice")) {
                            const valueType = try index.type_.getValueType(self.type_defs.?, self.ast_arena_allocator.allocator());
                            switch (valueType) {
                                .int => {
                                    try self.callFunction(buffer, ast.Expression{ .var_ = "get_slice_int" }, @constCast(&[_]ast.Expression{
                                        index.lhs.*,
                                        index.rhs.*,
                                    }));
                                },
                                .byte => {
                                    try self.callFunction(buffer, ast.Expression{ .var_ = "get_slice_byte" }, @constCast(&[_]ast.Expression{
                                        index.lhs.*,
                                        index.rhs.*,
                                    }));
                                },
                                else => {
                                    try self.compileExprFromAst(buffer, index.lhs.*);
                                    try buffer.append(ast.Instruction{ .load = 8 });
                                    try self.compileExprFromAst(buffer, index.rhs.*);
                                    try buffer.append(ast.Instruction{ .push = valueType.size() });
                                    try buffer.append(ast.Instruction{ .mul = true });
                                    try buffer.append(ast.Instruction{ .add = true });
                                    try buffer.append(ast.Instruction{ .load = valueType.size() });
                                },
                            }
                        } else if (std.mem.eql(u8, apply.name, "map")) {
                            try self.compileExprFromAst(buffer, index.lhs.*);
                            try self.compileExprFromAst(buffer, index.rhs.*);
                            try buffer.append(ast.Instruction{ .table_get = true });
                        } else {
                            std.log.err("Invalid index type: {any}\n", .{index.type_});
                            unreachable;
                        }
                    },
                    .ptr => |ptr| {
                        try self.compileLhsExprFromAst(buffer, expr);
                        try buffer.append(ast.Instruction{ .load = ptr.type_.size() });
                    },
                    else => {
                        std.log.err("Invalid index type: {any} {any}[{any}]\n", .{ index.type_, index.lhs, index.rhs });
                        unreachable;
                    },
                }
            },
            .new => |new| {
                try self.compileNewExpr(buffer, new);
            },
            .project => |project| {
                try self.compileLhsExprFromAst(buffer, expr);
                try buffer.append(ast.Instruction{ .load = project.result_type.size() });
            },
            .as => |as| {
                try self.compileExprFromAst(buffer, as.lhs.*);
            },
            .type_ => {
                // TODO: type parameter runtime representation
                try buffer.append(ast.Instruction{ .push = -1 });
            },
        }
    }

    fn compileNewExpr(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), new: ast.NewExpr) anyerror!void {
        switch (new.type_) {
            .struct_ => |struct_| {
                std.debug.assert(new.initializers.len == struct_.len);

                // TODO: calculate the size of each field
                try self.callFunction(buffer, ast.Expression{ .var_ = "allocate_memory" }, @constCast(&[_]ast.Expression{
                    .{ .literal = .{ .number = @intCast(struct_.len * 8) } },
                }));

                var fields = try self.ast_arena_allocator.allocator().alloc(ast.Expression, struct_.len);
                for (new.initializers) |sinit| {
                    var offset: usize = 0;
                    for (struct_, 0..) |field, i| {
                        if (std.mem.eql(u8, field.name, sinit.field)) {
                            offset = i;
                            break;
                        }
                    }
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
            .ident => |ident| {
                const def = self.type_defs.?.get(ident).?;

                std.debug.assert(new.initializers.len == def.fields.len);

                // TODO: calculate the size of each field
                try self.callFunction(buffer, ast.Expression{ .var_ = "allocate_memory" }, @constCast(&[_]ast.Expression{
                    .{ .literal = .{ .number = @intCast(def.fields.len * 8) } },
                }));

                var fields = try self.ast_arena_allocator.allocator().alloc(ast.Expression, def.fields.len);
                for (new.initializers) |sinit| {
                    const offset = try def.getFieldOffset(sinit.field);
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
            .apply => |apply| {
                if (std.mem.eql(u8, apply.name, "map")) {
                    std.debug.assert(new.initializers.len == 0);

                    // TODO: growable capacity
                    try self.callFunction(buffer, ast.Expression{ .var_ = "allocate_memory" }, @constCast(&[_]ast.Expression{
                        .{ .literal = .{ .number = @intCast(128 * @as(usize, apply.params[1].size())) } },
                    }));
                } else if (std.mem.eql(u8, apply.name, "vec")) {
                    std.debug.assert(new.initializers.len == 0);

                    try self.callFunction(buffer, ast.Expression{ .var_ = "new_vec" }, @constCast(&[_]ast.Expression{
                        .{ .literal = .{ .number = @intCast(apply.params[0].size()) } },
                        .{ .literal = .{ .number = @intCast(128) } },
                    }));
                    return;
                } else {
                    var def = self.type_defs.?.get(apply.name).?;
                    def = try def.apply(self.ast_arena_allocator.allocator(), apply.params);

                    std.debug.assert(new.initializers.len == def.fields.len);

                    // TODO: calculate the size of each field
                    try self.callFunction(buffer, ast.Expression{ .var_ = "allocate_memory" }, @constCast(&[_]ast.Expression{
                        .{ .literal = .{ .number = @intCast(def.fields.len * 8) } },
                    }));

                    var fields = try self.ast_arena_allocator.allocator().alloc(ast.Expression, def.fields.len);
                    for (new.initializers) |sinit| {
                        const offset = try def.getFieldOffset(sinit.field);
                        fields[offset] = sinit.value;
                    }

                    for (fields, 0..) |e, i| {
                        try buffer.append(ast.Instruction{ .get_local_d = self.env_offset });
                        try buffer.append(ast.Instruction{ .push = @intCast(i * 8) });
                        try buffer.append(ast.Instruction{ .add = true });
                        try self.compileExprFromAst(buffer, e);
                        try buffer.append(ast.Instruction{ .store = 8 });
                    }
                }
            },
            else => {
                std.log.err("Invalid new type: {any}\n", .{new.type_});
                unreachable;
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
                std.debug.assert(index.elem_type != .unknown);

                switch (index.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "array")) {
                            try self.compileExprFromAst(buffer, index.lhs.*);
                            try self.compileExprFromAst(buffer, index.rhs.*);
                            try buffer.append(ast.Instruction{ .push = index.elem_type.size() });
                            try buffer.append(ast.Instruction{ .mul = true });
                            try buffer.append(ast.Instruction{ .add = true });
                        } else if (std.mem.eql(u8, apply.name, "vec")) {
                            try self.callFunction(buffer, ast.Expression{ .var_ = "get_vec_int" }, @constCast(&[_]ast.Expression{
                                index.lhs.*,
                                index.rhs.*,
                            }));
                        } else if (std.mem.eql(u8, apply.name, "slice")) {
                            const valueType = try index.type_.getValueType(self.type_defs.?, self.ast_arena_allocator.allocator());
                            switch (valueType) {
                                .int => {
                                    try self.callFunction(buffer, ast.Expression{ .var_ = "get_slice_int" }, @constCast(&[_]ast.Expression{
                                        index.lhs.*,
                                        index.rhs.*,
                                    }));
                                },
                                .byte => {
                                    try self.callFunction(buffer, ast.Expression{ .var_ = "get_slice_byte" }, @constCast(&[_]ast.Expression{
                                        index.lhs.*,
                                        index.rhs.*,
                                    }));
                                },
                                else => {
                                    try self.compileExprFromAst(buffer, index.lhs.*);
                                    try buffer.append(ast.Instruction{ .load = 8 });
                                    try self.compileExprFromAst(buffer, index.rhs.*);
                                    try buffer.append(ast.Instruction{ .push = valueType.size() });
                                    try buffer.append(ast.Instruction{ .mul = true });
                                    try buffer.append(ast.Instruction{ .add = true });
                                    try buffer.append(ast.Instruction{ .load = valueType.size() });
                                },
                            }
                        } else if (std.mem.eql(u8, apply.name, "map")) {
                            try self.compileExprFromAst(buffer, index.lhs.*);
                            try self.compileExprFromAst(buffer, index.rhs.*);
                            try buffer.append(ast.Instruction{ .table_set = true });
                        } else {
                            std.log.err("Invalid index type: {s}({any})\n", .{ apply.name, apply.params });
                            unreachable;
                        }
                    },
                    .ptr => |ptr| {
                        try self.compileExprFromAst(buffer, index.lhs.*);
                        try self.compileExprFromAst(buffer, index.rhs.*);
                        try buffer.append(ast.Instruction{ .push = ptr.type_.size() });
                        try buffer.append(ast.Instruction{ .mul = true });
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                    else => {
                        std.log.err("Invalid index type: {any}\n", .{index.type_});
                        unreachable;
                    },
                }
            },
            .project => |project| {
                std.debug.assert(project.index >= 0);

                try self.compileExprFromAst(buffer, project.lhs.*);
                try buffer.append(ast.Instruction{ .push = project.index });
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
                // 変数の型情報を保存
                if (let.value == .new) {
                    try self.env_types.put(let.name, let.value.new.type_);
                }
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
                            .apply => |apply| {
                                if (std.mem.eql(u8, apply.name, "slice")) {
                                    try self.compileExprFromAst(buffer, assign.lhs.index.lhs.*);
                                    try buffer.append(ast.Instruction{ .load = 8 });
                                    try self.compileExprFromAst(buffer, assign.lhs.index.rhs.*);
                                    try buffer.append(ast.Instruction{ .push = index.elem_type.size() });
                                    try buffer.append(ast.Instruction{ .mul = true });
                                    try buffer.append(ast.Instruction{ .add = true });
                                    try self.compileExprFromAst(buffer, assign.rhs);
                                    try buffer.append(ast.Instruction{ .store = index.elem_type.size() });
                                } else if (std.mem.eql(u8, apply.name, "map")) {
                                    try self.compileExprFromAst(buffer, assign.lhs.index.lhs.*);
                                    try self.compileExprFromAst(buffer, assign.lhs.index.rhs.*);
                                    try self.compileExprFromAst(buffer, assign.rhs);
                                    try buffer.append(ast.Instruction{ .table_set = true });
                                } else if (std.mem.eql(u8, apply.name, "vec")) {
                                    switch (apply.params[0]) {
                                        .int => {
                                            try self.callFunction(buffer, ast.Expression{ .var_ = "set_vec_int" }, @constCast(&[_]ast.Expression{
                                                assign.lhs.index.lhs.*,
                                                assign.lhs.index.rhs.*,
                                                assign.rhs,
                                            }));
                                            try buffer.append(ast.Instruction{ .pop = true });
                                        },
                                        else => {
                                            unreachable;
                                        },
                                    }
                                } else {
                                    try self.compileLhsExprFromAst(buffer, assign.lhs);
                                    try self.compileExprFromAst(buffer, assign.rhs);
                                    try buffer.append(ast.Instruction{ .store = assign.type_.size() });
                                }
                            },
                            .ptr => |ptr| {
                                try self.compileLhsExprFromAst(buffer, assign.lhs);
                                try self.compileExprFromAst(buffer, assign.rhs);
                                try buffer.append(ast.Instruction{ .store = ptr.type_.size() });
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
                    .apply => |apply| {
                        std.debug.assert(std.mem.eql(u8, apply.name, "vec"));

                        try self.callFunction(buffer, ast.Expression{ .var_ = "push_vec_int" }, @constCast(&[_]ast.Expression{
                            push.lhs,
                            push.rhs,
                        }));
                        try buffer.append(ast.Instruction{ .pop = true });
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
        body: ast.Block,
    ) anyerror![]ast.Instruction {
        var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        try self.compileBlockFromAst(&buffer, body);

        return buffer.items;
    }

    fn compileDecl(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), decl: ast.Decl, label_override: ?[]const u8) anyerror!void {
        switch (decl) {
            .fun => |f| {
                self.env_offset = 0;
                self.env.clearAndFree();
                self.env_types.clearAndFree();

                const label = label_override orelse f.name;

                try buffer.append(ast.Instruction{ .label = label });

                var index: i32 = -3;

                // register names in the reverse order
                for (0..f.params.len) |ri| {
                    const i = f.params.len - ri - 1;
                    try self.env.put(f.params[i].name, index);
                    try self.env_types.put(f.params[i].name, f.params[i].type_);
                    index -= 1;
                }
                // register type_params in the reverse order
                for (0..f.type_params.len) |ri| {
                    const i = f.type_params.len - ri - 1;
                    try self.env.put(f.type_params[i], index);
                    index -= 1;
                }
                // register return value
                try self.env.put("return", index);

                try buffer.appendSlice(try self.compileBlock(f.body));

                const end_of_f = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "end_of_{s}", .{label});
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
            .type_ => |t| {
                for (t.methods) |m| {
                    try self.compileDecl(buffer, m, null);
                }
            },
        }
    }

    fn compileModule(self: *VmCompiler, buffer: *std.ArrayList(ast.Instruction), module: ast.Module) anyerror!void {
        for (module.decls) |decl| {
            try self.compileDecl(buffer, decl, null);
        }

        for (module.decls) |decl| {
            switch (decl) {
                .fun => |fun| {
                    for (self.generic_calls.items) |generic_call| {
                        if (std.mem.eql(u8, generic_call.name, fun.name) and generic_call.types.len > 0) {
                            var label = std.ArrayList(u8).init(self.ast_arena_allocator.allocator());

                            try label.appendSlice(fun.name);
                            for (generic_call.types) |type_| {
                                try std.fmt.format(label.writer(), "_{any}", .{type_});
                            }

                            try self.compileDecl(buffer, decl, label.items);
                        }
                    }
                },
                else => {},
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
                            .type_args = &[_]ast.Type{},
                            .callee = @constCast(&ast.Expression{ .var_ = entrypoint }),
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
        self.type_defs = module.type_defs;
        self.generic_calls.clearAndFree();
        try self.generic_calls.appendSlice(module.generic_calls);

        var initBuffer = std.ArrayList(ast.Instruction).init(self.allocator);
        defer initBuffer.deinit();

        // ==== global section
        //  0: null_ptr
        //  8: data_section_ptr
        // 16: global_section_ptr
        // 24: heap_section_ptr
        // 32: heap_ptr
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

        const sizeBuiltinGlobalSection = 32;

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

        // set heap_ptr
        try initBuffer.append(ast.Instruction{ .push = @intCast(heap_ptr) });
        try initBuffer.append(ast.Instruction{ .push = @intCast(sizeBuiltinGlobalSection + sizeGlobalSection + sizeDataSection + 8) });
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
                    } else if (program[i - 1].is_mul() and program[i - 2].is_get_local_d() and program[i - 3].is_get_local_d()) {
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
