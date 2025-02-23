const std = @import("std");

pub const Operator = enum {
    eqeq,
    lte,
    gte,
    eq,
    colon,
    semicolon,
    dot,
    comma,
    lparen,
    rparen,
    langle,
    rangle,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    plus,
    minus,
    star,
    percent,
    push,
    let,
    do,
    end,
    fun,
    return_,
    if_,
    else_,
    while_,
    true_,
    false_,
    new,
    struct_,
};

pub const TokenType = enum {
    keyword,
    number,
    ident,
    string,
};

pub const Token = union(TokenType) {
    keyword: Operator,
    number: u32,
    ident: []const u8,
    string: []const u8,
};

pub const StructInitializer = struct {
    field: []const u8,
    value: Expression,
};

pub const ExpressionType = enum {
    var_,
    literal,
    binop,
    block,
    call,
    if_,
    index,
    new,
    project,
};

pub const Expression = union(ExpressionType) {
    var_: []const u8,
    literal: Literal,
    binop: struct {
        op: Operator,
        lhs: *Expression,
        rhs: *Expression,
    },
    block: Block,
    call: struct {
        name: []const u8,
        args: []Expression,
    },
    if_: struct {
        cond: *Expression,
        then_: Block,
        else_: Block,
    },
    index: struct {
        type_: Type,
        lhs: *Expression,
        rhs: *Expression,
    },
    new: struct {
        type_: Type,
        initializers: []StructInitializer,
    },
    project: struct {
        struct_: StructData,
        lhs: *Expression,
        rhs: []const u8,
    },
};

pub const LiteralType = enum {
    boolean,
    number,
    string,
};

pub const Literal = union(LiteralType) {
    boolean: bool,
    number: u32,
    string: []const u8,
};

pub const StatementType = enum {
    let,
    return_,
    expr,
    if_,
    assign,
    push,
    while_,
};

pub const Statement = union(StatementType) {
    let: struct {
        name: []const u8,
        value: Expression,
    },
    return_: Expression,
    expr: Expression,
    if_: struct {
        cond: *Expression,
        then_: Block,
        else_: ?Block,
    },
    assign: struct {
        type_: Type,
        lhs: Expression,
        rhs: Expression,
    },
    push: struct {
        type_: Type,
        lhs: Expression,
        rhs: Expression,
    },
    while_: struct {
        cond: *Expression,
        body: Block,
    },
};

pub const Block = struct {
    statements: []Statement,
    expr: ?*Expression,
};

pub const DeclType = enum {
    fun,
};

pub const Decl = union(DeclType) {
    fun: struct {
        name: []const u8,
        params: []const []const u8,
        body: Block,
    },
};

pub const Module = struct {
    decls: []Decl,
};

pub const TypeType = enum {
    unknown,
    bool_,
    byte,
    int,
    array,
    slice,
    vec,
    map,
    fun,
    struct_,
};

pub const AstTypeError = error{
    UnexpectedType,
};

pub const StructField = struct {
    name: []const u8,
    type_: Type,
};

pub const StructData = struct {
    fields: []StructField,

    pub fn getFieldType(self: StructData, name: []const u8) !Type {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return field.type_;
            }
        }

        unreachable;
    }

    pub fn getFieldOffset(self: StructData, name: []const u8) !usize {
        for (self.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name)) {
                return i;
            }
        }

        std.log.err("Field not found: {s} in {any}\n", .{ name, self });
        unreachable;
    }
};

pub const Type = union(TypeType) {
    unknown: bool,
    bool_: bool,
    byte: bool,
    int: bool,
    array: struct {
        elem_type: *Type,
        size: usize,
    },
    slice: struct {
        elem_type: *Type,
    },
    vec: struct {
        elem_type: *Type,
    },
    map: struct {
        key_type: *Type,
        value_type: *Type,
    },
    fun: struct {
        params: []Type,
        return_type: *Type,
    },
    struct_: StructData,

    pub fn size(self: Type) u4 {
        return switch (self) {
            Type.unknown => unreachable,
            Type.bool_ => 1,
            Type.byte => 1,
            Type.int => 8,
            Type.array => 8,
            Type.slice => 8,
            Type.vec => 8,
            Type.map => 8,
            Type.fun => unreachable,
            Type.struct_ => 8,
        };
    }

    pub fn getIndexType(actual: Type) anyerror!Type {
        switch (actual) {
            .array => {
                return Type{ .int = true };
            },
            .slice => {
                return Type{ .int = true };
            },
            .map => |map| {
                return map.key_type.*;
            },
            .vec => {
                return Type{ .int = true };
            },
            else => {
                std.log.err("Expected array-like data structure, got {any} ({})\n", .{ actual, @src() });
                return error.UnexpectedType;
            },
        }
    }

    pub fn getValueType(actual: Type) anyerror!Type {
        switch (actual) {
            .array => |array| {
                return array.elem_type.*;
            },
            .slice => |slice| {
                return slice.elem_type.*;
            },
            .map => |map| {
                return map.value_type.*;
            },
            .vec => |vec| {
                return vec.elem_type.*;
            },
            else => {
                std.log.err("Expected array-like data structure, got {any} ({})\n", .{ actual, @src() });
                return error.UnexpectedType;
            },
        }
    }

    pub fn getStructData(t: Type) anyerror!StructData {
        return switch (t) {
            Type.struct_ => t.struct_,
            else => error.UnexpectedType,
        };
    }
};

pub const ValueError = error{
    UnexpectedType,
};

pub const ValueType = enum {
    nil,
    i64_,
    bool_,
};

pub const Value = union(ValueType) {
    nil: bool,
    i64_: i64,
    bool_: bool,

    pub fn asI64(self: Value) ValueError!i64 {
        return switch (self) {
            Value.i64_ => self.i64_,
            else => error.UnexpectedType,
        };
    }

    pub fn asBool(self: Value) ValueError!bool {
        return switch (self) {
            Value.bool_ => self.bool_,
            else => error.UnexpectedType,
        };
    }
};

pub const InstructionType = enum {
    push,
    pop,
    ret,
    eq,
    jump,
    jump_ifzero,
    add,
    add_di,
    madd_d,
    sub,
    mul,
    div,
    call,
    call_d,
    label,
    get_pc,
    get_bp,
    set_bp,
    get_sp,
    set_sp,
    jump_d,
    jump_ifzero_d,
    get_local_d,
    set_local_d,
    nop,
    mod,
    lt,
    lt_d,
    lte,
    gt,
    gte,
    load,
    store,
    set_memory,
    allocate_memory,
    set_cip,
    table_set,
    table_get,
    allocate_vec,
    vec_get,
    vec_set,
    vec_push,
};

pub const Instruction = union(InstructionType) {
    push: i32,
    pop: bool,
    ret: bool,
    eq: bool,
    jump: []const u8, // (resolve_label phase) replaced with jump_d
    jump_ifzero: []const u8, // (resolve_label phase) replaced with jump_ifzero_d
    add: bool,
    add_di: struct {
        lhs: i32,
        imm: i32,
        target: ?i32,
    },
    madd_d: struct {
        lhs: i32,
        rhs: i32,
        base: i32,
        target: ?i32,
    },
    sub: bool,
    mul: bool,
    div: bool,
    call: []const u8, // (resolve_label phase) replaced with call_d
    call_d: usize,
    label: []const u8, // (resolve_label phase) replaced with nop
    get_pc: bool,
    get_bp: bool,
    set_bp: bool,
    get_sp: bool,
    set_sp: bool,
    jump_d: usize,
    jump_ifzero_d: usize,
    get_local_d: i32,
    set_local_d: i32,
    nop: bool,
    mod: bool,
    lt: bool,
    lt_d: struct {
        lhs: i32,
        rhs: i32,
    },
    lte: bool,
    gt: bool,
    gte: bool,
    load: u4,
    store: u4,
    set_memory: struct {
        data: []const u8,
        offset: usize,
    },
    allocate_memory: usize,
    set_cip: usize, // For tracing JIT
    table_set: bool, // For hashmap
    table_get: bool, // For hashmap
    allocate_vec: u4, // For vector
    vec_get: bool, // For vector
    vec_set: bool, // For vector
    vec_push: bool, // For vector

    pub fn format(
        self: Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            Instruction.push => {
                try std.fmt.format(writer, "push #{d}", .{self.push});
            },
            Instruction.pop => {
                try std.fmt.format(writer, "pop", .{});
            },
            Instruction.ret => {
                try std.fmt.format(writer, "ret", .{});
            },
            Instruction.eq => {
                try std.fmt.format(writer, "eq", .{});
            },
            Instruction.jump => {
                try std.fmt.format(writer, "jump {s}", .{self.jump});
            },
            Instruction.jump_ifzero => {
                try std.fmt.format(writer, "jump_ifzero {s}", .{self.jump_ifzero});
            },
            Instruction.add => {
                try std.fmt.format(writer, "add", .{});
            },
            Instruction.sub => {
                try std.fmt.format(writer, "sub", .{});
            },
            Instruction.mul => {
                try std.fmt.format(writer, "mul", .{});
            },
            Instruction.div => {
                try std.fmt.format(writer, "div", .{});
            },
            Instruction.call => {
                try std.fmt.format(writer, "call {s}", .{self.call});
            },
            Instruction.call_d => {
                try std.fmt.format(writer, "call_d #{d}", .{self.call_d});
            },
            Instruction.label => {
                try std.fmt.format(writer, "label {s}", .{self.label});
            },
            Instruction.get_pc => {
                try std.fmt.format(writer, "get_pc", .{});
            },
            Instruction.get_bp => {
                try std.fmt.format(writer, "get_bp", .{});
            },
            Instruction.set_bp => {
                try std.fmt.format(writer, "set_bp", .{});
            },
            Instruction.get_sp => {
                try std.fmt.format(writer, "get_sp", .{});
            },
            Instruction.set_sp => {
                try std.fmt.format(writer, "set_sp", .{});
            },
            Instruction.jump_d => {
                try std.fmt.format(writer, "jump_d #{d}", .{self.jump_d});
            },
            Instruction.jump_ifzero_d => {
                try std.fmt.format(writer, "jump_ifzero_d #{d}", .{self.jump_ifzero_d});
            },
            Instruction.get_local_d => {
                try std.fmt.format(writer, "get_local_d [{d}]", .{self.get_local_d});
            },
            Instruction.set_local_d => {
                try std.fmt.format(writer, "set_local_d [{d}]", .{self.set_local_d});
            },
            Instruction.nop => {
                try std.fmt.format(writer, "nop", .{});
            },
            Instruction.mod => {
                try std.fmt.format(writer, "mod", .{});
            },
            Instruction.lt => {
                try std.fmt.format(writer, "lt", .{});
            },
            Instruction.lte => {
                try std.fmt.format(writer, "lte", .{});
            },
            Instruction.gt => {
                try std.fmt.format(writer, "gt", .{});
            },
            Instruction.gte => {
                try std.fmt.format(writer, "gte", .{});
            },
            Instruction.load => {
                try std.fmt.format(writer, "load_{d}", .{self.load});
            },
            Instruction.store => {
                try std.fmt.format(writer, "store_{d}", .{self.store});
            },
            Instruction.set_memory => {
                try std.fmt.format(writer, "set_memory #{s} #{d}", .{ self.set_memory.data, self.set_memory.offset });
            },
            Instruction.allocate_memory => {
                try std.fmt.format(writer, "allocate_memory #{d}", .{self.allocate_memory});
            },
            Instruction.set_cip => {
                try std.fmt.format(writer, "set_cip #{d}", .{self.set_cip});
            },
            Instruction.lt_d => {
                try std.fmt.format(writer, "lt_d [{d}] [{d}]", .{ self.lt_d.lhs, self.lt_d.rhs });
            },
            Instruction.add_di => {
                if (self.add_di.target) |t| {
                    try std.fmt.format(writer, "add_di [{d}] #{d} -> [{d}]", .{ self.add_di.lhs, self.add_di.imm, t });
                } else {
                    try std.fmt.format(writer, "add_di [{d}] #{d}", .{ self.add_di.lhs, self.add_di.imm });
                }
            },
            Instruction.madd_d => {
                if (self.madd_d.target) |t| {
                    try std.fmt.format(writer, "madd_d [{d}] [{d}] [{d}] -> [{d}]", .{ self.madd_d.lhs, self.madd_d.rhs, self.madd_d.base, t });
                } else {
                    try std.fmt.format(writer, "madd_d [{d}] [{d}] [{d}]", .{ self.madd_d.lhs, self.madd_d.rhs, self.madd_d.base });
                }
            },
            Instruction.table_set => {
                try std.fmt.format(writer, "table_set", .{});
            },
            Instruction.table_get => {
                try std.fmt.format(writer, "table_get", .{});
            },
            Instruction.allocate_vec => {
                try std.fmt.format(writer, "allocate_vec #{d}", .{self.allocate_vec});
            },
            Instruction.vec_get => {
                try std.fmt.format(writer, "vec_get", .{});
            },
            Instruction.vec_set => {
                try std.fmt.format(writer, "vec_set", .{});
            },
            Instruction.vec_push => {
                try std.fmt.format(writer, "vec_push", .{});
            },
        }
    }

    pub fn is_get_local_d(self: Instruction) bool {
        return switch (self) {
            Instruction.get_local_d => true,
            else => false,
        };
    }

    pub fn is_set_local_d(self: Instruction) bool {
        return switch (self) {
            Instruction.set_local_d => true,
            else => false,
        };
    }

    pub fn is_push(self: Instruction) bool {
        return switch (self) {
            Instruction.push => true,
            else => false,
        };
    }

    pub fn is_mul(self: Instruction) bool {
        return switch (self) {
            Instruction.mul => true,
            else => false,
        };
    }

    pub fn is_add(self: Instruction) bool {
        return switch (self) {
            Instruction.add => true,
            else => false,
        };
    }

    pub fn is_add_di(self: Instruction) bool {
        return switch (self) {
            Instruction.add_di => true,
            else => false,
        };
    }

    pub fn is_madd_d(self: Instruction) bool {
        return switch (self) {
            Instruction.madd_d => true,
            else => false,
        };
    }
};
