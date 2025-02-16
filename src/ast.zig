const std = @import("std");

pub const Operator = enum {
    eqeq,
    lte,
    gte,
    eq,
    semicolon,
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

pub const ExpressionType = enum {
    var_,
    literal,
    binop,
    block,
    call,
    if_,
    index,
    new,
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
        elem_type: Type,
        lhs: *Expression,
        rhs: *Expression,
    },
    new: struct {
        array_size: usize,
        initializers: []Expression,
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
    fun,
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
    fun: struct {
        params: []Type,
        return_type: *Type,
    },

    pub fn size(self: Type) u4 {
        return switch (self) {
            Type.unknown => unreachable,
            Type.bool_ => 1,
            Type.byte => 1,
            Type.int => 8,
            Type.array => 8,
            Type.slice => 8,
            Type.fun => unreachable,
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
    lte,
    gt,
    gte,
    load,
    store,
    set_memory,
    allocate_memory,
};

pub const Instruction = union(InstructionType) {
    push: i32,
    pop: bool,
    ret: bool,
    eq: bool,
    jump: []const u8, // (resolve_label phase) replaced with jump_d
    jump_ifzero: []const u8, // (resolve_label phase) replaced with jump_ifzero_d
    add: bool,
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
};
