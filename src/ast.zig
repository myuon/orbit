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
};

pub const TokenType = enum {
    keyword,
    number,
    ident,
};

pub const Token = union(TokenType) {
    keyword: Operator,
    number: u32,
    ident: []const u8,
};

pub const ExpressionType = enum {
    var_,
    literal,
    binop,
    block,
    call,
    if_,
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
        name: []const u8,
        value: Expression,
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
    call,
    call_d,
    get_local,
    set_local,
    register_local,
    clone_env,
    restore_env,
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
    call: []const u8, // (resolve_label phase) replaced with call_d
    call_d: usize,
    get_local: []const u8, // (resolve_local phase) replaced with get_local_d
    set_local: []const u8, // (resolve_local phase) replaced with set_local_d
    register_local: struct { name: []const u8, offset: i64 }, // (resolve_local phase) replaced with nop
    clone_env: bool,
    restore_env: bool,
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
};
