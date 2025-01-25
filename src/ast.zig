const std = @import("std");

pub const Operator = enum {
    plus,
    star,
    let,
};

pub const TokenType = enum {
    keyword,
    number,
};

pub const Token = union(TokenType) {
    keyword: Operator,
    number: u32,
};

pub const ExpressionType = enum {
    literal,
    binop,
};

pub const Expression = union(ExpressionType) {
    literal: Literal,
    binop: struct {
        op: Operator,
        lhs: *Expression,
        rhs: *Expression,
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
