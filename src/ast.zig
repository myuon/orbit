const std = @import("std");

pub const Lexeme = enum { Plus, Star, Let, Number };

pub const Token = struct {
    lexeme: Lexeme,
    number: ?u32 = null,
};
