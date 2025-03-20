const std = @import("std");
const utils = @import("utils.zig");
const ast = @import("ast.zig");
const P = @import("profiler");

pub const LexerError = error{
    UnexpectedToken,
};

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    source: []const u8,
    position: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return Lexer{
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .source = source,
            .position = 0,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.ast_arena_allocator.deinit();
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.position >= self.source.len) {
            return null;
        }

        return self.source[self.position];
    }

    fn consume(self: *Lexer) ?u8 {
        if (self.peek()) |ch| {
            self.position += 1;
            return ch;
        } else {
            return null;
        }
    }

    fn consume_hex_number(self: *Lexer) ?u32 {
        if (!utils.startsWith(self.source[self.position..], "0x")) {
            return null;
        }

        self.position += 2;

        var number: u32 = 0;
        while (self.peek()) |ch| {
            if (ch >= '0' and ch <= '9') {
                number = number * 16 + @as(u32, (ch - @as(u8, '0')));
            } else if (ch >= 'a' and ch <= 'f') {
                number = number * 16 + @as(u32, (ch - @as(u8, 'a') + 10));
            } else if (ch >= 'A' and ch <= 'F') {
                number = number * 16 + @as(u32, (ch - @as(u8, 'A') + 10));
            } else {
                break;
            }

            _ = self.consume();
        }

        return number;
    }

    fn consume_number(self: *Lexer) ?u32 {
        var happened = false;
        var number: u32 = 0;
        while (self.peek()) |ch| {
            if (ch < '0' or ch > '9') {
                break;
            }

            number = number * 10 + @as(u32, (ch - @as(u8, '0')));
            _ = self.consume();
            happened = true;
        }

        if (!happened) {
            return null;
        }
        return number;
    }

    fn consume_string(self: *Lexer) ?[]const u8 {
        const start = self.position;
        if (self.consume() != '"') {
            return null;
        }

        while (self.peek()) |ch| {
            if (ch == '"') {
                self.position += 1;
                return self.source[start + 1 .. self.position - 1];
            }

            _ = self.consume();
        }

        return null;
    }

    fn consume_spaces(self: *Lexer) usize {
        var n: usize = 0;

        while (self.peek()) |ch| {
            if (ch == ' ' or ch == '\n' or ch == '\t') {
                _ = self.consume();
                n += 1;
            } else {
                break;
            }
        }

        return n;
    }

    fn consume_token(self: *Lexer) ?ast.Token {
        const tokenTable = comptime [_]struct { str: []const u8, operator: ast.Operator }{
            .{ .str = "+", .operator = ast.Operator.plus },
            .{ .str = "-", .operator = ast.Operator.minus },
            .{ .str = "*", .operator = ast.Operator.star },
            .{ .str = ":", .operator = ast.Operator.colon },
            .{ .str = ";", .operator = ast.Operator.semicolon },
            .{ .str = ".", .operator = ast.Operator.dot },
            .{ .str = ",", .operator = ast.Operator.comma },
            .{ .str = "==", .operator = ast.Operator.eqeq },
            .{ .str = "<=", .operator = ast.Operator.lte },
            .{ .str = ">=", .operator = ast.Operator.gte },
            .{ .str = "<-", .operator = ast.Operator.push },
            .{ .str = "=", .operator = ast.Operator.eq },
            .{ .str = "(", .operator = ast.Operator.lparen },
            .{ .str = ")", .operator = ast.Operator.rparen },
            .{ .str = "<", .operator = ast.Operator.langle },
            .{ .str = ">", .operator = ast.Operator.rangle },
            .{ .str = "[", .operator = ast.Operator.lbracket },
            .{ .str = "]", .operator = ast.Operator.rbracket },
            .{ .str = "{", .operator = ast.Operator.lbrace },
            .{ .str = "}", .operator = ast.Operator.rbrace },
            .{ .str = "%", .operator = ast.Operator.percent },
        };

        for (tokenTable) |token| {
            if (utils.startsWith(self.source[self.position..], token.str)) {
                self.position += token.str.len;
                return ast.Token{ .keyword = token.operator };
            }
        }

        const keywordTable = comptime [_]struct { str: []const u8, operator: ast.Operator }{
            .{ .str = "let", .operator = ast.Operator.let },
            .{ .str = "do", .operator = ast.Operator.do },
            .{ .str = "end", .operator = ast.Operator.end },
            .{ .str = "fun", .operator = ast.Operator.fun },
            .{ .str = "return", .operator = ast.Operator.return_ },
            .{ .str = "if", .operator = ast.Operator.if_ },
            .{ .str = "else", .operator = ast.Operator.else_ },
            .{ .str = "while", .operator = ast.Operator.while_ },
            .{ .str = "true", .operator = ast.Operator.true_ },
            .{ .str = "false", .operator = ast.Operator.false_ },
            .{ .str = "new", .operator = ast.Operator.new },
            .{ .str = "struct", .operator = ast.Operator.struct_ },
            .{ .str = "as", .operator = ast.Operator.as },
            .{ .str = "type", .operator = ast.Operator.type_ },
            .{ .str = "extends", .operator = ast.Operator.extends },
            .{ .str = "sizeof", .operator = ast.Operator.sizeof },
        };

        for (keywordTable) |keyword| {
            if (utils.startsWith(self.source[self.position..], keyword.str) and
                (self.position + keyword.str.len >= self.source.len or !isIdentChar(self.source[self.position + keyword.str.len])))
            {
                self.position += keyword.str.len;
                return ast.Token{ .keyword = keyword.operator };
            }
        }

        return null;
    }

    fn isIdentChar(ch: u8) bool {
        return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_' or (ch >= '0' and ch <= '9');
    }

    fn consume_ident(self: *Lexer) ?[]const u8 {
        const start = self.position;
        while (self.peek()) |ch| {
            if (!isIdentChar(ch)) {
                break;
            }

            _ = self.consume();
        }

        if (start == self.position) {
            return null;
        }
        return self.source[start..self.position];
    }

    fn consume_line_comment(self: *Lexer) bool {
        if (utils.startsWith(self.source[self.position..], "//")) {
            self.position += 2;

            while (self.peek()) |next_ch| {
                if (next_ch == '\n') {
                    break;
                }
                _ = self.consume();
            }
            return true;
        }

        return false;
    }

    pub fn run(self: *Lexer) anyerror!std.ArrayList(ast.Token) {
        const zone = P.begin(@src(), "Lexer.run");
        defer zone.end();

        var tokens = std.ArrayList(ast.Token).init(self.ast_arena_allocator.allocator());

        while (self.position < self.source.len) {
            if (self.consume_spaces() > 0) {
                continue;
            }

            if (self.consume_line_comment()) {
                continue;
            }

            if (self.consume_token()) |token| {
                try tokens.append(token);
                continue;
            }

            if (self.consume_hex_number()) |number| {
                try tokens.append(ast.Token{ .number = number });
                continue;
            }

            if (self.consume_number()) |number| {
                try tokens.append(ast.Token{ .number = number });
                continue;
            }

            if (self.consume_ident()) |ident| {
                try tokens.append(ast.Token{ .ident = ident });
                continue;
            }

            if (self.consume_string()) |string| {
                try tokens.append(ast.Token{ .string = string });
                continue;
            }

            std.debug.print("unexpected token: {s}\n", .{self.source[self.position..]});
            return error.UnexpectedToken;
        }

        return tokens;
    }
};

test {
    var lexer = Lexer.init(std.testing.allocator, "1 + 20 * 300");
    defer lexer.deinit();

    const result = try lexer.run();

    var expected = std.ArrayList(ast.Token).init(std.testing.allocator);
    defer expected.deinit();
    try expected.append(ast.Token{ .number = 1 });
    try expected.append(ast.Token{ .keyword = ast.Operator.plus });
    try expected.append(ast.Token{ .number = 20 });
    try expected.append(ast.Token{ .keyword = ast.Operator.star });
    try expected.append(ast.Token{ .number = 300 });

    try std.testing.expectEqualSlices(ast.Token, expected.items, result.items);
}

test {
    var lexer = Lexer.init(std.testing.allocator, "let k = \"hello\";");
    defer lexer.deinit();

    const result = try lexer.run();

    var expected = std.ArrayList(ast.Token).init(std.testing.allocator);
    defer expected.deinit();
    try expected.append(ast.Token{ .keyword = ast.Operator.let });
    try expected.append(ast.Token{ .ident = "k" });
    try expected.append(ast.Token{ .keyword = ast.Operator.eq });
    try expected.append(ast.Token{ .string = "hello" });
    try expected.append(ast.Token{ .keyword = ast.Operator.semicolon });

    try std.testing.expectEqual(result.items.len, expected.items.len);
    try std.testing.expectEqualDeep(expected.items, result.items);
}
