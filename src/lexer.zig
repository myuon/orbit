const std = @import("std");
const utils = @import("utils.zig");

const Allocator = std.heap.page_allocator;

const Lexeme = enum { Plus, Star, Let, Number };
const Token = struct {
    lexeme: Lexeme,
    number: ?u32 = null,
};

const LexerError = error{
    UnexpectedToken,
};

const Lexer = struct {
    source: []const u8,
    position: usize,

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

    fn consume_spaces(self: *Lexer) usize {
        var n: usize = 0;

        while (self.peek()) |ch| {
            if (ch != ' ') {
                break;
            }

            _ = self.consume();
            n += 1;
        }

        return n;
    }

    fn consume_token(self: *Lexer) ?Token {
        const tokenTable = comptime [_]struct { str: []const u8, lexeme: Lexeme }{
            .{ .str = "+", .lexeme = Lexeme.Plus },
            .{ .str = "*", .lexeme = Lexeme.Star },
            .{ .str = "let", .lexeme = Lexeme.Let },
        };

        for (tokenTable) |token| {
            if (utils.startsWith(self.source[self.position..], token.str)) {
                self.position += token.str.len;
                return Token{ .lexeme = token.lexeme };
            }
        }

        return null;
    }

    fn run(self: *Lexer) anyerror!std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(Allocator);

        while (self.position < self.source.len) {
            if (self.consume_spaces() > 0) {
                continue;
            }

            if (self.consume_token()) |token| {
                try tokens.append(token);
                continue;
            }

            if (self.consume_number()) |number| {
                try tokens.append(Token{ .lexeme = Lexeme.Number, .number = number });
                continue;
            }

            return error.UnexpectedToken;
        }

        return tokens;
    }
};

test "lexer" {
    var lexer = Lexer{ .source = "1 + 20 * 300", .position = 0 };
    const result = try lexer.run();

    var expected = std.ArrayList(Token).init(std.testing.allocator);
    defer expected.deinit();
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 1 });
    try expected.append(Token{ .lexeme = Lexeme.Plus });
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 20 });
    try expected.append(Token{ .lexeme = Lexeme.Star });
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 300 });

    try std.testing.expectEqualSlices(Token, expected.items, result.items);
}
