const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const Cell = vaxis.Cell;
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;

const TuiEvent = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};

pub const Tui = struct {
    allocator: std.mem.Allocator,
    tty: vaxis.Tty,
    vx: vaxis.Vaxis,
    loop: vaxis.Loop(TuiEvent),
    texts: std.StringHashMap([]u8),

    pub fn init(allocator: std.mem.Allocator) anyerror!Tui {
        var tty = try vaxis.Tty.init();
        var vx = try vaxis.init(allocator, .{});

        var loop: vaxis.Loop(TuiEvent) = .{
            .tty = &tty,
            .vaxis = &vx,
        };
        try loop.init();

        return Tui{
            .allocator = allocator,
            .tty = tty,
            .vx = vx,
            .loop = loop,
            .texts = std.StringHashMap([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *Tui) void {
        self.loop.stop();
        self.texts.deinit();
        self.vx.deinit(self.allocator, self.tty.anyWriter());
        self.tty.deinit();
    }

    pub fn recover_panic() void {
        vaxis.panic_handler("Panic Recovered", null, null);
    }

    pub fn handle_event(self: *Tui, event: TuiEvent) anyerror!bool {
        switch (event) {
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    return false;
                } else if (key.matches('q', .{})) {
                    return false;
                }
            },

            .winsize => |ws| try self.vx.resize(self.allocator, self.tty.anyWriter(), ws),
            else => {},
        }

        return true;
    }

    pub fn draw(self: *Tui) anyerror!void {
        const win = self.vx.window();
        win.clear();

        const child = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = 50,
            .height = 60,
            .border = .{
                .where = .all,
            },
        });
        _ = child.printSegment(.{ .text = self.texts.get("ir").? }, .{});

        const child2 = win.child(.{
            .x_off = 50,
            .y_off = 0,
            .width = 80,
            .height = 60,
            .border = .{
                .where = .all,
            },
        });
        _ = child2.printSegment(.{ .text = self.texts.get("stack").? }, .{});

        const child3 = win.child(.{
            .x_off = 130,
            .y_off = 0,
            .width = 60,
            .height = 60,
            .border = .{
                .where = .all,
            },
        });
        _ = child3.printSegment(.{ .text = self.texts.get("memory").? }, .{});

        try self.vx.render(self.tty.anyWriter());
    }

    pub fn start(self: *Tui) anyerror!void {
        try self.loop.start();
        try self.vx.enterAltScreen(self.tty.anyWriter());
        try self.vx.queryTerminal(self.tty.anyWriter(), 1 * std.time.ns_per_s);
    }

    pub fn fetchEvent(self: *Tui) !TuiEvent {
        return self.loop.nextEvent();
    }

    pub fn set_text(self: *Tui, key: []const u8, text: []u8) anyerror!void {
        try self.texts.put(key, text);
    }
};
