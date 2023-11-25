const std = @import("std");
const xzb = @This();

const assert = std.debug.assert;

// TODO: enum type for depth?
// TODO: wrapper types for Window, Pixmap, etc that hold their connections
// TODO: rename different things to be a bit clearer
// TODO: reorganize everything, maybe into multiple files
// TODO: add lots of error checking
// TODO: abstract out the value_list generation pattern I'm using in Window and GraphicsContext
// TODO: add doc comments
// TODO: Change masks to be enum sets from the standard library (or IntegerBitSet directly? might be the only way)
// TODO: should I prefer checked or unchecked functions? requesting a check is blocking but how bad is this in practice?
// TODO: remove parameter names from extern function definitions

const log = std.log.scoped(.xzb);

const raw_allocator = std.heap.raw_c_allocator;

pub const copy_from_parent = 0;

pub const Timestamp = enum(u32) {
    current = 0,
    _,
};
pub fn timestamp(time: u32) Timestamp {
    return @enumFromInt(time);
}

pub const Keycode = enum(u8) { any = 0, _ };
// pub const Keysym = @import("keysym.zig").Keysym;
pub usingnamespace @import("keysym.zig");
// pub const Keysym = xzb.Keysym;
// pub fn keysymToNames(keysym: Keysym) []const u8 {
//     var buf: [10]u8 = undefined;
//     const keysym_string = std.fmt.bufPrint(&buf, "{d}", .{@intFromEnum(keysym)}) catch unreachable;
//     inline for (@TypeOf(std.meta.fields(xzb.KeysymValues))) |field| {
//         if (std.meta.eql(u8, keysym_string, field.name)) {
//             return @field(xzb.KeysymValues, keysym_string);
//         }
//     }
//     log.err("No names for keysym value {s}!", .{keysym_string});
//     return "";
// }
// pub fn keysym(value: u32) Keysym {
//     return @enumFromInt(Keysym, value);
// }
pub const MouseButton = enum(u8) {
    any = 0,
    left = 1,
    middle = 2,
    right = 3,
    scroll_up = 4,
    scroll_down = 5,
    scroll_left = 6,
    scroll_right = 7,
    _,
};

pub const Connection = opaque {
    pub fn connect(display_name: ?[*:0]const u8, screen: ?*c_int) !*Connection {
        const result = xcb_connect(display_name, screen);
        try result.checkError();
        return result;
    }
    extern fn xcb_connect(displayname: ?[*:0]const u8, screenp: ?*c_int) *Connection;

    // pub fn connectWithAuthInfo(display_name: ?[*:0]const u8, screen: ?*c_int) !*Connection {
    // }
    // extern fn xcb_connect_to_display_with_auth_info(displayname: ?[*:0]const u8, screenp: ?*c_int) *Connection;

    // pub fn connectToFd(display_name: ?[*:0]const u8, screen: ?*c_int) !*Connection {
    // }
    // extern fn xcb_connect_to_fd(displayname: ?[*:0]const u8, screenp: ?*c_int) *Connection;

    pub fn checkError(self: *Connection) !void {
        try xcb_connection_has_error(self).check();
    }
    const ConnectionError = enum(c_int) {
        none = 0,
        XcbConnectionError = 1,
        XcbConnectionExtNotSupported = 2,
        XcbConnectionMemoryInsufficient = 3,
        XcbConnectionRequestLengthExceed = 4,
        XcbConnectionDisplayParseError = 5,
        XcbConnectionInvalidScreen = 6,
        XcbConnectionFdPassingFailed = 7,

        pub fn check(self: ConnectionError) !void {
            return switch (self) {
                .none => {},
                .XcbConnectionError => error.XcbConnectionError,
                .XcbConnectionExtNotSupported => error.XcbConnectionExtNotSupported,
                .XcbConnectionMemoryInsufficient => error.XcbConnectionMemoryInsufficient,
                .XcbConnectionRequestLengthExceed => error.XcbConnectionRequestLengthExceed,
                .XcbConnectionDisplayParseError => error.XcbConnectionDisplayParseError,
                .XcbConnectionInvalidScreen => error.XcbConnectionInvalidScreen,
                .XcbConnectionFdPassingFailed => error.XcbConnectionFdPassingFailed,
            };
        }
    };
    extern fn xcb_connection_has_error(c: *const Connection) ConnectionError;

    pub const disconnect = xcb_disconnect;
    extern fn xcb_disconnect(c: *Connection) void;

    pub fn flush(self: *Connection) !void {
        if (xcb_flush(self) <= 0) return error.XcbFlushError;
    }
    extern fn xcb_flush(c: *Connection) c_int;

    pub fn generateId(self: *Connection, comptime T: type) !T {
        try self.checkError();
        switch (T) {
            Window, Pixmap, GraphicsContext, Colormap, Font => {},
            else => @compileError("Invalid type '" ++ @typeName(T) ++ "' for Connection.generateId()!"),
        }
        return @enumFromInt(xcb_generate_id(self));
    }
    extern fn xcb_generate_id(c: *Connection) u32;

    pub fn getSetup(self: *Connection) !*const Setup {
        try self.checkError();
        return xcb_get_setup(self);
    }
    extern fn xcb_get_setup(c: *Connection) *const Setup;

    pub fn destroy(self: *Connection, comptime T: type, thing: T) void {
        const function = switch (T) {
            Window => xcb_destroy_window,
            Pixmap => xcb_free_pixmap,
            GraphicsContext => xcb_free_gc,
            else => @compileError("Unsupported type '" ++ @typeName(T) ++ "' for Connection.destroy"),
        };
        _ = function(self, thing);
    }
    extern fn xcb_destroy_window(connection: *Connection, window: Window) VoidCookie;
    extern fn xcb_free_pixmap(c: *Connection, pixmap: Pixmap) VoidCookie;
    extern fn xcb_free_gc(c: *Connection, gc: GraphicsContext) VoidCookie;

    // TODO: figure out how *forEvent functions return errors
    // TODO: figure out how to chop off the response_type field
    // TODO: figure out when XCB_EVENT_MASK_NO_EVENT can happen, seems to be on error?
    pub fn waitForEvent(self: *Connection) !Event {
        const generic_event = xcb_wait_for_event(self) orelse return error.XcbEventIOError;
        const tag: EventType = @enumFromInt(generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                const pointer: *T = @ptrCast(generic_event);
                break :value @unionInit(Event, @tagName(event_type), pointer.*);
            },
        };

        xzb.destroy(generic_event);
        return result;
    }
    extern fn xcb_wait_for_event(c: *Connection) ?*Event.Generic;

    pub fn pollForEvent(self: *Connection) !?Event {
        const generic_event = xcb_poll_for_event(self) orelse return null;
        const tag: EventType = @enumFromInt(generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                const pointer: *T = @ptrCast(generic_event);
                break :value @unionInit(Event, @tagName(event_type), pointer.*);
            },
        };

        xzb.destroy(generic_event);
        return result;
    }
    extern fn xcb_poll_for_event(c: *Connection) ?*Event.Generic;

    pub fn pollForQueuedEvent(self: *Connection) ?Event {
        const generic_event = xcb_poll_for_queued_event(self) orelse return null;
        const tag: EventType = @enumFromInt(generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                const pointer: *T = @ptrCast(generic_event);
                break :value @unionInit(Event, @tagName(event_type), pointer.*);
            },
        };

        xzb.destroy(generic_event);
        return result;
    }
    extern fn xcb_poll_for_queued_event(c: *Connection) ?*Event.Generic;

    // pub fn sendEvent(self: *Connection) void {
    //     _ = self;
    // }
    // extern fn xcb_send_event(
    //     c: *Connection,
    //     propogate: bool,
    //     destination: Window,
    //     event_mask: EventMask,
    //     event: Event,
    // ) VoidCookie;

    pub fn grabPointer(
        self: *Connection,
        owner_events: bool,
        grab_window: Window,
        event_mask: EventMask,
        pointer_mode: GrabMode,
        keyboard_mode: GrabMode,
        confine_to: Window,
        cursor: Cursor,
        time: Timestamp,
    ) !void {
        const cookie = xcb_grab_pointer(
            self,
            owner_events,
            grab_window,
            event_mask,
            pointer_mode,
            keyboard_mode,
            confine_to,
            cursor,
            time,
        );

        // var err: *GenericError = undefined;
        const reply = xcb_grab_pointer_reply(self, cookie, null);
        defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        return switch (reply.status) {
            .already_grabbed => error.XcbAlreadyGrabbed,
            .frozen => error.XcbFrozen,
            .invalid_time => error.XcbInvalidTime,
            .not_viewable => error.XcbNotViewable,
            .success => {},
        };
    }
    extern fn xcb_grab_pointer(c: *Connection, owner_events: bool, grab_window: Window, event_mask: EventMask, pointer_mode: GrabMode, keyboard_mode: GrabMode, confine_to: Window, cursor: Cursor, time: Timestamp) GrabCookie;
    extern fn xcb_grab_pointer_reply(c: *Connection, cookie: GrabCookie, e: ?**GenericError) *GrabReply;

    pub fn ungrabPointer(self: *Connection, time: Timestamp) void {
        _ = xcb_ungrab_pointer(self, time);
    }
    extern fn xcb_ungrab_pointer(c: *Connection, time: Timestamp) VoidCookie;

    // TODO: ungrab fn
    // TODO: rearrange arguments
    pub fn grabButton(
        self: *Connection,
        button: MouseButton,
        modifiers: ModMask,
        owner_events: bool,
        grab_window: Window,
        event_mask: EventMask,
        confine_to: Window,
        cursor: Cursor,
        pointer_mode: GrabMode,
        keyboard_mode: GrabMode,
    ) !void {
        const cookie = xcb_grab_button_checked(
            self,
            owner_events,
            grab_window,
            event_mask,
            pointer_mode,
            keyboard_mode,
            confine_to,
            cursor,
            button,
            modifiers,
        );
        try self.requestCheck(cookie);
    }
    extern fn xcb_grab_button_checked(c: *Connection, owner_events: bool, grab_window: Window, event_mask: EventMask, pointer_mode: GrabMode, keyboard_mode: GrabMode, confine_to: Window, cursor: Cursor, button: MouseButton, modifiers: ModMask) VoidCookie;

    pub fn ungrabButton(
        self: *Connection,
        button: MouseButton,
        modifiers: ModMask,
        grab_window: Window,
    ) void {
        _ = xcb_ungrab_button(self, button, grab_window, modifiers);
    }
    extern fn xcb_ungrab_button(c: *Connection, button: MouseButton, grab_window: Window, modifiers: ModMask) VoidCookie;

    pub fn grabKey(
        self: *Connection,
        key: Keycode,
        modifiers: ModMask,
        owner_events: bool,
        grab_window: Window,
        pointer_mode: GrabMode,
        keyboard_mode: GrabMode,
    ) !void {
        const cookie = xcb_grab_key_checked(
            self,
            owner_events,
            grab_window,
            modifiers,
            key,
            pointer_mode,
            keyboard_mode,
        );
        try self.requestCheck(cookie);
    }
    extern fn xcb_grab_key_checked(c: *Connection, owner_events: bool, grab_window: Window, modifiers: ModMask, key: Keycode, pointer_mode: GrabMode, keyboard_mode: GrabMode) VoidCookie;

    pub fn ungrabKey(
        self: *Connection,
        key: Keycode,
        modifiers: ModMask,
        grab_window: Window,
    ) void {
        _ = xcb_ungrab_key(self, key, grab_window, modifiers);
    }
    extern fn xcb_ungrab_key(c: *Connection, key: Keycode, grab_window: Window, modifiers: ModMask) VoidCookie;

    // TODO: ungrab fn
    pub fn grabKeyboard(
        self: *Connection,
        owner_events: bool,
        grab_window: Window,
        pointer_mode: GrabMode,
        keyboard_mode: GrabMode,
        time: Timestamp,
    ) !void {
        const cookie = xcb_grab_keyboard(
            self,
            owner_events,
            grab_window,
            time,
            pointer_mode,
            keyboard_mode,
        );

        // var err: *GenericError = undefined;
        const reply = xcb_grab_keyboard_reply(self, cookie, null);
        defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        return switch (reply.status) {
            .already_grabbed => error.XcbAlreadyGrabbed,
            .frozen => error.XcbFrozen,
            .invalid_time => error.XcbInvalidTime,
            .not_viewable => error.XcbNotViewable,
            .success => {},
        };
    }
    extern fn xcb_grab_keyboard(c: *Connection, owner_events: bool, grab_window: Window, time: Timestamp, pointer_mode: GrabMode, keyboard_mode: GrabMode) GrabCookie;
    extern fn xcb_grab_keyboard_reply(c: *Connection, cookie: GrabCookie, e: ?**GenericError) *GrabReply;

    pub fn ungrabKeyboard(self: *Connection, time: Timestamp) void {
        _ = xcb_ungrab_keyboard(self, time);
    }
    extern fn xcb_ungrab_keyboard(c: *Connection, time: Timestamp) VoidCookie;

    pub fn allowEvents(self: *Connection, mode: AllowMode, time: Timestamp) void {
        _ = xcb_allow_events(self, mode, time);
    }
    extern fn xcb_allow_events(c: *Connection, mode: AllowMode, time: Timestamp) VoidCookie;

    pub fn fakeInput(
        self: *Connection,
        event_type: EventType,
        detail: Keycode,
        root: Window,
        root_x: i16,
        root_y: i16,
        device_id: u8,
        time: Timestamp,
    ) !void {
        const cookie = xcb_test_fake_input_checked(
            self,
            event_type,
            detail,
            time,
            root,
            root_x,
            root_y,
            device_id,
        );
        try self.requestCheck(cookie);
    }
    extern fn xcb_test_fake_input_checked(c: *Connection, @"type": EventType, detail: Keycode, time: Timestamp, root: Window, rootX: i16, rootY: i16, deviceid: u8) VoidCookie;

    pub fn requestCheck(self: *Connection, cookie: VoidCookie) !void {
        const err = xcb_request_check(self, cookie) orelse return;
        try err.error_code.check();
    }
    extern fn xcb_request_check(c: *Connection, cookie: VoidCookie) ?*GenericError;

    pub fn internAtom(
        self: *Connection,
        only_if_exists: bool,
        name: []const u8,
    ) Atom {
        const cookie = xcb_intern_atom(self, only_if_exists, @truncate(name.len), name.ptr);

        // var err: *GenericError = undefined;
        const reply = xcb_intern_atom_reply(self, cookie, null);
        defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        return reply.atom;
    }
    extern fn xcb_intern_atom(c: *Connection, only_if_exists: bool, name_len: u16, name: [*]const u8) InternAtomCookie;
    extern fn xcb_intern_atom_reply(c: *Connection, cookie: InternAtomCookie, e: ?**GenericError) *InternAtomReply;

    pub fn getActiveWindow(self: *Connection, root: Window) !Window {
        const active_window_atom = self.internAtom(true, "_NET_ACTIVE_WINDOW");
        if (active_window_atom == .none) return error.XcbNoActiveWindow;
        // TODO: if the atom exists is the property guaranteed to exist?
        const property_reply = root.getProperty(self, false, active_window_atom, .window, 0, 1);
        const data = property_reply.getValue().?;
        const pointer: *Window = @ptrCast(@alignCast(data));
        return pointer.*;
    }

    // TODO: error not getting filled?
    pub fn getInputFocus(self: *Connection) Window {
        const cookie = xcb_get_input_focus(self);

        // var err: *GenericError = undefined;
        const reply = xcb_get_input_focus_reply(self, cookie, null);
        defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        const result = reply.focus;

        return result;
    }
    extern fn xcb_get_input_focus(c: *Connection) GetInputFocusCookie;
    extern fn xcb_get_input_focus_reply(c: *Connection, cookie: GetInputFocusCookie, e: ?**GenericError) *GetInputFocusReply;

    // TODO: test this to make sure it's accurate
    pub fn getTopLevelParent(self: *Connection, window: Window) Window {
        var result = window;

        var tree = self.queryTree(result);
        while (tree.parent != tree.root and result != tree.root) {
            result = tree.parent;
            tree.destroy();
            tree = self.queryTree(result);
        }
        tree.destroy();

        return result;
    }

    // TODO: is freeing it safe or are children owned by the tree?
    // TODO: error doesn't seem to get filled?
    pub fn queryTree(self: *Connection, window: Window) *QueryTreeReply {
        const cookie = xcb_query_tree(self, window);

        // var err: *GenericError = undefined;
        const reply = xcb_query_tree_reply(self, cookie, null);
        // defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        return reply;
        // return reply.*;
    }
    extern fn xcb_query_tree(c: *Connection, window: Window) QueryTreeCookie;
    extern fn xcb_query_tree_reply(c: *Connection, cookie: QueryTreeCookie, e: ?**GenericError) *QueryTreeReply;

    pub fn getFileDescriptor(self: *Connection) std.os.fd_t {
        return xcb_get_file_descriptor(self);
    }
    extern fn xcb_get_file_descriptor(c: *Connection) std.os.fd_t;
};

pub const KeySymbols = opaque {
    pub fn alloc(connection: *Connection) !*KeySymbols {
        return xcb_key_symbols_alloc(connection) orelse {
            try connection.checkError();
            return error.XcbKeySymbolAllocationError;
        };
    }
    extern fn xcb_key_symbols_alloc(c: *Connection) ?*KeySymbols;

    pub fn getKeysym(self: *KeySymbols, keycode: Keycode, column: c_int) xzb.Keysym {
        return xcb_key_symbols_get_keysym(self, keycode, column);
    }
    extern fn xcb_key_symbols_get_keysym(syms: *KeySymbols, keycode: Keycode, col: c_int) xzb.Keysym;

    // TODO: how should I make managing this memory more ergonomic?
    // TODO: handle null value https://cgit.freedesktop.org/xcb/util-keysyms/tree/keysyms/keysyms.c
    // pub fn getKeycode(self: *KeySymbols, keysym: xzb.Keysym) []Keycode {
    //     const keycodes = xcb_key_symbols_get_keycode(self, keysym) orelse {
    //         std.debug.print("KEYCODE NULL\n", .{});
    //         return &.{};
    //     };
    //     return std.mem.sliceTo(keycodes, @enumFromInt(Keycode, 0));
    // }
    // extern fn xcb_key_symbols_get_keycode(syms: *KeySymbols, keysym: xzb.Keysym) ?[*:@enumFromInt(Keycode, 0)]Keycode;

    const keysyms_per_keycode = 4;
    pub fn getKeycode(self: *KeySymbols, connection: *Connection, keysym: xzb.Keysym) ![]Keycode {
        var result: [keysyms_per_keycode]Keycode = undefined;
        var result_len: usize = 0;
        const setup = try connection.getSetup();

        {
            const min_keycode: u8 = @intFromEnum(setup.min_keycode);
            const max_keycode: u8 = @intFromEnum(setup.max_keycode);
            var keycode: u8 = min_keycode;
            while (keycode < max_keycode) : (keycode += 1) {
                {
                    var column: u8 = 0;
                    while (column < keysyms_per_keycode) : (column += 1) {
                        const ks = self.getKeysym(@enumFromInt(keycode), column);
                        if (ks == keysym) {
                            result[result_len] = @enumFromInt(keycode);
                            result_len += 1;
                        }
                    }
                }
            }
        }

        return result[0..result_len];
    }

    pub fn refreshMapping(self: *KeySymbols, event: Event.MappingNotify) bool {
        return if (xcb_refresh_keyboard_mapping(self, &event) == 1) true else false;
    }
    extern fn xcb_refresh_keyboard_mapping(syms: *KeySymbols, event: *Event.MappingNotify) c_int;

    pub fn free(self: *KeySymbols) void {
        xcb_key_symbols_free(self);
    }
    extern fn xcb_key_symbols_free(syms: *KeySymbols) void;
};

pub const Colormap = enum(u32) {
    _,

    pub fn create(
        connection: *Connection,
        alloc: u8,
        mid: Colormap,
        window: Window,
        visual: VisualId,
    ) !Pixmap {
        // const colormap = Colormap{ .id = try connection.generateId(Colormap) };
        const colormap = try connection.generateId(Colormap);
        _ = xcb_create_colormap(connection, alloc, mid, window, visual);
        return colormap;
    }
    extern fn xcb_create_colormap(
        c: *Connection,
        alloc: u8,
        mid: Colormap,
        window: Window,
        visual: VisualId,
    ) VoidCookie;

    pub fn destroy(self: Colormap, connection: *Connection) void {
        _ = xcb_free_colormap(connection, self);
    }
    extern fn xcb_free_colormap(c: *Connection, cmap: Colormap) VoidCookie;
};

pub const Font = enum(u32) {
    _,

    pub fn open(
        connection: *Connection,
        name: []const u8,
    ) Font {
        const font = try connection.generateId(Font);
        xcb_open_font(connection, font, @truncate(name.len), name.ptr);
    }
    extern fn xcb_open_font(c: *Connection, fid: Font, name_len: u16, name: [*]const u8) VoidCookie;
};

pub const Cursor = enum(u32) {
    none = 0,
    _,
};

pub const Screen = extern struct {
    root: Window,
    default_colormap: Colormap,
    white_pixel: u32,
    black_pixel: u32,
    current_input_masks: u32,
    width_in_pixels: u16,
    height_in_pixels: u16,
    width_in_millimeters: u16,
    height_in_millimeters: u16,
    min_installed_maps: u16,
    max_installed_maps: u16,
    root_visual: VisualId,
    backing_stores: u8,
    save_unders: u8,
    root_depth: u8,
    allowed_depths_len: u8,
};

pub const ScreenIterator = extern struct {
    data: [*]Screen,
    rem: c_int,
    index: c_int,

    pub fn next(self: *ScreenIterator) ?Screen {
        if (self.rem == 0) return null;
        const result = self.data;
        xcb_screen_next(self);
        return result[0];
    }
    extern fn xcb_screen_next(iterator: *ScreenIterator) void;

    pub fn last(self: *ScreenIterator) *Screen {
        const last_iter = xcb_screen_end(self.*);
        return last_iter.data;
    }
    extern fn xcb_screen_end(iterator: ScreenIterator) ScreenIterator;
};

pub const Setup = extern struct {
    status: u8,
    pad0: u8,
    protocol_major_version: u16,
    protocol_minor_version: u16,
    length: u16,
    release_number: u32,
    resource_id_base: u32,
    resource_id_mask: u32,
    motion_buffer_size: u32,
    vendor_len: u16,
    maximum_request_length: u16,
    roots_len: u8,
    pixmap_formats_len: u8,
    image_byte_order: u8,
    bitmap_format_bit_order: u8,
    bitmap_format_scanline_unit: u8,
    bitmap_format_scanline_pad: u8,
    min_keycode: Keycode,
    max_keycode: Keycode,
    pad1: [4]u8,

    pub const rootsIterator = xcb_setup_roots_iterator;
    extern fn xcb_setup_roots_iterator(setup: *const Setup) ScreenIterator;
};

pub const Atom = enum(u32) {
    none = 0,
    primary = 1,
    secondary = 2,
    arc = 3,
    atom = 4,
    bitmap = 5,
    cardinal = 6,
    colormap = 7,
    cursor = 8,
    cut_buffer0 = 9,
    cut_buffer1 = 10,
    cut_buffer2 = 11,
    cut_buffer3 = 12,
    cut_buffer4 = 13,
    cut_buffer5 = 14,
    cut_buffer6 = 15,
    cut_buffer7 = 16,
    drawable = 17,
    font = 18,
    integer = 19,
    pixmap = 20,
    point = 21,
    rectangle = 22,
    resource_manager = 23,
    rgb_color_map = 24,
    rgb_best_map = 25,
    rgb_blue_map = 26,
    rgb_default_map = 27,
    rgb_gray_map = 28,
    rgb_green_map = 29,
    rgb_red_map = 30,
    string = 31,
    visualid = 32,
    window = 33,
    wm_command = 34,
    wm_hints = 35,
    wm_client_machine = 36,
    wm_icon_name = 37,
    wm_icon_size = 38,
    wm_name = 39,
    wm_normal_hints = 40,
    wm_size_hints = 41,
    wm_zoom_hints = 42,
    min_space = 43,
    norm_space = 44,
    max_space = 45,
    end_space = 46,
    superscript_x = 47,
    superscript_y = 48,
    subscript_x = 49,
    subscript_y = 50,
    underline_position = 51,
    underline_thickness = 52,
    strikeout_ascent = 53,
    strikeout_descent = 54,
    italic_angle = 55,
    x_height = 56,
    quad_width = 57,
    weight = 58,
    point_size = 59,
    resolution = 60,
    copyright = 61,
    notice = 62,
    font_name = 63,
    family_name = 64,
    full_name = 65,
    cap_height = 66,
    wm_class = 67,
    wm_transient_for = 68,
    _,

    pub const any = Atom.none;
};

// TODO: rename to just Visual?
pub const VisualId = enum(u32) {
    copy_from_parent = 0,
    _,

    // pub const copy_from_parent = VisualId{ .id = @enumFromInt(VisualId, 0) };
};

pub const Window = enum(u32) {
    none = 0,
    _,

    // pub const none = Window{ .id = @enumFromInt(Window, 0) };

    pub const Options = struct {
        depth: u8, // TODO: figure out legal depth values
        parent: Window,
        x: i16,
        y: i16,
        width: u16,
        height: u16,
        border_width: u16,
        class: WindowClass,
        visual: VisualId,

        window_name: ?[]const u8 = null,
        class_name: ?[]const u8 = null,
    };

    pub const ValueMask = enum(u32) {
        none = 0,
        background_pixmap = 1,
        background_pixel = 2,
        border_pixmap = 4,
        border_pixel = 8,
        bit_gravity = 16,
        win_gravity = 32,
        backing_store = 64,
        backing_planes = 128,
        backing_pixel = 256,
        override_redirect = 512,
        save_under = 1024,
        event_mask = 2048,
        do_not_propagate = 4096,
        colormap = 8192,
        cursor = 16384,
        _,

        pub fn new(mask: anytype) ValueMask {
            var result: u32 = 0;
            inline for (mask) |value| {
                result |= @intFromEnum(value);
            }
            return @enumFromInt(result);
        }
    };

    // TODO: replace stuff with enums
    pub const ValueTypes = struct {
        pub const background_pixmap: type = Pixmap;
        pub const background_pixel: type = u32;
        pub const border_pixmap: type = Pixmap;
        pub const border_pixel: type = u32;
        pub const bit_gravity: type = u32;
        pub const win_gravity: type = u32;
        pub const backing_store: type = u32;
        pub const backing_planes: type = u32;
        pub const backing_pixel: type = u32;
        pub const override_redirect: type = bool;
        pub const save_under: type = bool;
        pub const event_mask: type = u32;
        pub const do_not_propogate_mask: type = u32;
        pub const colormap: type = Colormap;
        pub const cursor: type = Cursor;
    };

    pub const WindowClass = enum(u16) {
        copy_from_parent,
        input_output,
        input_only,
    };

    fn WindowValueList(comptime T: type) type {
        return [std.meta.fields(T).len]u32;
    }

    // TODO: remove the window_name and class_name stuff
    pub fn create(
        connection: *Connection,
        options: Options,
        values: anytype,
    ) !Window {
        const ValuesT = @TypeOf(values);
        comptime var mask = ValueMask.none;
        comptime var i = 0;
        var value_list: WindowValueList(ValuesT) = undefined;

        comptime for (std.meta.fields(ValuesT)) |field| {
            if (!@hasField(ValueMask, field.name))
                @compileError("Unexpected value '" ++ field.name ++ "' in window values!");
        };

        inline for (std.meta.fields(ValueMask)) |field| {
            if (@hasField(ValuesT, field.name)) {
                const field_value = @as(@field(ValueTypes, field.name), @field(values, field.name));
                const FieldT = @TypeOf(field_value);
                mask = comptime ValueMask.new(.{ mask, @field(ValueMask, field.name) });
                value_list[i] = if (FieldT == bool)
                    @intFromBool(field_value)
                else
                    @bitCast(field_value);
                i += 1;
            }
        }

        const window = try connection.generateId(Window);
        _ = xcb_create_window(
            connection,
            options.depth,
            window,
            options.parent,
            options.x,
            options.y,
            options.width,
            options.height,
            options.border_width,
            options.class,
            options.visual,
            mask,
            &value_list,
        );

        if (options.window_name) |name| {
            window.changeProperty(
                connection,
                .replace,
                .wm_name,
                .string,
                .u8,
                name,
            );
        }

        if (options.class_name) |class| {
            window.changeProperty(
                connection,
                .replace,
                .wm_class,
                .string,
                .u8,
                class,
            );
        }

        return window;
    }
    extern fn xcb_create_window(c: *Connection, depth: u8, wid: Window, parent: Window, x: i16, y: i16, width: u16, height: u16, border_width: u16, _class: WindowClass, visual: VisualId, value_mask: ValueMask, value_list: [*]const u32) VoidCookie;

    // TODO: figure out how I can make the return type more useful; I should
    //       be able to assign return types based on property_type.
    //       Also think about data_offset/data_length
    pub fn getProperty(
        self: Window,
        connection: *Connection,
        delete: bool,
        property: Atom,
        property_type: Atom,
        data_offset: u32,
        data_length: u32,
    ) !*GetPropertyReply {
        const cookie = xcb_get_property(
            connection,
            delete,
            self,
            property,
            property_type,
            data_offset,
            data_length,
        );

        // var err: *GenericError = undefined;
        const reply = xcb_get_property_reply(connection, cookie, null);
        // xzb.destroy(reply);
        // defer xzb.destroy(err);

        // try err.error_code.check();

        return reply;
        // return reply.getValue();
    }
    extern fn xcb_get_property(c: *Connection, _delete: bool, window: Window, property: Atom, @"type": Atom, long_offset: u32, long_length: u32) GetPropertyCookie;
    extern fn xcb_get_property_reply(c: *Connection, cookie: GetPropertyCookie, e: ?**GenericError) *GetPropertyReply;

    // TODO: I may be able to determine data type from property_type and forego format entirely
    pub fn changeProperty(
        self: Window,
        connection: *Connection,
        mode: PropertyMode,
        property: Atom,
        property_type: Atom,
        // TODO: is it reasonable to assume format will always be known at comptime?
        //       that would make the data type more informative
        format: PropertyFormat,
        data: []const u8, // is this okay? research more about what kind of data properties might take
    ) void {
        _ = xcb_change_property(
            connection,
            mode,
            self,
            property,
            property_type,
            format,
            @truncate(data.len),
            data.ptr,
        );
    }
    const PropertyMode = enum(u8) { replace, prepend, append };
    const PropertyFormat = enum(u8) { u8 = 8, u16 = 16, u32 = 32 };
    extern fn xcb_change_property(connection: *Connection, mode: PropertyMode, window: Window, property: Atom, property_type: Atom, format: PropertyFormat, data_len: u32, data: [*]const u8) VoidCookie;

    pub fn map(self: Window, connection: *Connection) void {
        _ = xcb_map_window(connection, self);
    }
    extern fn xcb_map_window(connection: *Connection, window: Window) VoidCookie;

    pub fn getGeometry(self: Window, connection: *Connection) !Geometry {
        return self.toDrawable().getGeometry(connection);
    }

    pub fn toDrawable(self: Window) Drawable {
        return Drawable.new(self);
    }

    pub fn destroy(self: Window, connection: *Connection) void {
        connection.destroy(Window, self);
    }
};

pub const Pixmap = enum(u32) {
    _,

    pub fn create(
        connection: *Connection,
        depth: u8,
        drawable: Drawable,
        width: u16,
        height: u16,
    ) !Pixmap {
        const pixmap = try connection.generateId(Pixmap);
        _ = xcb_create_pixmap(connection, depth, pixmap, drawable, width, height);
        return pixmap;
    }
    extern fn xcb_create_pixmap(c: *Connection, depth: u8, pid: Pixmap, drawable: Drawable, width: u16, height: u16) VoidCookie;

    pub fn shapeMask(
        self: Pixmap,
        connection: *Connection,
        destination: Window,
        operation: ShapeOperation,
        kind: ShapeKind,
        x_offset: i16,
        y_offset: i16,
    ) void {
        _ = xcb_shape_mask(connection, operation, kind, destination, x_offset, y_offset, self);
    }
    extern fn xcb_shape_mask(c: *Connection, operation: ShapeOperation, destination_kind: ShapeKind, destination_window: Window, x_offset: i16, y_offset: i16, source_bitmap: Pixmap) VoidCookie;

    pub fn getGeometry(self: Pixmap, connection: *Connection) !Geometry {
        return self.toDrawable().getGeometry(connection);
    }

    pub fn toDrawable(self: Pixmap) Drawable {
        return Drawable.new(self);
    }

    pub fn destroy(self: Pixmap, connection: *Connection) void {
        connection.destroy(Pixmap, self);
    }
};

pub const Geometry = struct {
    x: i16,
    y: i16,
    width: u16,
    height: u16,

    border_width: u16,
};

pub const Drawable = extern union {
    window: Window,
    pixmap: Pixmap,

    pub fn new(from: anytype) Drawable {
        return switch (@TypeOf(from)) {
            Window => Drawable{ .window = from },
            Pixmap => Drawable{ .pixmap = from },
            else => @compileError("Drawable type must be Window or Pixmap!"),
        };
    }

    // TODO: error doesn't seem to be filled? why is that happening
    pub fn getGeometry(self: Drawable, connection: *Connection) Geometry {
        const cookie = xcb_get_geometry(connection, self);

        // var err: *GenericError = undefined;
        const reply = xcb_get_geometry_reply(connection, cookie, null);
        defer xzb.destroy(reply);
        // defer xzb.destroy(err);

        // std.debug.print("{any}\n", .{@bitCast(u32, self)});
        // try err.error_code.check();

        return Geometry{
            .x = reply.x,
            .y = reply.y,
            .width = reply.width,
            .height = reply.height,
            .border_width = reply.border_width,
        };
    }
    extern fn xcb_get_geometry(c: *Connection, drawable: Drawable) GeometryCookie;
    extern fn xcb_get_geometry_reply(c: *Connection, cookie: GeometryCookie, e: ?**GenericError) *GeometryReply;
};

pub const GeometryReply = extern struct {
    response_type: u8,
    depth: u8,
    sequence: u16,
    length: u32,
    root: Window,
    x: i16,
    y: i16,
    width: u16,
    height: u16,
    border_width: u16,
    pad0: [2]u8,
};

// TODO: wrapper type that holds it's drawable? probably not, as a single GC can be reused for multiple drawables
pub const GraphicsContext = enum(u32) {
    _,

    pub const ValueMask = enum(u32) {
        none = 0,
        function = 1,
        plane_mask = 2,
        foreground = 4,
        background = 8,
        line_width = 16,
        line_style = 32,
        cap_style = 64,
        join_style = 128,
        fill_style = 256,
        fill_rule = 512,
        tile = 1024,
        stipple = 2048,
        tile_stipple_origin_x = 4096,
        tile_stipple_origin_y = 8192,
        font = 16384,
        subwindow_mode = 32768,
        graphics_exposures = 65536,
        clip_origin_x = 131072,
        clip_origin_y = 262144,
        clip_mask = 524288,
        dash_offset = 1048576,
        dash_list = 2097152,
        arc_mode = 4194304,
        _,

        pub fn new(mask: anytype) ValueMask {
            var result: u32 = 0;
            inline for (mask) |value| {
                result |= @intFromEnum(@as(ValueMask, value));
            }
            return @enumFromInt(result);
        }
    };

    // TODO: enums
    pub const ValueTypes = struct {
        pub const function: type = u32;
        pub const plane_mask: type = u32; // TODO: enum???
        pub const foreground: type = u32;
        pub const background: type = u32;
        pub const line_width: type = u32;
        pub const line_style: type = u32; // TODO: enum
        pub const cap_style: type = u32; // TODO: enum
        pub const join_style: type = u32; // TODO: enum
        pub const fill_style: type = u32; // TODO: enum
        pub const fill_rule: type = u32; // TODO: enum
        pub const tile: type = Pixmap;
        pub const stipple: type = Pixmap;
        pub const tile_stipple_x_origin: type = u32;
        pub const tile_stipple_y_origin: type = u32;
        pub const font: type = Font;
        pub const subwindow_mode: type = u32; // TODO: enum
        pub const graphics_exposures: type = bool;
        pub const clip_x_origin: type = u32;
        pub const clip_y_origin: type = u32;
        pub const clip_mask: type = Pixmap;
        pub const dash_offset: type = u32;
        pub const dashes: type = u32; // TODO: ????
        pub const arc_mode: type = u32; // TODO: enum
    };

    fn GraphicsContextValueList(comptime T: type) type {
        return [std.meta.fields(T).len]u32;
    }

    pub fn create(
        connection: *Connection,
        drawable: Drawable,
        values: anytype,
    ) !GraphicsContext {
        const ValuesT = @TypeOf(values);
        comptime var mask = ValueMask.none;
        comptime var i = 0;
        var value_list: GraphicsContextValueList(ValuesT) = undefined;

        comptime for (std.meta.fields(ValuesT)) |field| {
            if (!@hasField(ValueMask, field.name))
                @compileError("Unexpected value '" ++ field.name ++ "' in window values!");
        };

        inline for (std.meta.fields(ValueMask)) |field| {
            if (@hasField(ValuesT, field.name)) {
                const field_value = @as(@field(ValueTypes, field.name), @field(values, field.name));
                const FieldT = @TypeOf(field_value);
                mask = comptime ValueMask.new(.{ mask, @field(ValueMask, field.name) });
                value_list[i] = if (FieldT == bool)
                    @intFromBool(field_value)
                else
                    @bitCast(field_value);
                i += 1;
            }
        }
        const gc = try connection.generateId(GraphicsContext);
        _ = xcb_create_gc(connection, gc, drawable, mask, &value_list);
        return gc;
    }
    extern fn xcb_create_gc(c: *Connection, cid: GraphicsContext, drawable: Drawable, value_mask: ValueMask, value_list: [*]const u32) VoidCookie;

    pub fn change(self: GraphicsContext, connection: *Connection, values: anytype) void {
        const ValuesT = @TypeOf(values);
        comptime var mask = ValueMask.none;
        comptime var i = 0;
        var value_list: GraphicsContextValueList(ValuesT) = undefined;

        comptime for (std.meta.fields(ValuesT)) |field| {
            if (!@hasField(ValueMask, field.name))
                @compileError("Unexpected value '" ++ field.name ++ "' in window values!");
        };

        inline for (std.meta.fields(ValueMask)) |field| {
            if (@hasField(ValuesT, field.name)) {
                const field_value = @as(@field(ValueTypes, field.name), @field(values, field.name));
                const FieldT = @TypeOf(field_value);
                mask = comptime ValueMask.new(.{ mask, @field(ValueMask, field.name) });
                value_list[i] = if (FieldT == bool)
                    @intFromBool(field_value)
                else
                    @bitCast(field_value);
                i += 1;
            }
        }

        _ = xcb_change_gc(connection, self, mask, &value_list);
    }
    extern fn xcb_change_gc(c: *Connection, gc: GraphicsContext, mask: ValueMask, values: [*]const u32) VoidCookie;

    const DrawOptions = struct { coordinate_mode: CoordinateMode = .origin };
    const CoordinateMode = enum(u8) { origin = 0, previous = 1 };

    pub fn polyLine(
        self: GraphicsContext,
        connection: *Connection,
        drawable: Drawable,
        points: []Point,
        options: DrawOptions,
    ) void {
        _ = xcb_poly_line(
            connection,
            options.coordinate_mode,
            drawable,
            self,
            @truncate(points.len),
            points.ptr,
        );
    }
    extern fn xcb_poly_line(c: *Connection, coordinate_mode: CoordinateMode, drawable: Drawable, gc: GraphicsContext, points_len: u32, points: [*]const Point) VoidCookie;

    pub fn polyFillRectangle(
        self: GraphicsContext,
        connection: *Connection,
        drawable: Drawable,
        rectangles: []Rectangle,
    ) void {
        _ = xcb_poly_fill_rectangle(
            connection,
            drawable,
            self,
            @truncate(rectangles.len),
            rectangles.ptr,
        );
    }
    extern fn xcb_poly_fill_rectangle(c: *Connection, drawable: Drawable, gc: GraphicsContext, rectangles_len: u32, rectangles: [*]const Rectangle) VoidCookie;

    pub fn copyArea(
        self: GraphicsContext,
        connection: *Connection,
        source: Drawable,
        source_x: i16,
        source_y: i16,
        destination: Drawable,
        destination_x: i16,
        destination_y: i16,
        width: u16,
        height: u16,
    ) void {
        _ = xcb_copy_area(
            connection,
            source,
            destination,
            self,
            source_x,
            source_y,
            destination_x,
            destination_y,
            width,
            height,
        );
    }
    extern fn xcb_copy_area(c: *Connection, src_drawable: Drawable, dst_drawable: Drawable, gc: GraphicsContext, src_x: i16, src_y: i16, dst_x: i16, dst_y: i16, width: u16, height: u16) VoidCookie;

    pub fn destroy(self: GraphicsContext, connection: *Connection) void {
        connection.destroy(GraphicsContext, self);
    }
};

// TODO: figure out how to use this
pub const GenericError = extern struct {
    response_type: u8,
    error_code: GenericErrorCode,
    sequence: u16,
    resource_id: u32,
    minor_code: u16,
    major_code: u8,
    pad0: u8,
    pad: [5]u32,
    full_sequence: u32,
};

pub const GenericErrorCode = enum(u8) {
    none = 0,
    request = 1,
    value = 2,
    window = 3,
    pixmap = 4,
    atom = 5,
    cursor = 6,
    font = 7,
    match = 8,
    drawable = 9,
    access = 10,
    alloc = 11,
    colormap = 12,
    graphics_context = 13,
    id_choice = 14,
    name = 15,
    length = 16,
    implementation = 17,

    pub fn check(self: GenericErrorCode) !void {
        return switch (self) {
            .none => {},
            .request => error.XcbRequestError,
            .value => error.XcbValueError,
            .window => error.XcbWindowError,
            .pixmap => error.XcbPixmapError,
            .atom => error.XcbAtomError,
            .cursor => error.XcbCursorError,
            .font => error.XcbFontError,
            .match => error.XcbMatchError,
            .drawable => error.XcbDrawableError,
            .access => error.XcbAccessError,
            .alloc => error.XcbAllocError,
            .colormap => error.XcbColormapError,
            .graphics_context => error.XcbGraphicsContextError,
            .id_choice => error.XcbIdChoiceError,
            .name => error.XcbNameError,
            .length => error.XcbLengthError,
            .implementation => error.XcbImplementationError,
        };
    }
};

pub fn Cookie(comptime name: @TypeOf(.enum_literal)) type {
    _ = name;
    return extern struct {
        sequence: c_uint,
    };
}

pub const VoidCookie = Cookie(.void);
pub const GrabCookie = Cookie(.grab);
pub const InternAtomCookie = Cookie(.intern_atom);
pub const GetPropertyCookie = Cookie(.get_property);
pub const GetInputFocusCookie = Cookie(.get_input_focus);
pub const QueryTreeCookie = Cookie(.query_tree);
pub const GeometryCookie = Cookie(.geometry);

pub const GrabMode = enum(u8) {
    sync = 0,
    @"async" = 1,
};

pub const GrabStatus = enum(u8) {
    success = 0,
    already_grabbed = 1,
    invalid_time = 2,
    not_viewable = 3,
    frozen = 4,
};

// TODO: enum types
pub const GrabReply = extern struct {
    response_type: u8,
    status: GrabStatus,
    sequence: u16,
    length: u32,
};

pub const InternAtomReply = extern struct {
    response_type: u8,
    pad0: u8,
    sequence: u16,
    length: u32,
    atom: Atom,
};

pub const GetPropertyReply = extern struct {
    response_type: u8,
    format: u8,
    sequence: u16,
    length: u32,
    type: Atom,
    bytes_after: u32,
    value_len: u32,
    pad0: [12]u8,

    pub fn getValue(self: *const GetPropertyReply) ?*anyopaque {
        return xcb_get_property_value(self);
    }
    extern fn xcb_get_property_value(*const GetPropertyReply) ?*anyopaque;
};

pub const GetInputFocusReply = extern struct {
    response_type: u8,
    revert_to: u8, // TODO: what is?
    sequence: u16,
    length: u32,
    focus: Window,
};

pub const QueryTreeReply = extern struct {
    response_type: u8,
    pad0: u8,
    sequence: u16,
    length: u32,
    root: Window,
    parent: Window,
    children_len: u16,
    pad1: [14]u8,

    pub fn children(self: *const QueryTreeReply) []Window {
        return xcb_query_tree_children(self)[0..self.children_len];
    }
    extern fn xcb_query_tree_children(*const QueryTreeReply) [*]Window;

    pub fn destroy(self: *QueryTreeReply) void {
        xzb.destroy(self);
    }
};

pub const AllowMode = enum(u8) {
    async_pointer = 0,
    sync_pointer = 1,
    replay_pointer = 2,
    async_keyboard = 3,
    sync_keyboard = 4,
    replay_keyboard = 5,
    async_both = 6,
    sync_both = 7,
};

pub const ShapeOperation = enum(u8) {
    set = 0,
    // TODO: consider renaming?
    @"union" = 1,
    intersect = 2,
    subtract = 3,
    invert = 4,
};

pub const ShapeKind = enum(u8) {
    bounding = 0,
    clip = 1,
    input = 2,
};

pub const Point = extern struct {
    x: i16,
    y: i16,

    pub inline fn new(x: i16, y: i16) Point {
        return Point{ .x = x, .y = y };
    }
};

pub const Rectangle = extern struct {
    x: i16,
    y: i16,
    width: u16,
    height: u16,

    pub inline fn new(x: i16, y: i16, width: u16, height: u16) Rectangle {
        return Rectangle{ .x = x, .y = y, .width = width, .height = height };
    }
};

// TODO: add mouse buttons here too?????
// TODO: fill in combinations for convenience, up through alt at least
pub const ModMask = enum(u16) {
    none = 0,

    shift = 1,

    lock = 2,
    lock_shift = 3,

    control = 4,
    control_shift = 5,
    control_lock = 6,
    control_lock_shift = 7,

    alt = 8,
    alt_shift = 9,
    alt_lock = 10,
    alt_lock_shift = 11,
    alt_control = 12,
    alt_control_shift = 13,
    alt_control_lock = 14,
    alt_control_lock_shift = 15,

    mod_2 = 16,

    mod_3 = 32,

    super = 64,
    super_shift = 65,
    super_control = 68,
    super_control_shift = 69,
    super_alt = 72,
    super_alt_control = 76,
    super_alt_control_shift = 77,

    mod_5 = 128,

    button_left = 256,
    button_middle = 512,
    button_right = 1024,
    // TODO: are these two correct? if so how does that work????
    button_scroll_up = 2048,
    button_scroll_down = 4096,

    any = 32768,
    _,

    pub fn new(mask_values: anytype) ModMask {
        var mask: u32 = 0;
        inline for (mask_values) |value| {
            mask |= @intFromEnum(@as(ModMask, value));
        }
        return @enumFromInt(mask);
    }

    pub fn hasAny(self: ModMask, mask_values: anytype) bool {
        const mask = @intFromEnum(ModMask.new(mask_values));
        return mask & @intFromEnum(self) > 0;
    }

    pub fn hasAll(self: ModMask, mask_values: anytype) bool {
        const mask = @intFromEnum(ModMask.new(mask_values));
        return mask & @intFromEnum(self) == mask;
    }

    pub fn onlyKeys(self: ModMask) ModMask {
        const button_mask = ModMask.new(.{
            .button_left,
            .button_middle,
            .button_right,
            .button_scroll_up,
            .button_scroll_down,
        });
        return @enumFromInt(@intFromEnum(self) & ~@intFromEnum(button_mask));
    }

    pub fn onlyButtons(self: ModMask) ModMask {
        const key_mask = ModMask.new(.{
            .shift,   .lock,
            .control, .alt,
            .mod_2,   .mod_3,
            .super,   .mod_5,
        });
        return @enumFromInt(@intFromEnum(self) & ~@intFromEnum(key_mask));
    }
};

// TODO: event type of 0 is error, figure out how exactly that looks in code
pub const EventType = enum(u8) {
    key_press = 2,
    key_release = 3,
    button_press = 4,
    button_release = 5,
    motion_notify = 6,
    enter_notify = 7,
    leave_notify = 8,
    focus_in = 9,
    focus_out = 10,
    keymap_notify = 11,
    expose = 12,
    graphics_exposure = 13,
    no_exposure = 14,
    visibility_notify = 15,
    create_notify = 16,
    destroy_notify = 17,
    unmap_notify = 18,
    map_notify = 19,
    map_request = 20,
    reparent_notify = 21,
    configure_notify = 22,
    configure_request = 23,
    gravity_notify = 24,
    resize_request = 25,
    circulate_notify = 26,
    circulate_request = 27,
    property_notify = 28,
    selection_clear = 29,
    selection_request = 30,
    selection_notify = 31,
    colormap_notify = 32,
    client_message = 33,
    mapping_notify = 34,
    ge_generic = 35,
};

pub const EventMask = enum(u32) {
    no_event = 0,
    key_press = 1,
    key_release = 2,
    button_press = 4,
    button_release = 8,
    enter_window = 16,
    leave_window = 32,
    pointer_motion = 64,
    pointer_motion_hint = 128,
    button_1_motion = 256,
    button_2_motion = 512,
    button_3_motion = 1024,
    button_4_motion = 2048,
    button_5_motion = 4096,
    button_motion = 8192,
    keymap_state = 16384,
    exposure = 32768,
    visibility_change = 65536,
    structure_notify = 131072,
    resize_redirect = 262144,
    substructure_notify = 524288,
    substructure_redirect = 1048576,
    focus_change = 2097152,
    property_change = 4194304,
    color_map_change = 8388608,
    owner_grab_button = 16777216,
    _,

    pub fn new(mask: anytype) EventMask {
        var result: u32 = 0;
        inline for (mask) |value| {
            result |= @intFromEnum(@as(EventMask, value));
        }
        return @enumFromInt(result);
    }
};

// TODO: maybe remove `response_type`? we'll see
// TODO: REFERENCE: https://xcb.freedesktop.org/tutorial/events/
// TODO: maybe make some event names more clear, e.g. `button_press` => `mouse_button_press`
pub const Event = union(EventType) {
    key_press: KeyPress,
    key_release: KeyRelease,
    button_press: ButtonPress,
    button_release: ButtonRelease,
    motion_notify: MotionNotify,
    enter_notify: EnterNotify,
    leave_notify: LeaveNotify,
    focus_in: FocusIn,
    focus_out: FocusOut,
    keymap_notify: KeymapNotify,
    expose: Expose,
    graphics_exposure: GraphicsExposure,
    no_exposure: NoExposure,
    visibility_notify: VisibilityNotify,
    create_notify: CreateNotify,
    destroy_notify: DestroyNotify,
    unmap_notify: UnmapNotify,
    map_notify: MapNotify,
    map_request: MapRequest,
    reparent_notify: ReparentNotify,
    configure_notify: ConfigureNotify,
    configure_request: ConfigureRequest,
    gravity_notify: GravityNotify,
    resize_request: ResizeRequest,
    circulate_notify: CirculateNotify,
    circulate_request: CirculateRequest,
    property_notify: PropertyNotify,
    selection_clear: SelectionClear,
    selection_request: SelectionRequest,
    selection_notify: SelectionNotify,
    colormap_notify: ColormapNotify,
    client_message: ClientMessage,
    mapping_notify: MappingNotify,
    ge_generic: GeGeneric,

    // pub fn free(self: Event) void {
    //     const address = switch (self) {
    //         inline else => |event| @ptrToInt(event) - 1,
    //     };
    //     xzb.destroy(@intToPtr(*Generic, address));
    // }

    pub const Generic = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        pad: [7]u32,
        full_sequence: u32,
    };

    pub const KeyPress = extern struct {
        response_type: u8,
        detail: Keycode,
        sequence: u16,
        time: Timestamp,
        root: Window,
        event: Window,
        child: Window,
        root_x: i16,
        root_y: i16,
        event_x: i16,
        event_y: i16,
        state: ModMask,
        same_screen: bool,
        pad0: u8,
    };
    pub const KeyRelease = KeyPress;

    pub const ButtonEvent = extern struct {
        response_type: u8,
        detail: MouseButton,
        sequence: u16,
        time: Timestamp,
        root: Window,
        event: Window,
        child: Window,
        root_x: i16,
        root_y: i16,
        event_x: i16,
        event_y: i16,
        state: ModMask,
        same_screen: bool,
    };
    pub const ButtonPress = ButtonEvent;
    pub const ButtonRelease = ButtonEvent;

    pub const MotionNotify = extern struct {
        response_type: u8,
        detail: u8, // TODO: what is this???????
        sequence: u16,
        time: Timestamp,
        root: Window,
        event: Window,
        child: Window,
        root_x: i16,
        root_y: i16,
        event_x: i16,
        event_y: i16,
        state: ModMask,
        same_screen: bool,
    };

    pub const EnterNotify = extern struct {
        response_type: u8,
        detail: u8, // TODO:
        sequence: u16,
        time: Timestamp,
        root: Window,
        event: Window,
        child: Window,
        root_x: i16,
        root_y: i16,
        event_x: i16,
        event_y: i16,
        state: ModMask,
        mode: u8, // TODO: ???
        same_screen_focus: bool,
    };
    pub const LeaveNotify = EnterNotify;

    pub const FocusIn = extern struct {
        response_type: u8,
        detail: u8, // TODO:
        sequence: u16,
        event: Window,
        mode: u8, // TODO: same as EnterNotify?
        pad0: [3]u8,
    };
    pub const FocusOut = FocusIn;

    pub const KeymapNotify = extern struct {
        response_type: u8,
        keys: [31]u8, // TODO: Keycode? figure out what this is exactly
    };

    pub const Expose = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        window: Window,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        count: u16, // TODO: count of what???
        pad1: [2]u8,
    };

    pub const GraphicsExposure = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        drawable: Drawable,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        minor_opcode: u16, // TODO:
        count: u16, // TODO:
        major_opcode: u8, // TODO:
        pad1: [3]u8,
    };

    pub const NoExposure = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        drawable: Drawable,
        minor_opcode: u16, // TODO:
        major_opcode: u8, // TODO:
        pad1: u8,
    };

    pub const VisibilityNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        window: Window,
        state: u8, // TODO:
        pad1: [3]u8,
    };

    pub const CreateNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        parent: Window,
        window: Window,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        border_width: u16,
        override_redirect: bool,
        pad1: u8,
    };

    pub const DestroyNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
    };

    pub const UnmapNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        from_configure: bool,
        pad1: [3]u8,
    };

    pub const MapNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        override_redirect: bool,
        pad1: [3]u8,
    };

    pub const MapRequest = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        parent: Window,
        window: Window,
    };

    pub const ReparentNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        parent: Window,
        x: u16,
        y: u16,
        override_redirect: bool,
        pad1: [3]u8,
    };

    pub const ConfigureNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        above_sibling: Window,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        border_width: u16,
        override_redirect: bool,
        pad1: u8,
    };

    pub const ConfigureRequest = extern struct {
        response_type: u8,
        stack_mode: u8, // TODO:
        sequence: u16,
        parent: Window,
        window: Window,
        sibling: Window,
        x: u16,
        y: u16,
        width: u16,
        height: u16,
        border_width: u16,
        value_mask: u16, // TODO:
    };

    pub const GravityNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        x: u16,
        y: u16,
    };

    pub const ResizeRequest = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        window: Window,
        width: u16,
        height: u16,
    };

    pub const CirculateNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        event: Window,
        window: Window,
        pad1: [4]u8,
        place: u8, // TODO:
        pad2: [3]u8,
    };
    pub const CirculateRequest = CirculateNotify;

    pub const PropertyNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        window: Window,
        atom: Atom,
        time: Timestamp,
        state: u8, // TODO:
        pad1: [3]u8,
    };

    pub const SelectionClear = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        time: Timestamp,
        owner: Window,
        selection: Atom,
    };

    pub const SelectionRequest = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        time: Timestamp,
        owner: Window,
        requestor: Window,
        selection: Atom,
        target: Atom,
        property: Atom,
    };

    pub const SelectionNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        time: Timestamp,
        requestor: Window,
        selection: Atom,
        target: Atom,
        property: Atom,
    };

    pub const ColormapNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        window: Window,
        colormap: Colormap,
        _new: u8, // TODO:
        state: u8, // TODO:
        pad1: [2]u8,
    };

    pub const ClientMessage = extern struct {
        response_type: u8,
        format: Format, // TODO:
        sequence: u16,
        window: Window,
        type: Atom,
        data: Data,

        pub const Format = enum(u8) { u8 = 8, u16 = 16, u32 = 32 };

        pub const Data = extern union {
            data8: [20]u8,
            data16: [10]u16,
            data32: [5]u32,
        };
    };

    pub const MappingNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16,
        request: u8, // TODO:
        first_keycode: Keycode,
        count: u8,
        pad1: u8,
    };

    pub const GeGeneric = extern struct {
        response_type: u8,
        extension: u8,
        sequence: u16,
        length: u32,
        event_type: u16, // TODO:
        pad0: [22]u8,
        full_sequence: u32,
    };
};

pub fn destroy(pointer: anytype) void {
    raw_allocator.destroy(pointer);
}

pub fn free(memory: anytype) void {
    raw_allocator.free(memory);
}
