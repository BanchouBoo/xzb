const std = @import("std");

const assert = std.debug.assert;
const comptimePrint = std.fmt.comptimePrint;

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

const raw_allocator = std.heap.raw_c_allocator;

pub const copy_from_parent = 0;

pub const Timestamp = enum(u32) {
    current = 0,
    _,
};
pub fn timestamp(time: u32) Timestamp {
    return @intToEnum(Timestamp, time);
}

pub const Keycode = enum(u8) { _ };

pub fn Id(comptime _: type) type {
    return enum(u32) { _ };
}

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

    pub fn generateId(self: *Connection, comptime T: type) !Id(T) {
        try self.checkError();
        switch (T) {
            Window, Pixmap, GraphicsContext, Colormap, Font => {},
            else => @compileError("Invalid type '" ++ @typeName(T) ++ "' for Connection.generateId()!"),
        }
        return @intToEnum(Id(T), xcb_generate_id(self));
    }
    extern fn xcb_generate_id(c: *Connection) u32;

    pub fn getSetup(self: *Connection) !*const Setup {
        try self.checkError();
        return xcb_get_setup(self);
    }
    extern fn xcb_get_setup(c: *Connection) *const Setup;

    pub fn destroy(self: *Connection, comptime T: type, thing: T) void {
        comptime var function = switch (T) {
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
        const tag = @intToEnum(EventType, generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                break :value @unionInit(Event, @tagName(event_type), @ptrCast(*T, generic_event).*);
            },
        };

        raw_allocator.destroy(generic_event);
        return result;
    }
    extern fn xcb_wait_for_event(c: *Connection) ?*Event.Generic;

    pub fn pollForEvent(self: *Connection) !?Event {
        const generic_event = xcb_poll_for_event(self) orelse return null;
        const tag = @intToEnum(EventType, generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                break :value @unionInit(Event, @tagName(event_type), @ptrCast(*T, generic_event).*);
            },
        };

        raw_allocator.destroy(generic_event);
        return result;
    }
    extern fn xcb_poll_for_event(c: *Connection) ?*Event.Generic;

    pub fn pollForQueuedEvent(self: *Connection) ?Event {
        const generic_event = xcb_poll_for_queued_event(self) orelse return null;
        const tag = @intToEnum(EventType, generic_event.response_type & ~@as(u8, 0x80));

        const result = switch (tag) {
            inline else => |event_type| value: {
                @setEvalBranchQuota(2000);
                const T = comptime type: for (std.meta.fields(Event)) |field| {
                    if (std.mem.eql(u8, field.name, @tagName(event_type))) {
                        break :type field.type;
                    }
                };

                break :value @unionInit(Event, @tagName(event_type), @ptrCast(*T, generic_event).*);
            },
        };

        raw_allocator.destroy(generic_event);
        return result;
    }
    extern fn xcb_poll_for_queued_event(c: *Connection) ?*Event.Generic;

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
        const grab_pointer_cookie = xcb_grab_pointer(
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

        const pointer_reply = xcb_grab_pointer_reply(
            self,
            grab_pointer_cookie,
            null,
        );
        defer raw_allocator.destroy(pointer_reply);

        return switch (pointer_reply.status) {
            .already_grabbed => error.XcbCursorAlreadyGrabbed,
            .frozen => error.XcbCursorFrozen,
            .invalid_time => error.XcbCursorInvalidTime,
            .not_viewable => error.XcbCursorNotViewable,
            .success => {},
        };
    }
    extern fn xcb_grab_pointer(
        c: *Connection,
        owner_events: bool,
        grab_window: Window,
        event_mask: EventMask,
        pointer_mode: GrabMode,
        keyboard_mode: GrabMode,
        confine_to: Window,
        cursor: Cursor,
        time: Timestamp,
    ) GrabPointerCookie;
    extern fn xcb_grab_pointer_reply(
        c: *Connection,
        cookie: GrabPointerCookie,
        e: ?**GenericError, // TODO: what would be filled into the error? will there be anything not covered by status?
    ) *GrabPointerReply;

    pub fn ungrabPointer(self: *Connection, time: Timestamp) void {
        _ = xcb_ungrab_pointer(self, time);
    }
    extern fn xcb_ungrab_pointer(c: *Connection, time: Timestamp) VoidCookie;
};

pub const Colormap = packed struct {
    id: Id(Colormap),

    pub fn create(
        connection: *Connection,
        alloc: u8,
        mid: Colormap,
        window: Window,
        visual: VisualId,
    ) !Pixmap {
        const colormap = Colormap{ .id = try connection.generateId(Colormap) };
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

pub const Font = packed struct {
    id: Id(Font),

    pub fn open(
        connection: *Connection,
        name: []const u8,
    ) Font {
        const font = Font{ .id = try connection.generateId(Font) };
        xcb_open_font(connection, font, @truncate(u16, name.len), name.ptr);
    }
    extern fn xcb_open_font(c: *Connection, fid: Font, name_len: u16, name: [*]const u8) VoidCookie;
};

pub const Cursor = packed struct {
    id: Id(Cursor),

    pub const none = Cursor{ .id = @intToEnum(Id(Cursor), 0) };
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

    pub const any = Atom.none;
};

// TODO: rename to just Visual?
pub const VisualId = packed struct {
    id: Id(VisualId),

    pub const copy_from_parent = VisualId{ .id = @intToEnum(Id(VisualId), 0) };
};

pub const Window = packed struct {
    id: Id(Window),

    pub const none = Window{ .id = @intToEnum(Id(Window), 0) };

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
                result |= @enumToInt(value);
            }
            return @intToEnum(ValueMask, result);
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
                    @boolToInt(field_value)
                else
                    @bitCast(u32, field_value);
                i += 1;
            }
        }

        const window = Window{ .id = try connection.generateId(Window) };
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
            @truncate(u32, data.len),
            data.ptr,
        );
    }
    const PropertyMode = enum(u8) { replace, prepend, append };
    const PropertyFormat = enum(u8) { u8 = 8, u16 = 16, u32 = 32 };
    extern fn xcb_change_property(connection: *Connection, mode: PropertyMode, window: Window, property: Atom, property_type: Atom, format: PropertyFormat, data_len: u32, data: [*]const u8) VoidCookie;

    pub fn destroy(self: Window, connection: *Connection) void {
        connection.destroy(Window, self);
    }

    pub fn map(self: Window, connection: *Connection) void {
        _ = xcb_map_window(connection, self);
    }
    extern fn xcb_map_window(connection: *Connection, window: Window) VoidCookie;

    pub fn toDrawable(self: Window) Drawable {
        return Drawable.new(self);
    }
};

pub const Pixmap = packed struct {
    id: Id(Pixmap),

    pub fn create(
        connection: *Connection,
        depth: u8,
        drawable: Drawable,
        width: u16,
        height: u16,
    ) !Pixmap {
        const pixmap = Pixmap{ .id = try connection.generateId(Pixmap) };
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

    pub fn destroy(self: Pixmap, connection: *Connection) void {
        connection.destroy(Pixmap, self);
    }

    pub fn toDrawable(self: Pixmap) Drawable {
        return Drawable.new(self);
    }
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
};

// TODO: wrapper type that holds it's drawable? probably not, as a single GC can be reused for multiple drawables
pub const GraphicsContext = packed struct {
    id: Id(GraphicsContext),

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
                result |= @enumToInt(@as(ValueMask, value));
            }
            return @intToEnum(ValueMask, result);
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
                    @boolToInt(field_value)
                else
                    @bitCast(u32, field_value);
                i += 1;
            }
        }
        const gc = GraphicsContext{ .id = try connection.generateId(GraphicsContext) };
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
                    @boolToInt(field_value)
                else
                    @bitCast(u32, field_value);
                i += 1;
            }
        }

        _ = xcb_change_gc(connection, self, mask, &value_list);
    }
    extern fn xcb_change_gc(c: *Connection, gc: GraphicsContext, mask: ValueMask, values: [*]const u32) VoidCookie;

    const DrawOptions = struct { coordinate_mode: CoordinateMode = .origin };
    const CoordinateMode = enum(u8) { origin = 0, previous = 1 };

    pub fn poly_line(
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
            @truncate(u32, points.len),
            points.ptr,
        );
    }
    extern fn xcb_poly_line(c: *Connection, coordinate_mode: CoordinateMode, drawable: Drawable, gc: GraphicsContext, points_len: u32, points: [*]const Point) VoidCookie;

    pub fn poly_fill_rectangle(
        self: GraphicsContext,
        connection: *Connection,
        drawable: Drawable,
        rectangles: []Rectangle,
    ) void {
        _ = xcb_poly_fill_rectangle(
            connection,
            drawable,
            self,
            @truncate(u32, rectangles.len),
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
    error_code: u8,
    sequence: u16,
    resource_id: u32,
    minor_code: u16,
    major_code: u8,
    pad0: u8,
    pad: [5]u32,
    full_sequence: u32,
};

pub const VoidCookie = extern struct {
    sequence: c_uint,
};

pub const GrabMode = enum(u8) {
    sync = 0,
    @"async" = 1,
};

pub const GrabPointerStatus = enum(u8) {
    success = 0,
    already_grabbed = 1,
    invalid_time = 2,
    not_viewable = 3,
    frozen = 4,
};

pub const GrabPointerCookie = extern struct {
    sequence: c_uint,
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

// TODO: enum types
pub const GrabPointerReply = extern struct {
    response_type: u8,
    status: GrabPointerStatus,
    sequence: u16,
    length: u32,
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
pub const ModMask = enum(u16) {
    shift = 1,
    lock = 2,
    control = 4,
    alt = 8,
    mod_2 = 16,
    mod_3 = 32,
    mod_4 = 64,
    mod_5 = 128,
    any = 32768,
    _,

    pub fn new(mask_values: anytype) ModMask {
        var mask: u32 = 0;
        inline for (mask_values) |value| {
            mask |= @enumToInt(value);
        }
        return @intToEnum(ModMask, mask);
    }

    pub fn has(self: ModMask, mask_values: anytype) bool {
        var mask: u32 = 0;
        inline for (mask_values) |value| {
            mask |= @enumToInt(value);
        }
        return @enumToInt(self) & mask == mask;
    }
};

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

pub const EventMask = enum(c_int) {
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
            result |= @enumToInt(@as(EventMask, value));
        }
        return @intToEnum(EventMask, result);
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
    //     raw_allocator.destroy(@intToPtr(*Generic, address));
    // }

    pub const Generic = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
        pad: [7]u32,
        full_sequence: u32,
    };

    pub const KeyPress = extern struct {
        response_type: u8,
        detail: Keycode,
        sequence: u16, // TODO: enum?
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

    pub const ButtonPress = extern struct {
        response_type: u8,
        detail: Button,
        sequence: u16, //TODO:
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

        pub const Button = enum(u8) {
            left = 1,
            middle = 2,
            right = 3,
            _,
        };
    };
    pub const ButtonRelease = ButtonPress;

    pub const MotionNotify = extern struct {
        response_type: u8,
        detail: u8, // TODO: what is this???????
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        drawable: Drawable,
        minor_opcode: u16, // TODO:
        major_opcode: u8, // TODO:
        pad1: u8,
    };

    pub const VisibilityNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
        window: Window,
        state: u8, // TODO:
        pad1: [3]u8,
    };

    pub const CreateNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        event: Window,
        window: Window,
    };

    pub const UnmapNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        parent: Window,
        window: Window,
    };

    pub const ReparentNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        event: Window,
        window: Window,
        x: u16,
        y: u16,
    };

    pub const ResizeRequest = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
        window: Window,
        width: u16,
        height: u16,
    };

    pub const CirculateNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        window: Window,
        atom: Atom,
        time: Timestamp,
        state: u8, // TODO:
        pad1: [3]u8,
    };

    pub const SelectionClear = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
        time: Timestamp,
        owner: Window,
        selection: Atom,
    };

    pub const SelectionRequest = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
        time: Timestamp,
        requestor: Window,
        selection: Atom,
        target: Atom,
        property: Atom,
    };

    pub const ColormapNotify = extern struct {
        response_type: u8,
        pad0: u8,
        sequence: u16, // TODO:
        window: Window,
        colormap: Colormap,
        _new: u8, // TODO:
        state: u8, // TODO:
        pad1: [2]u8,
    };

    pub const ClientMessage = extern struct {
        response_type: u8,
        format: Format, // TODO:
        sequence: u16, // TODO:
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
        sequence: u16, // TODO:
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
