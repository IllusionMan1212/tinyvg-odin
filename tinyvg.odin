package tinyvg

import "core:os/os2"
import "core:fmt"
import "core:io"
import "core:slice"
import "core:bytes"
import "core:debug/trace"

import "base:runtime"

TINYVG_VERSION :: 1
TINYVG_MAGIC :: [2]byte {0x72, 0x56}
MIN_POLYGON_POINT_COUNT :: 2

ColorEncoding :: enum u8 {
    RGBA8888,
    RGB565,
    RGBAF32,
    Custom,
}

CoordinateRange :: enum u8 {
    Default, // 16 bits
    Reduced, // 8 bits
    Enhanced, // 32 bits
}

CommandIndex :: enum u8 {
    EndOfDocument,
    FillPolygon,
    FillRectangles,
    FillPath,
    DrawLines,
    DrawLineLoop,
    DrawLineStrip,
    DrawLinePath,
    OutlineFillPolygon,
    OutlineFillRectangles,
    OutlineFillPath,
}

StyleKind :: enum u8 {
    FlatColored,
    LinearGradient,
    RadialGradient,
}

PathInstructionKind :: enum u8 {
    Line,
    HorizontalLine,
    VerticalLine,
    CubicBezier,
    ArcCircle,
    ArcEllipse,
    ClosePath,
    QuadraticBezier,
}

Command :: bit_field u8 {
    command_index: CommandIndex | 6,
    prim_style_kind: StyleKind | 2,
}

#assert(size_of(Header) == 4)
Header :: struct {
    magic: [2]byte,
    version: byte,
    using _: bit_field u8 {
        scale: u8 | 4,
        color_encoding: ColorEncoding | 2,
        coordinate_range: CoordinateRange | 2,
    }
}

Gradient :: struct {
    point_0: Point,
    point_1: Point,
    color_index_0: u32,
    color_index_1: u32,
}

Style :: struct {
    kind: StyleKind,
    using _: struct #raw_union {
        flat: u32,
        linear_gradient: Gradient,
        radial_gradient: Gradient,
    }
}

DrawCommand :: union {
    FillPolygon,
    FillRectangles,
    FillPath,
    DrawLines,
    DrawLineLoop,
    DrawLineStrip,
    DrawLinePath,
    OutlineFillPolygon,
    OutlineFillRectangles,
    OutlineFillPath,
}

PathInstruction :: union {
    LineInstruction,
    HorizontalLineInstruction,
    VerticalLineInstruction,
    CubicBezierInstruction,
    ArcCircleInstruction,
    ArcEllipseInstruction,
    ClosePathInstruction,
    QuadraticBezierInstruction,
}

LineInstruction :: struct {
    position: Point,
}
HorizontalLineInstruction :: struct {
    x: Unit,
}
VerticalLineInstruction :: struct {
    y: Unit,
}
CubicBezierInstruction :: struct {
    control_0, control_1: Point,
    point_1: Point,
}
ArcCircleInstruction :: struct {
    large_arc: bool,
    sweep: bool,
    radius: Point,
    target: Point,
}
ArcEllipseInstruction :: struct {
    large_arc: bool,
    sweep: bool,
    radius_x: Unit,
    radius_y: Unit,
    rotation: Unit,
    target: Point,
}
ClosePathInstruction :: struct{}
QuadraticBezierInstruction :: struct {
    control: Point,
    point_1: Point,
}

Segment :: struct {
    start: Point,
    instructions: []PathInstruction,
}

Path :: struct {
    segments: []Segment
}

FillPolygon :: struct {
    style: Style,
    points: []Point,
}

FillRectangles :: struct {
    style: Style,
    rectangles: []Rectangle,
}

FillPath :: struct {
    style: Style,
    path: Path,
}

Line :: struct {
    start, end: Point,
}

DrawLines :: struct {
    style: Style,
    width: Unit,
    lines: []Line,
}

DrawLineLoop :: struct {
    style: Style,
    width: Unit,
    points: []Point,
}

DrawLineStrip :: struct {
    style: Style,
    width: Unit,
    points: []Point,
}

DrawLinePath :: struct {
    style: Style,
    width: Unit,
    path: Path,
}

OutlineFillPolygon :: struct {
    fill_style: Style,
    line_style: Style,
    width: Unit,
    points: []Point,
}

OutlineFillRectangles :: struct {
    fill_style: Style,
    line_style: Style,
    width: Unit,
    rectangles: []Rectangle,
}

OutlineFillPath :: struct {
    fill_style: Style,
    line_style: Style,
    line_width: Unit,
    path: Path,
}

Rectangle :: struct {
    x, y, width, height: Unit
}

Tag :: bit_field u8 {
    instruction:    PathInstructionKind | 3,
    padding_0:      byte                | 1,
    has_line_width: bool                | 1,
    padding_1:      byte                | 3,
}

Color :: [4]f32
Unit :: f32
Point :: [2]Unit

Tvg :: struct {
    width: u32,
    height: u32,
    colors: []Color,
    draw_commands: [dynamic]DrawCommand,
}

TvgError :: enum {
    Unsupported_Color_Encoding,
    Unsupported_Version,
    Invalid_Signature,
    Invalid_Style,
    Invalid_File,
}

Error :: union {
    TvgError,
    io.Error,
    os2.Error,
}

load_from_file :: proc(filename: string, allocator := context.allocator) -> (tvg: ^Tvg, err: Error) {
    data := os2.read_entire_file(filename, allocator) or_return
    defer delete(data, allocator)

    return load_from_bytes(data, allocator)
}

load_from_bytes :: proc(data: []byte, allocator := context.allocator) -> (tvg: ^Tvg, err: Error) {
    context.allocator = allocator

    r: Reader
    bytes.reader_init(&r, data)
    header := read_data(&r, Header) or_return

    if header.magic != TINYVG_MAGIC {
        return nil, .Invalid_Signature
    }

    if header.version != TINYVG_VERSION {
        return nil, .Unsupported_Version
    }

    if header.color_encoding == .Custom {
        return nil, .Unsupported_Color_Encoding
    }

    tvg = new(Tvg)

    switch header.coordinate_range {
    case .Default:
        tvg.width = cast(u32)(read_data(&r, u16) or_return)
        tvg.height = cast(u32)(read_data(&r, u16) or_return)
    case .Reduced:
        tvg.width = cast(u32)(read_data(&r, u8) or_return)
        tvg.height = cast(u32)(read_data(&r, u8) or_return)
    case .Enhanced:
        tvg.width = read_data(&r, u32) or_return
        tvg.height = read_data(&r, u32) or_return
    }

    color_count := read_variable_uint(&r) or_return

    tvg.colors = make([]Color, color_count)

    for i in 0..<color_count {
        #partial switch header.color_encoding {
        case .RGBA8888:
            tvg.colors[i] = {
                f32(bytes.reader_read_byte(&r) or_return) / 255.0,
                f32(bytes.reader_read_byte(&r) or_return) / 255.0,
                f32(bytes.reader_read_byte(&r) or_return) / 255.0,
                f32(bytes.reader_read_byte(&r) or_return) / 255.0,
            }
        case .RGB565:
            // TODO: is the endianness correct here
            rgb565 := read_data(&r, u16) or_return
            tvg.colors[i] = {
                f32(rgb565 & 0x001F) / 31.0,
                f32((rgb565 & 0x07E0) >> 5) / 63.0,
                f32((rgb565 & 0xF800) >> 11) / 31.0,
                1.0,
            }
        case .RGBAF32:
            // TODO: endianness ditto
            tvg.colors[i] = read_data(&r, [4]f32) or_return
        }
    }

    loop: for {
        cmd := read_data(&r, Command) or_return
        switch cmd.command_index {
        case .EndOfDocument:
            assert(cast(int)cmd == 0x00)
            break loop
        case .FillPolygon:
            point_count := (read_variable_uint(&r) or_return) + 1
            if point_count < MIN_POLYGON_POINT_COUNT {
                return nil, TvgError.Invalid_File
            }

            style := read_style(&r, header, cmd.prim_style_kind) or_return

            points := make([]Point, point_count)

            for i in 0..<point_count {
                points[i] = Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return}
            }

            append(&tvg.draw_commands, FillPolygon{style = style, points = points})
        case .FillRectangles:
            rectangle_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return

            rectangles := make([]Rectangle, rectangle_count)

            for i in 0..<rectangle_count {
                rectangles[i] = Rectangle{
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                }
            }

            append(&tvg.draw_commands, FillRectangles{style = style, rectangles = rectangles})
        case .FillPath:
            segment_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return

            path := read_path(&r, header, segment_count) or_return
            append(&tvg.draw_commands, FillPath{style, path})
        case .DrawLines:
            line_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return
            width := read_unit(&r, header) or_return

            lines := make([]Line, line_count)

            for i in 0..<line_count {
                lines[i] = Line{
                    Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return},
                    Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return},
                }
            }

            append(&tvg.draw_commands, DrawLines{style, width, lines})
        case .DrawLineLoop:
            point_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return
            width := read_unit(&r, header) or_return

            points := make([]Point, point_count)

            for i in 0..<point_count {
                points[i] = Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return}
            }

            append(&tvg.draw_commands, DrawLineLoop{style, width, points})
        case .DrawLineStrip:
            point_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return
            width := read_unit(&r, header) or_return

            points := make([]Point, point_count)

            for i in 0..<point_count {
                points[i] = Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return}
            }

            append(&tvg.draw_commands, DrawLineStrip{style, width, points})
        case .DrawLinePath:
            segment_count := (read_variable_uint(&r) or_return) + 1
            style := read_style(&r, header, cmd.prim_style_kind) or_return
            width := read_unit(&r, header) or_return

            path := read_path(&r, header, segment_count) or_return
            append(&tvg.draw_commands, DrawLinePath{style, width, path})
        case .OutlineFillPolygon:
            b := bytes.reader_read_byte(&r) or_return

            segment_count := (b & 0x3F) + 1
            secondary_style_kind := StyleKind(b >> 6)

            fill_style := read_style(&r, header, cmd.prim_style_kind) or_return
            line_style := read_style(&r, header, secondary_style_kind) or_return
            width := read_unit(&r, header) or_return

            points := make([]Point, segment_count)

            for i in 0..<segment_count {
                points[i] = Point{read_unit(&r, header) or_return, read_unit(&r, header) or_return}
            }

            append(&tvg.draw_commands, OutlineFillPolygon{fill_style, line_style, width, points})
        case .OutlineFillRectangles:
            b := bytes.reader_read_byte(&r) or_return

            rect_count := (b & 0x3F) + 1
            secondary_style_kind := StyleKind(b >> 6)

            fill_style := read_style(&r, header, cmd.prim_style_kind) or_return
            line_style := read_style(&r, header, secondary_style_kind) or_return
            width := read_unit(&r, header) or_return

            rectangles := make([]Rectangle, rect_count)

            for i in 0..<rect_count {
                rectangles[i] = Rectangle{
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                    read_unit(&r, header) or_return,
                }
            }

            append(&tvg.draw_commands, OutlineFillRectangles{fill_style, line_style, width, rectangles})
        case .OutlineFillPath:
            b := bytes.reader_read_byte(&r) or_return
            segment_count := (b & 0x3F) + 1
            secondary_style_kind := StyleKind(b >> 6)

            fill_style := read_style(&r, header, cmd.prim_style_kind) or_return
            line_style := read_style(&r, header, secondary_style_kind) or_return
            line_width := read_unit(&r, header) or_return

            path := read_path(&r, header, cast(u32)segment_count) or_return
            append(&tvg.draw_commands, OutlineFillPath{fill_style, line_style, line_width, path})
        case:
            panic(fmt.tprintf("Invalid command: %v", cmd))
        }
    }

    return
}

load :: proc{load_from_file, load_from_bytes}

destroy :: proc(tvg: ^Tvg, allocator := context.allocator) {
    context.allocator = allocator

    delete(tvg.colors)
    for draw_cmd in tvg.draw_commands {
        switch v in draw_cmd {
        case FillPath:
            for seg in v.path.segments {
                delete(seg.instructions)
            }
            delete(v.path.segments)
        case DrawLines:
            delete(v.lines)
        case FillPolygon:
            delete(v.points)
        case DrawLineLoop:
            delete(v.points)
        case DrawLinePath:
            for seg in v.path.segments {
                delete(seg.instructions)
            }
            delete(v.path.segments)
        case DrawLineStrip:
            delete(v.points)
        case FillRectangles:
            delete(v.rectangles)
        case OutlineFillPath:
            for seg in v.path.segments {
                delete(seg.instructions)
            }
            delete(v.path.segments)
        case OutlineFillPolygon:
            delete(v.points)
        case OutlineFillRectangles:
            delete(v.rectangles)
        }
    }
    delete(tvg.draw_commands)

    free(tvg)
}

@(private, require_results)
read_variable_uint :: proc(r: ^Reader) -> (var_uint: u32, err: Error) {
    count: u32 = 0

    for {
        b := bytes.reader_read_byte(r) or_return

        val := u32(b & 0x7F) << (7 * count)
        var_uint |= val

        if b & 0x80 == 0 {
            return var_uint, nil
        }

        count += 1
    }

    return var_uint, nil
}

@(private, require_results)
read_style :: proc(r: ^Reader, header: Header, style_kind: StyleKind) -> (style: Style, err: Error) {
    switch style_kind {
    case .FlatColored:
        color_index := read_variable_uint(r) or_return
        return Style{kind = .FlatColored, flat = color_index}, nil
    case .LinearGradient:
        point_0 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
        point_1 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
        color_index_0 := read_variable_uint(r) or_return
        color_index_1 := read_variable_uint(r) or_return
        return Style{
            kind = .LinearGradient,
            linear_gradient = {
                point_0, point_1, color_index_0, color_index_1,
            }
        }, nil
    case .RadialGradient:
        point_0 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
        point_1 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
        color_index_0 := read_variable_uint(r) or_return
        color_index_1 := read_variable_uint(r) or_return
        return Style{
            kind = .RadialGradient,
            radial_gradient = {
                point_0, point_1, color_index_0, color_index_1,
            }
        }, nil
    }

    return Style{}, .Invalid_Style
}

@(private, require_results)
read_unit :: proc(r: ^Reader, header: Header) -> (unit: f32, err: Error) {
    switch header.coordinate_range {
    case .Default:
        unit = to_fixed_point(read_data(r, u16) or_return, header.scale)
    case .Reduced:
        unit = to_fixed_point(read_data(r, u8) or_return, header.scale)
    case .Enhanced:
        unit = to_fixed_point(read_data(r, u32) or_return, header.scale)
    }

    return
}

@(private, require_results)
read_path :: proc(r: ^Reader, header: Header, segment_count: u32) -> (path: Path, err: Error) {
    segments_instructions: [1024]u32
    segments := make([]Segment, segment_count)

    for s in 0..<segment_count {
        segments_instructions[s] = (read_variable_uint(r) or_return) + 1
    }

    for s in 0..<segment_count {
        segments[s].start = Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
        instructions := make([]PathInstruction, segments_instructions[s])

        for i in 0..<segments_instructions[s] {
            tag := read_data(r, Tag) or_return
            line_width: f32

            if tag.has_line_width {
                line_width = read_unit(r, header) or_return
            }

            switch tag.instruction {
            case .Line:
                position := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}

                instructions[i] = LineInstruction{position}
            case .HorizontalLine:
                x := read_unit(r, header) or_return

                instructions[i] = HorizontalLineInstruction{x}
            case .VerticalLine:
                y := read_unit(r, header) or_return

                instructions[i] = VerticalLineInstruction{y}
            case .CubicBezier:
                control_0 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
                control_1 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
                point_1 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}

                instructions[i] = CubicBezierInstruction{control_0, control_1, point_1}
            case .ArcCircle:
                b := bytes.reader_read_byte(r) or_return
                radius := read_unit(r, header) or_return
                target := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}

                large_arc := bool(b & 0x1)
                sweep := bool(b & 0x2)

                instructions[i] = ArcCircleInstruction{large_arc, sweep, radius, target}
            case .ArcEllipse:
                b := bytes.reader_read_byte(r) or_return
                radius_x := read_unit(r, header) or_return
                radius_y := read_unit(r, header) or_return
                rotation := read_unit(r, header) or_return
                target := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}

                large_arc := bool(b & 0x1)
                sweep := bool(b & 0x2)

                instructions[i] = ArcEllipseInstruction{large_arc, sweep, radius_x, radius_y, rotation, target}
            case .ClosePath:
                instructions[i] = ClosePathInstruction{}
            case .QuadraticBezier:
                control := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}
                point_1 := Point{read_unit(r, header) or_return, read_unit(r, header) or_return}

                instructions[i] = QuadraticBezierInstruction{control, point_1}
            }
        }

        segments[s].instructions = instructions
    }

    path.segments = segments

    return
}

@(private, require_results)
to_fixed_point :: proc(#any_int data: int, fractional_bits: u8) -> f32 {
    return cast(f32)data / f32(u32(1) << fractional_bits)
}
