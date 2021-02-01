const std = @import("std");
const builtin = std.builtin;
const io = std.io;
const fs = std.fs;
const process = std.process;
const ChildProcess = std.ChildProcess;
const print = std.debug.print;
const mem = std.mem;
const testing = std.testing;

const max_doc_file_size = 10 * 1024 * 1024;

const exe_ext = @as(std.zig.CrossTarget, .{}).exeFileExt();
const obj_ext = @as(std.zig.CrossTarget, .{}).oFileExt();
const tmp_dir_name = "docgen_tmp";
const test_out_path = tmp_dir_name ++ fs.path.sep_str ++ "test" ++ exe_ext;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = &arena.allocator;

    var args_it = process.args();

    if (!args_it.skip()) @panic("expected self arg");

    const zig_exe = try (args_it.next(allocator) orelse @panic("expected zig exe arg"));
    defer allocator.free(zig_exe);

    const in_file_name = try (args_it.next(allocator) orelse @panic("expected input arg"));
    defer allocator.free(in_file_name);

    const out_file_name = try (args_it.next(allocator) orelse @panic("expected output arg"));
    defer allocator.free(out_file_name);

    var in_file = try fs.cwd().openFile(in_file_name, .{ .read = true });
    defer in_file.close();

    var out_file = try fs.cwd().createFile(out_file_name, .{});
    defer out_file.close();

    const input_file_bytes = try in_file.reader().readAllAlloc(allocator, max_doc_file_size);

    var buffered_writer = io.bufferedWriter(out_file.writer());

    var tokenizer = Tokenizer.init(in_file_name, input_file_bytes);
    var toc = try genToc(allocator, &tokenizer);

    try fs.cwd().makePath(tmp_dir_name);
    defer fs.cwd().deleteTree(tmp_dir_name) catch {};

    try genHtml(allocator, &tokenizer, &toc, buffered_writer.writer(), zig_exe);
    try buffered_writer.flush();
}

const Token = struct {
    id: Id,
    start: usize,
    end: usize,

    const Id = enum {
        Invalid,
        Content,
        BracketOpen,
        TagContent,
        Separator,
        BracketClose,
        Eof,
    };
};

const Tokenizer = struct {
    buffer: []const u8,
    index: usize,
    state: State,
    source_file_name: []const u8,
    code_node_count: usize,

    const State = enum {
        Start,
        LBracket,
        Hash,
        TagName,
        Eof,
    };

    fn init(source_file_name: []const u8, buffer: []const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
            .state = State.Start,
            .source_file_name = source_file_name,
            .code_node_count = 0,
        };
    }

    fn next(self: *Tokenizer) Token {
        var result = Token{
            .id = Token.Id.Eof,
            .start = self.index,
            .end = undefined,
        };
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (self.state) {
                State.Start => switch (c) {
                    '{' => {
                        self.state = State.LBracket;
                    },
                    else => {
                        result.id = Token.Id.Content;
                    },
                },
                State.LBracket => switch (c) {
                    '#' => {
                        if (result.id != Token.Id.Eof) {
                            self.index -= 1;
                            self.state = State.Start;
                            break;
                        } else {
                            result.id = Token.Id.BracketOpen;
                            self.index += 1;
                            self.state = State.TagName;
                            break;
                        }
                    },
                    else => {
                        result.id = Token.Id.Content;
                        self.state = State.Start;
                    },
                },
                State.TagName => switch (c) {
                    '|' => {
                        if (result.id != Token.Id.Eof) {
                            break;
                        } else {
                            result.id = Token.Id.Separator;
                            self.index += 1;
                            break;
                        }
                    },
                    '#' => {
                        self.state = State.Hash;
                    },
                    else => {
                        result.id = Token.Id.TagContent;
                    },
                },
                State.Hash => switch (c) {
                    '}' => {
                        if (result.id != Token.Id.Eof) {
                            self.index -= 1;
                            self.state = State.TagName;
                            break;
                        } else {
                            result.id = Token.Id.BracketClose;
                            self.index += 1;
                            self.state = State.Start;
                            break;
                        }
                    },
                    else => {
                        result.id = Token.Id.TagContent;
                        self.state = State.TagName;
                    },
                },
                State.Eof => unreachable,
            }
        } else {
            switch (self.state) {
                State.Start, State.LBracket, State.Eof => {},
                else => {
                    result.id = Token.Id.Invalid;
                },
            }
            self.state = State.Eof;
        }
        result.end = self.index;
        return result;
    }

    const Location = struct {
        line: usize,
        column: usize,
        line_start: usize,
        line_end: usize,
    };

    fn getTokenLocation(self: *Tokenizer, token: Token) Location {
        var loc = Location{
            .line = 0,
            .column = 0,
            .line_start = 0,
            .line_end = 0,
        };
        for (self.buffer) |c, i| {
            if (i == token.start) {
                loc.line_end = i;
                while (loc.line_end < self.buffer.len and self.buffer[loc.line_end] != '\n') : (loc.line_end += 1) {}
                return loc;
            }
            if (c == '\n') {
                loc.line += 1;
                loc.column = 0;
                loc.line_start = i + 1;
            } else {
                loc.column += 1;
            }
        }
        return loc;
    }
};

fn parseError(tokenizer: *Tokenizer, token: Token, comptime fmt: []const u8, args: anytype) anyerror {
    const loc = tokenizer.getTokenLocation(token);
    const args_prefix = .{ tokenizer.source_file_name, loc.line + 1, loc.column + 1 };
    print("{s}:{d}:{d}: error: " ++ fmt ++ "\n", args_prefix ++ args);
    if (loc.line_start <= loc.line_end) {
        print("{s}\n", .{tokenizer.buffer[loc.line_start..loc.line_end]});
        {
            var i: usize = 0;
            while (i < loc.column) : (i += 1) {
                print(" ", .{});
            }
        }
        {
            const caret_count = token.end - token.start;
            var i: usize = 0;
            while (i < caret_count) : (i += 1) {
                print("~", .{});
            }
        }
        print("\n", .{});
    }
    return error.ParseError;
}

fn assertToken(tokenizer: *Tokenizer, token: Token, id: Token.Id) !void {
    if (token.id != id) {
        return parseError(tokenizer, token, "expected {s}, found {s}", .{ @tagName(id), @tagName(token.id) });
    }
}

fn eatToken(tokenizer: *Tokenizer, id: Token.Id) !Token {
    const token = tokenizer.next();
    try assertToken(tokenizer, token, id);
    return token;
}

const HeaderOpen = struct {
    name: []const u8,
    url: []const u8,
    n: usize,
};

const SeeAlsoItem = struct {
    name: []const u8,
    token: Token,
};

const ExpectedOutcome = enum {
    Succeed,
    Fail,
    BuildFail,
};

const Code = struct {
    id: Id,
    name: []const u8,
    source_token: Token,
    is_inline: bool,
    mode: builtin.Mode,
    link_objects: []const []const u8,
    target_str: ?[]const u8,
    link_libc: bool,
    disable_cache: bool,

    const Id = union(enum) {
        Test,
        TestError: []const u8,
        TestSafety: []const u8,
        Exe: ExpectedOutcome,
        Obj: ?[]const u8,
        Lib,
    };
};

const Link = struct {
    url: []const u8,
    name: []const u8,
    token: Token,
};

const Node = union(enum) {
    Content: []const u8,
    Nav,
    Builtin: Token,
    HeaderOpen: HeaderOpen,
    SeeAlso: []const SeeAlsoItem,
    Code: Code,
    Link: Link,
    Syntax: Token,
};

const Toc = struct {
    nodes: []Node,
    toc: []u8,
    urls: std.StringHashMap(Token),
};

const Action = enum {
    Open,
    Close,
};

fn genToc(allocator: *mem.Allocator, tokenizer: *Tokenizer) !Toc {
    var urls = std.StringHashMap(Token).init(allocator);
    errdefer urls.deinit();

    var header_stack_size: usize = 0;
    var last_action = Action.Open;
    var last_columns: ?u8 = null;

    var toc_buf = std.ArrayList(u8).init(allocator);
    defer toc_buf.deinit();

    var toc = toc_buf.writer();

    var nodes = std.ArrayList(Node).init(allocator);
    defer nodes.deinit();

    try toc.writeByte('\n');

    while (true) {
        const token = tokenizer.next();
        switch (token.id) {
            Token.Id.Eof => {
                if (header_stack_size != 0) {
                    return parseError(tokenizer, token, "unbalanced headers", .{});
                }
                try toc.writeAll("    </ul>\n");
                break;
            },
            Token.Id.Content => {
                try nodes.append(Node{ .Content = tokenizer.buffer[token.start..token.end] });
            },
            Token.Id.BracketOpen => {
                const tag_token = try eatToken(tokenizer, Token.Id.TagContent);
                const tag_name = tokenizer.buffer[tag_token.start..tag_token.end];

                if (mem.eql(u8, tag_name, "nav")) {
                    _ = try eatToken(tokenizer, Token.Id.BracketClose);

                    try nodes.append(Node.Nav);
                } else if (mem.eql(u8, tag_name, "builtin")) {
                    _ = try eatToken(tokenizer, Token.Id.BracketClose);
                    try nodes.append(Node{ .Builtin = tag_token });
                } else if (mem.eql(u8, tag_name, "header_open")) {
                    _ = try eatToken(tokenizer, Token.Id.Separator);
                    const content_token = try eatToken(tokenizer, Token.Id.TagContent);
                    const content = tokenizer.buffer[content_token.start..content_token.end];
                    var columns: ?u8 = null;
                    while (true) {
                        const bracket_tok = tokenizer.next();
                        switch (bracket_tok.id) {
                            .BracketClose => break,
                            .Separator => continue,
                            .TagContent => {
                                const param = tokenizer.buffer[bracket_tok.start..bracket_tok.end];
                                if (mem.eql(u8, param, "2col")) {
                                    columns = 2;
                                } else {
                                    return parseError(
                                        tokenizer,
                                        bracket_tok,
                                        "unrecognized header_open param: {s}",
                                        .{param},
                                    );
                                }
                            },
                            else => return parseError(tokenizer, bracket_tok, "invalid header_open token", .{}),
                        }
                    }

                    header_stack_size += 1;

                    const urlized = try urlize(allocator, content);
                    try nodes.append(Node{
                        .HeaderOpen = HeaderOpen{
                            .name = content,
                            .url = urlized,
                            .n = header_stack_size,
                        },
                    });
                    if (try urls.fetchPut(urlized, tag_token)) |entry| {
                        parseError(tokenizer, tag_token, "duplicate header url: #{s}", .{urlized}) catch {};
                        parseError(tokenizer, entry.value, "other tag here", .{}) catch {};
                        return error.ParseError;
                    }
                    if (last_action == Action.Open) {
                        try toc.writeByte('\n');
                        try toc.writeByteNTimes(' ', header_stack_size * 4);
                        if (last_columns) |n| {
                            try toc.print("<ul style=\"columns: {}\">\n", .{n});
                        } else {
                            try toc.writeAll("<ul>\n");
                        }
                    } else {
                        last_action = Action.Open;
                    }
                    last_columns = columns;
                    try toc.writeByteNTimes(' ', 4 + header_stack_size * 4);
                    try toc.print("<li><a id=\"toc-{s}\" href=\"#{s}\">{s}</a>", .{ urlized, urlized, content });
                } else if (mem.eql(u8, tag_name, "header_close")) {
                    if (header_stack_size == 0) {
                        return parseError(tokenizer, tag_token, "unbalanced close header", .{});
                    }
                    header_stack_size -= 1;
                    _ = try eatToken(tokenizer, Token.Id.BracketClose);

                    if (last_action == Action.Close) {
                        try toc.writeByteNTimes(' ', 8 + header_stack_size * 4);
                        try toc.writeAll("</ul></li>\n");
                    } else {
                        try toc.writeAll("</li>\n");
                        last_action = Action.Close;
                    }
                } else if (mem.eql(u8, tag_name, "see_also")) {
                    var list = std.ArrayList(SeeAlsoItem).init(allocator);
                    errdefer list.deinit();

                    while (true) {
                        const see_also_tok = tokenizer.next();
                        switch (see_also_tok.id) {
                            Token.Id.TagContent => {
                                const content = tokenizer.buffer[see_also_tok.start..see_also_tok.end];
                                try list.append(SeeAlsoItem{
                                    .name = content,
                                    .token = see_also_tok,
                                });
                            },
                            Token.Id.Separator => {},
                            Token.Id.BracketClose => {
                                try nodes.append(Node{ .SeeAlso = list.toOwnedSlice() });
                                break;
                            },
                            else => return parseError(tokenizer, see_also_tok, "invalid see_also token", .{}),
                        }
                    }
                } else if (mem.eql(u8, tag_name, "link")) {
                    _ = try eatToken(tokenizer, Token.Id.Separator);
                    const name_tok = try eatToken(tokenizer, Token.Id.TagContent);
                    const name = tokenizer.buffer[name_tok.start..name_tok.end];

                    const url_name = blk: {
                        const tok = tokenizer.next();
                        switch (tok.id) {
                            Token.Id.BracketClose => break :blk name,
                            Token.Id.Separator => {
                                const explicit_text = try eatToken(tokenizer, Token.Id.TagContent);
                                _ = try eatToken(tokenizer, Token.Id.BracketClose);
                                break :blk tokenizer.buffer[explicit_text.start..explicit_text.end];
                            },
                            else => return parseError(tokenizer, tok, "invalid link token", .{}),
                        }
                    };

                    try nodes.append(Node{
                        .Link = Link{
                            .url = try urlize(allocator, url_name),
                            .name = name,
                            .token = name_tok,
                        },
                    });
                } else if (mem.eql(u8, tag_name, "code_begin")) {
                    _ = try eatToken(tokenizer, Token.Id.Separator);
                    const code_kind_tok = try eatToken(tokenizer, Token.Id.TagContent);
                    var name: []const u8 = "test";
                    const maybe_sep = tokenizer.next();
                    switch (maybe_sep.id) {
                        Token.Id.Separator => {
                            const name_tok = try eatToken(tokenizer, Token.Id.TagContent);
                            name = tokenizer.buffer[name_tok.start..name_tok.end];
                            _ = try eatToken(tokenizer, Token.Id.BracketClose);
                        },
                        Token.Id.BracketClose => {},
                        else => return parseError(tokenizer, token, "invalid token", .{}),
                    }
                    const code_kind_str = tokenizer.buffer[code_kind_tok.start..code_kind_tok.end];
                    var code_kind_id: Code.Id = undefined;
                    var is_inline = false;
                    if (mem.eql(u8, code_kind_str, "exe")) {
                        code_kind_id = Code.Id{ .Exe = ExpectedOutcome.Succeed };
                    } else if (mem.eql(u8, code_kind_str, "exe_err")) {
                        code_kind_id = Code.Id{ .Exe = ExpectedOutcome.Fail };
                    } else if (mem.eql(u8, code_kind_str, "exe_build_err")) {
                        code_kind_id = Code.Id{ .Exe = ExpectedOutcome.BuildFail };
                    } else if (mem.eql(u8, code_kind_str, "test")) {
                        code_kind_id = Code.Id.Test;
                    } else if (mem.eql(u8, code_kind_str, "test_err")) {
                        code_kind_id = Code.Id{ .TestError = name };
                        name = "test";
                    } else if (mem.eql(u8, code_kind_str, "test_safety")) {
                        code_kind_id = Code.Id{ .TestSafety = name };
                        name = "test";
                    } else if (mem.eql(u8, code_kind_str, "obj")) {
                        code_kind_id = Code.Id{ .Obj = null };
                    } else if (mem.eql(u8, code_kind_str, "obj_err")) {
                        code_kind_id = Code.Id{ .Obj = name };
                        name = "test";
                    } else if (mem.eql(u8, code_kind_str, "lib")) {
                        code_kind_id = Code.Id.Lib;
                    } else if (mem.eql(u8, code_kind_str, "syntax")) {
                        code_kind_id = Code.Id{ .Obj = null };
                        is_inline = true;
                    } else {
                        return parseError(tokenizer, code_kind_tok, "unrecognized code kind: {s}", .{code_kind_str});
                    }

                    var mode: builtin.Mode = .Debug;
                    var link_objects = std.ArrayList([]const u8).init(allocator);
                    defer link_objects.deinit();
                    var target_str: ?[]const u8 = null;
                    var link_libc = false;
                    var disable_cache = false;

                    const source_token = while (true) {
                        const content_tok = try eatToken(tokenizer, Token.Id.Content);
                        _ = try eatToken(tokenizer, Token.Id.BracketOpen);
                        const end_code_tag = try eatToken(tokenizer, Token.Id.TagContent);
                        const end_tag_name = tokenizer.buffer[end_code_tag.start..end_code_tag.end];
                        if (mem.eql(u8, end_tag_name, "code_release_fast")) {
                            mode = .ReleaseFast;
                        } else if (mem.eql(u8, end_tag_name, "code_release_safe")) {
                            mode = .ReleaseSafe;
                        } else if (mem.eql(u8, end_tag_name, "code_disable_cache")) {
                            disable_cache = true;
                        } else if (mem.eql(u8, end_tag_name, "code_link_object")) {
                            _ = try eatToken(tokenizer, Token.Id.Separator);
                            const obj_tok = try eatToken(tokenizer, Token.Id.TagContent);
                            try link_objects.append(tokenizer.buffer[obj_tok.start..obj_tok.end]);
                        } else if (mem.eql(u8, end_tag_name, "target_windows")) {
                            target_str = "x86_64-windows";
                        } else if (mem.eql(u8, end_tag_name, "target_linux_x86_64")) {
                            target_str = "x86_64-linux";
                        } else if (mem.eql(u8, end_tag_name, "target_linux_riscv64")) {
                            target_str = "riscv64-linux";
                        } else if (mem.eql(u8, end_tag_name, "target_wasm")) {
                            target_str = "wasm32-freestanding";
                        } else if (mem.eql(u8, end_tag_name, "target_wasi")) {
                            target_str = "wasm32-wasi";
                        } else if (mem.eql(u8, end_tag_name, "link_libc")) {
                            link_libc = true;
                        } else if (mem.eql(u8, end_tag_name, "code_end")) {
                            _ = try eatToken(tokenizer, Token.Id.BracketClose);
                            break content_tok;
                        } else {
                            return parseError(
                                tokenizer,
                                end_code_tag,
                                "invalid token inside code_begin: {s}",
                                .{end_tag_name},
                            );
                        }
                        _ = try eatToken(tokenizer, Token.Id.BracketClose);
                    } else
                        unreachable; // TODO issue #707
                    try nodes.append(Node{
                        .Code = Code{
                            .id = code_kind_id,
                            .name = name,
                            .source_token = source_token,
                            .is_inline = is_inline,
                            .mode = mode,
                            .link_objects = link_objects.toOwnedSlice(),
                            .target_str = target_str,
                            .link_libc = link_libc,
                            .disable_cache = disable_cache,
                        },
                    });
                    tokenizer.code_node_count += 1;
                } else if (mem.eql(u8, tag_name, "syntax")) {
                    _ = try eatToken(tokenizer, Token.Id.BracketClose);
                    const content_tok = try eatToken(tokenizer, Token.Id.Content);
                    _ = try eatToken(tokenizer, Token.Id.BracketOpen);
                    const end_syntax_tag = try eatToken(tokenizer, Token.Id.TagContent);
                    const end_tag_name = tokenizer.buffer[end_syntax_tag.start..end_syntax_tag.end];
                    if (!mem.eql(u8, end_tag_name, "endsyntax")) {
                        return parseError(
                            tokenizer,
                            end_syntax_tag,
                            "invalid token inside syntax: {s}",
                            .{end_tag_name},
                        );
                    }
                    _ = try eatToken(tokenizer, Token.Id.BracketClose);
                    try nodes.append(Node{ .Syntax = content_tok });
                } else {
                    return parseError(tokenizer, tag_token, "unrecognized tag name: {s}", .{tag_name});
                }
            },
            else => return parseError(tokenizer, token, "invalid token", .{}),
        }
    }

    return Toc{
        .nodes = nodes.toOwnedSlice(),
        .toc = toc_buf.toOwnedSlice(),
        .urls = urls,
    };
}

fn urlize(allocator: *mem.Allocator, input: []const u8) ![]u8 {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    const out = buf.writer();
    for (input) |c| {
        switch (c) {
            'a'...'z', 'A'...'Z', '_', '-', '0'...'9' => {
                try out.writeByte(c);
            },
            ' ' => {
                try out.writeByte('-');
            },
            else => {},
        }
    }
    return buf.toOwnedSlice();
}

fn escapeHtml(allocator: *mem.Allocator, input: []const u8) ![]u8 {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    const out = buf.writer();
    try writeEscaped(out, input);
    return buf.toOwnedSlice();
}

fn writeEscaped(out: anytype, input: []const u8) !void {
    for (input) |c| {
        try switch (c) {
            '&' => out.writeAll("&amp;"),
            '<' => out.writeAll("&lt;"),
            '>' => out.writeAll("&gt;"),
            '"' => out.writeAll("&quot;"),
            else => out.writeByte(c),
        };
    }
}

//#define VT_RED "\x1b[31;1m"
//#define VT_GREEN "\x1b[32;1m"
//#define VT_CYAN "\x1b[36;1m"
//#define VT_WHITE "\x1b[37;1m"
//#define VT_BOLD "\x1b[0;1m"
//#define VT_RESET "\x1b[0m"

const TermState = enum {
    Start,
    Escape,
    LBracket,
    Number,
    AfterNumber,
    Arg,
    ArgNumber,
    ExpectEnd,
};

test "term color" {
    const input_bytes = "A\x1b[32;1mgreen\x1b[0mB";
    const result = try termColor(std.testing.allocator, input_bytes);
    defer std.testing.allocator.free(result);
    testing.expectEqualSlices(u8, "A<span class=\"t32\">green</span>B", result);
}

fn termColor(allocator: *mem.Allocator, input: []const u8) ![]u8 {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    var out = buf.writer();
    var number_start_index: usize = undefined;
    var first_number: usize = undefined;
    var second_number: usize = undefined;
    var i: usize = 0;
    var state = TermState.Start;
    var open_span_count: usize = 0;
    while (i < input.len) : (i += 1) {
        const c = input[i];
        switch (state) {
            TermState.Start => switch (c) {
                '\x1b' => state = TermState.Escape,
                else => try out.writeByte(c),
            },
            TermState.Escape => switch (c) {
                '[' => state = TermState.LBracket,
                else => return error.UnsupportedEscape,
            },
            TermState.LBracket => switch (c) {
                '0'...'9' => {
                    number_start_index = i;
                    state = TermState.Number;
                },
                else => return error.UnsupportedEscape,
            },
            TermState.Number => switch (c) {
                '0'...'9' => {},
                else => {
                    first_number = std.fmt.parseInt(usize, input[number_start_index..i], 10) catch unreachable;
                    second_number = 0;
                    state = TermState.AfterNumber;
                    i -= 1;
                },
            },

            TermState.AfterNumber => switch (c) {
                ';' => state = TermState.Arg,
                else => {
                    state = TermState.ExpectEnd;
                    i -= 1;
                },
            },
            TermState.Arg => switch (c) {
                '0'...'9' => {
                    number_start_index = i;
                    state = TermState.ArgNumber;
                },
                else => return error.UnsupportedEscape,
            },
            TermState.ArgNumber => switch (c) {
                '0'...'9' => {},
                else => {
                    second_number = std.fmt.parseInt(usize, input[number_start_index..i], 10) catch unreachable;
                    state = TermState.ExpectEnd;
                    i -= 1;
                },
            },
            TermState.ExpectEnd => switch (c) {
                'm' => {
                    state = TermState.Start;
                    while (open_span_count != 0) : (open_span_count -= 1) {
                        try out.writeAll("</span>");
                    }
                    if (first_number != 0 or second_number != 0) {
                        try out.print("<span class=\"t{d}_{d}\">", .{ first_number, second_number });
                        open_span_count += 1;
                    }
                },
                else => return error.UnsupportedEscape,
            },
        }
    }
    return buf.toOwnedSlice();
}

const builtin_types = [_][]const u8{
    "f16",         "f32",      "f64",    "f128",     "c_longdouble", "c_short",
    "c_ushort",    "c_int",    "c_uint", "c_long",   "c_ulong",      "c_longlong",
    "c_ulonglong", "c_char",   "c_void", "void",     "bool",         "isize",
    "usize",       "noreturn", "type",   "anyerror", "comptime_int", "comptime_float",
};

fn isType(name: []const u8) bool {
    for (builtin_types) |t| {
        if (mem.eql(u8, t, name))
            return true;
    }
    return false;
}

fn tokenizeAndPrintRaw(docgen_tokenizer: *Tokenizer, out: anytype, source_token: Token, raw_src: []const u8) !void {
    const src = mem.trim(u8, raw_src, " \n");
    try out.writeAll("<code class=\"zig\">");
    var tokenizer = std.zig.Tokenizer.init(src);
    var index: usize = 0;
    var next_tok_is_fn = false;
    while (true) {
        const prev_tok_was_fn = next_tok_is_fn;
        next_tok_is_fn = false;

        const token = tokenizer.next();
        try writeEscaped(out, src[index..token.loc.start]);
        switch (token.id) {
            .Eof => break,

            .Keyword_align,
            .Keyword_and,
            .Keyword_asm,
            .Keyword_async,
            .Keyword_await,
            .Keyword_break,
            .Keyword_catch,
            .Keyword_comptime,
            .Keyword_const,
            .Keyword_continue,
            .Keyword_defer,
            .Keyword_else,
            .Keyword_enum,
            .Keyword_errdefer,
            .Keyword_error,
            .Keyword_export,
            .Keyword_extern,
            .Keyword_for,
            .Keyword_if,
            .Keyword_inline,
            .Keyword_noalias,
            .Keyword_noinline,
            .Keyword_nosuspend,
            .Keyword_opaque,
            .Keyword_or,
            .Keyword_orelse,
            .Keyword_packed,
            .Keyword_anyframe,
            .Keyword_pub,
            .Keyword_resume,
            .Keyword_return,
            .Keyword_linksection,
            .Keyword_callconv,
            .Keyword_struct,
            .Keyword_suspend,
            .Keyword_switch,
            .Keyword_test,
            .Keyword_threadlocal,
            .Keyword_try,
            .Keyword_union,
            .Keyword_unreachable,
            .Keyword_usingnamespace,
            .Keyword_var,
            .Keyword_volatile,
            .Keyword_allowzero,
            .Keyword_while,
            .Keyword_anytype,
            => {
                try out.writeAll("<span class=\"tok-kw\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .Keyword_fn => {
                try out.writeAll("<span class=\"tok-kw\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
                next_tok_is_fn = true;
            },

            .Keyword_undefined,
            .Keyword_null,
            .Keyword_true,
            .Keyword_false,
            => {
                try out.writeAll("<span class=\"tok-null\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .StringLiteral,
            .MultilineStringLiteralLine,
            .CharLiteral,
            => {
                try out.writeAll("<span class=\"tok-str\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .Builtin => {
                try out.writeAll("<span class=\"tok-builtin\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .LineComment,
            .DocComment,
            .ContainerDocComment,
            .ShebangLine,
            => {
                try out.writeAll("<span class=\"tok-comment\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .Identifier => {
                if (prev_tok_was_fn) {
                    try out.writeAll("<span class=\"tok-fn\">");
                    try writeEscaped(out, src[token.loc.start..token.loc.end]);
                    try out.writeAll("</span>");
                } else {
                    const is_int = blk: {
                        if (src[token.loc.start] != 'i' and src[token.loc.start] != 'u')
                            break :blk false;
                        var i = token.loc.start + 1;
                        if (i == token.loc.end)
                            break :blk false;
                        while (i != token.loc.end) : (i += 1) {
                            if (src[i] < '0' or src[i] > '9')
                                break :blk false;
                        }
                        break :blk true;
                    };
                    if (is_int or isType(src[token.loc.start..token.loc.end])) {
                        try out.writeAll("<span class=\"tok-type\">");
                        try writeEscaped(out, src[token.loc.start..token.loc.end]);
                        try out.writeAll("</span>");
                    } else {
                        try writeEscaped(out, src[token.loc.start..token.loc.end]);
                    }
                }
            },

            .IntegerLiteral,
            .FloatLiteral,
            => {
                try out.writeAll("<span class=\"tok-number\">");
                try writeEscaped(out, src[token.loc.start..token.loc.end]);
                try out.writeAll("</span>");
            },

            .Bang,
            .Pipe,
            .PipePipe,
            .PipeEqual,
            .Equal,
            .EqualEqual,
            .EqualAngleBracketRight,
            .BangEqual,
            .LParen,
            .RParen,
            .Semicolon,
            .Percent,
            .PercentEqual,
            .LBrace,
            .RBrace,
            .LBracket,
            .RBracket,
            .Period,
            .PeriodAsterisk,
            .Ellipsis2,
            .Ellipsis3,
            .Caret,
            .CaretEqual,
            .Plus,
            .PlusPlus,
            .PlusEqual,
            .PlusPercent,
            .PlusPercentEqual,
            .Minus,
            .MinusEqual,
            .MinusPercent,
            .MinusPercentEqual,
            .Asterisk,
            .AsteriskEqual,
            .AsteriskAsterisk,
            .AsteriskPercent,
            .AsteriskPercentEqual,
            .Arrow,
            .Colon,
            .Slash,
            .SlashEqual,
            .Comma,
            .Ampersand,
            .AmpersandEqual,
            .QuestionMark,
            .AngleBracketLeft,
            .AngleBracketLeftEqual,
            .AngleBracketAngleBracketLeft,
            .AngleBracketAngleBracketLeftEqual,
            .AngleBracketRight,
            .AngleBracketRightEqual,
            .AngleBracketAngleBracketRight,
            .AngleBracketAngleBracketRightEqual,
            .Tilde,
            => try writeEscaped(out, src[token.loc.start..token.loc.end]),

            .Invalid, .Invalid_ampersands, .Invalid_periodasterisks => return parseError(
                docgen_tokenizer,
                source_token,
                "syntax error",
                .{},
            ),
        }
        index = token.loc.end;
    }
    try out.writeAll("</code>");
}

fn tokenizeAndPrint(docgen_tokenizer: *Tokenizer, out: anytype, source_token: Token) !void {
    const raw_src = docgen_tokenizer.buffer[source_token.start..source_token.end];
    return tokenizeAndPrintRaw(docgen_tokenizer, out, source_token, raw_src);
}

fn genHtml(allocator: *mem.Allocator, tokenizer: *Tokenizer, toc: *Toc, out: anytype, zig_exe: []const u8) !void {
    var code_progress_index: usize = 0;

    var env_map = try process.getEnvMap(allocator);
    try env_map.set("ZIG_DEBUG_COLOR", "1");

    const builtin_code = try getBuiltinCode(allocator, &env_map, zig_exe);

    for (toc.nodes) |node| {
        switch (node) {
            .Content => |data| {
                try out.writeAll(data);
            },
            .Link => |info| {
                if (!toc.urls.contains(info.url)) {
                    return parseError(tokenizer, info.token, "url not found: {s}", .{info.url});
                }
                try out.print("<a href=\"#{s}\">{s}</a>", .{ info.url, info.name });
            },
            .Nav => {
                try out.writeAll(toc.toc);
            },
            .Builtin => |tok| {
                try out.writeAll("<pre>");
                try tokenizeAndPrintRaw(tokenizer, out, tok, builtin_code);
                try out.writeAll("</pre>");
            },
            .HeaderOpen => |info| {
                try out.print(
                    "<h{d} id=\"{s}\"><a href=\"#toc-{s}\">{s}</a> <a class=\"hdr\" href=\"#{s}\">§</a></h{d}>\n",
                    .{ info.n, info.url, info.url, info.name, info.url, info.n },
                );
            },
            .SeeAlso => |items| {
                try out.writeAll("<p>See also:</p><ul>\n");
                for (items) |item| {
                    const url = try urlize(allocator, item.name);
                    if (!toc.urls.contains(url)) {
                        return parseError(tokenizer, item.token, "url not found: {s}", .{url});
                    }
                    try out.print("<li><a href=\"#{s}\">{s}</a></li>\n", .{ url, item.name });
                }
                try out.writeAll("</ul>\n");
            },
            .Syntax => |content_tok| {
                try tokenizeAndPrint(tokenizer, out, content_tok);
            },
            .Code => |code| {
                code_progress_index += 1;
                print("docgen example code {}/{}...", .{ code_progress_index, tokenizer.code_node_count });

                const raw_source = tokenizer.buffer[code.source_token.start..code.source_token.end];
                const trimmed_raw_source = mem.trim(u8, raw_source, " \n");
                if (!code.is_inline) {
                    try out.print("<p class=\"file\">{s}.zig</p>", .{code.name});
                }
                try out.writeAll("<pre>");
                try tokenizeAndPrint(tokenizer, out, code.source_token);
                try out.writeAll("</pre>");
                const name_plus_ext = try std.fmt.allocPrint(allocator, "{s}.zig", .{code.name});
                const tmp_source_file_name = try fs.path.join(
                    allocator,
                    &[_][]const u8{ tmp_dir_name, name_plus_ext },
                );
                try fs.cwd().writeFile(tmp_source_file_name, trimmed_raw_source);

                switch (code.id) {
                    Code.Id.Exe => |expected_outcome| code_block: {
                        const name_plus_bin_ext = try std.fmt.allocPrint(allocator, "{s}{s}", .{ code.name, exe_ext });
                        var build_args = std.ArrayList([]const u8).init(allocator);
                        defer build_args.deinit();
                        try build_args.appendSlice(&[_][]const u8{
                            zig_exe,          "build-exe",
                            "--name",         code.name,
                            "--color",        "on",
                            "--enable-cache", tmp_source_file_name,
                        });
                        try out.print("<pre><code class=\"shell\">$ zig build-exe {s}.zig", .{code.name});
                        switch (code.mode) {
                            .Debug => {},
                            else => {
                                try build_args.appendSlice(&[_][]const u8{ "-O", @tagName(code.mode) });
                                try out.print(" -O {s}", .{@tagName(code.mode)});
                            },
                        }
                        for (code.link_objects) |link_object| {
                            const name_with_ext = try std.fmt.allocPrint(allocator, "{s}{s}", .{ link_object, obj_ext });
                            const full_path_object = try fs.path.join(
                                allocator,
                                &[_][]const u8{ tmp_dir_name, name_with_ext },
                            );
                            try build_args.append(full_path_object);
                            try out.print(" {s}", .{name_with_ext});
                        }
                        if (code.link_libc) {
                            try build_args.append("-lc");
                            try out.print(" -lc", .{});
                        }
                        const target = try std.zig.CrossTarget.parse(.{
                            .arch_os_abi = code.target_str orelse "native",
                        });
                        if (code.target_str) |triple| {
                            try build_args.appendSlice(&[_][]const u8{ "-target", triple });
                            if (!code.is_inline) {
                                try out.print(" -target {s}", .{triple});
                            }
                        }
                        if (expected_outcome == .BuildFail) {
                            const result = try ChildProcess.exec(.{
                                .allocator = allocator,
                                .argv = build_args.items,
                                .env_map = &env_map,
                                .max_output_bytes = max_doc_file_size,
                            });
                            switch (result.term) {
                                .Exited => |exit_code| {
                                    if (exit_code == 0) {
                                        print("{s}\nThe following command incorrectly succeeded:\n", .{result.stderr});
                                        dumpArgs(build_args.items);
                                        return parseError(tokenizer, code.source_token, "example incorrectly compiled", .{});
                                    }
                                },
                                else => {
                                    print("{s}\nThe following command crashed:\n", .{result.stderr});
                                    dumpArgs(build_args.items);
                                    return parseError(tokenizer, code.source_token, "example compile crashed", .{});
                                },
                            }
                            const escaped_stderr = try escapeHtml(allocator, result.stderr);
                            const colored_stderr = try termColor(allocator, escaped_stderr);
                            try out.print("\n{s}</code></pre>\n", .{colored_stderr});
                            break :code_block;
                        }
                        const exec_result = exec(allocator, &env_map, build_args.items) catch
                            return parseError(tokenizer, code.source_token, "example failed to compile", .{});

                        if (code.target_str) |triple| {
                            if (mem.startsWith(u8, triple, "wasm32") or
                                mem.startsWith(u8, triple, "riscv64-linux") or
                                (mem.startsWith(u8, triple, "x86_64-linux") and
                                std.Target.current.os.tag != .linux or std.Target.current.cpu.arch != .x86_64))
                            {
                                // skip execution
                                try out.print("</code></pre>\n", .{});
                                break :code_block;
                            }
                        }

                        const path_to_exe_dir = mem.trim(u8, exec_result.stdout, " \r\n");
                        const path_to_exe_basename = try std.fmt.allocPrint(allocator, "{s}{s}", .{
                            code.name,
                            target.exeFileExt(),
                        });
                        const path_to_exe = try fs.path.join(allocator, &[_][]const u8{
                            path_to_exe_dir,
                            path_to_exe_basename,
                        });
                        const run_args = &[_][]const u8{path_to_exe};

                        var exited_with_signal = false;

                        const result = if (expected_outcome == ExpectedOutcome.Fail) blk: {
                            const result = try ChildProcess.exec(.{
                                .allocator = allocator,
                                .argv = run_args,
                                .env_map = &env_map,
                                .max_output_bytes = max_doc_file_size,
                            });
                            switch (result.term) {
                                .Exited => |exit_code| {
                                    if (exit_code == 0) {
                                        print("{s}\nThe following command incorrectly succeeded:\n", .{result.stderr});
                                        dumpArgs(run_args);
                                        return parseError(tokenizer, code.source_token, "example incorrectly compiled", .{});
                                    }
                                },
                                .Signal => exited_with_signal = true,
                                else => {},
                            }
                            break :blk result;
                        } else blk: {
                            break :blk exec(allocator, &env_map, run_args) catch return parseError(tokenizer, code.source_token, "example crashed", .{});
                        };

                        const escaped_stderr = try escapeHtml(allocator, result.stderr);
                        const escaped_stdout = try escapeHtml(allocator, result.stdout);

                        const colored_stderr = try termColor(allocator, escaped_stderr);
                        const colored_stdout = try termColor(allocator, escaped_stdout);

                        try out.print("\n$ ./{s}\n{s}{s}", .{ code.name, colored_stdout, colored_stderr });
                        if (exited_with_signal) {
                            try out.print("(process terminated by signal)", .{});
                        }
                        try out.print("</code></pre>\n", .{});
                    },
                    Code.Id.Test => {
                        var test_args = std.ArrayList([]const u8).init(allocator);
                        defer test_args.deinit();

                        try test_args.appendSlice(&[_][]const u8{ zig_exe, "test", tmp_source_file_name });
                        try out.print("<pre><code class=\"shell\">$ zig test {s}.zig", .{code.name});
                        switch (code.mode) {
                            .Debug => {},
                            else => {
                                try test_args.appendSlice(&[_][]const u8{ "-O", @tagName(code.mode) });
                                try out.print(" -O {s}", .{@tagName(code.mode)});
                            },
                        }
                        if (code.link_libc) {
                            try test_args.append("-lc");
                            try out.print(" -lc", .{});
                        }
                        if (code.target_str) |triple| {
                            try test_args.appendSlice(&[_][]const u8{ "-target", triple });
                            try out.print(" -target {s}", .{triple});
                        }
                        const result = exec(allocator, &env_map, test_args.items) catch return parseError(tokenizer, code.source_token, "test failed", .{});
                        const escaped_stderr = try escapeHtml(allocator, result.stderr);
                        const escaped_stdout = try escapeHtml(allocator, result.stdout);
                        try out.print("\n{s}{s}</code></pre>\n", .{ escaped_stderr, escaped_stdout });
                    },
                    Code.Id.TestError => |error_match| {
                        var test_args = std.ArrayList([]const u8).init(allocator);
                        defer test_args.deinit();

                        try test_args.appendSlice(&[_][]const u8{
                            zig_exe,
                            "test",
                            "--color",
                            "on",
                            tmp_source_file_name,
                        });
                        try out.print("<pre><code class=\"shell\">$ zig test {s}.zig", .{code.name});
                        switch (code.mode) {
                            .Debug => {},
                            else => {
                                try test_args.appendSlice(&[_][]const u8{ "-O", @tagName(code.mode) });
                                try out.print(" -O {s}", .{@tagName(code.mode)});
                            },
                        }
                        const result = try ChildProcess.exec(.{
                            .allocator = allocator,
                            .argv = test_args.items,
                            .env_map = &env_map,
                            .max_output_bytes = max_doc_file_size,
                        });
                        switch (result.term) {
                            .Exited => |exit_code| {
                                if (exit_code == 0) {
                                    print("{s}\nThe following command incorrectly succeeded:\n", .{result.stderr});
                                    dumpArgs(test_args.items);
                                    return parseError(tokenizer, code.source_token, "example incorrectly compiled", .{});
                                }
                            },
                            else => {
                                print("{s}\nThe following command crashed:\n", .{result.stderr});
                                dumpArgs(test_args.items);
                                return parseError(tokenizer, code.source_token, "example compile crashed", .{});
                            },
                        }
                        if (mem.indexOf(u8, result.stderr, error_match) == null) {
                            print("{s}\nExpected to find '{s}' in stderr\n", .{ result.stderr, error_match });
                            return parseError(tokenizer, code.source_token, "example did not have expected compile error", .{});
                        }
                        const escaped_stderr = try escapeHtml(allocator, result.stderr);
                        const colored_stderr = try termColor(allocator, escaped_stderr);
                        try out.print("\n{s}</code></pre>\n", .{colored_stderr});
                    },

                    Code.Id.TestSafety => |error_match| {
                        var test_args = std.ArrayList([]const u8).init(allocator);
                        defer test_args.deinit();

                        try test_args.appendSlice(&[_][]const u8{
                            zig_exe,
                            "test",
                            tmp_source_file_name,
                        });
                        var mode_arg: []const u8 = "";
                        switch (code.mode) {
                            .Debug => {},
                            .ReleaseSafe => {
                                try test_args.append("-OReleaseSafe");
                                mode_arg = "-OReleaseSafe";
                            },
                            .ReleaseFast => {
                                try test_args.append("-OReleaseFast");
                                mode_arg = "-OReleaseFast";
                            },
                            .ReleaseSmall => {
                                try test_args.append("-OReleaseSmall");
                                mode_arg = "-OReleaseSmall";
                            },
                        }

                        const result = try ChildProcess.exec(.{
                            .allocator = allocator,
                            .argv = test_args.items,
                            .env_map = &env_map,
                            .max_output_bytes = max_doc_file_size,
                        });
                        switch (result.term) {
                            .Exited => |exit_code| {
                                if (exit_code == 0) {
                                    print("{s}\nThe following command incorrectly succeeded:\n", .{result.stderr});
                                    dumpArgs(test_args.items);
                                    return parseError(tokenizer, code.source_token, "example test incorrectly succeeded", .{});
                                }
                            },
                            else => {
                                print("{s}\nThe following command crashed:\n", .{result.stderr});
                                dumpArgs(test_args.items);
                                return parseError(tokenizer, code.source_token, "example compile crashed", .{});
                            },
                        }
                        if (mem.indexOf(u8, result.stderr, error_match) == null) {
                            print("{s}\nExpected to find '{s}' in stderr\n", .{ result.stderr, error_match });
                            return parseError(tokenizer, code.source_token, "example did not have expected runtime safety error message", .{});
                        }
                        const escaped_stderr = try escapeHtml(allocator, result.stderr);
                        const colored_stderr = try termColor(allocator, escaped_stderr);
                        try out.print("<pre><code class=\"shell\">$ zig test {s}.zig{s}\n{s}</code></pre>\n", .{
                            code.name,
                            mode_arg,
                            colored_stderr,
                        });
                    },
                    Code.Id.Obj => |maybe_error_match| {
                        const name_plus_obj_ext = try std.fmt.allocPrint(allocator, "{s}{s}", .{ code.name, obj_ext });
                        const tmp_obj_file_name = try fs.path.join(
                            allocator,
                            &[_][]const u8{ tmp_dir_name, name_plus_obj_ext },
                        );
                        var build_args = std.ArrayList([]const u8).init(allocator);
                        defer build_args.deinit();

                        const name_plus_h_ext = try std.fmt.allocPrint(allocator, "{s}.h", .{code.name});
                        const output_h_file_name = try fs.path.join(
                            allocator,
                            &[_][]const u8{ tmp_dir_name, name_plus_h_ext },
                        );

                        try build_args.appendSlice(&[_][]const u8{
                            zig_exe,
                            "build-obj",
                            tmp_source_file_name,
                            "--color",
                            "on",
                            "--name",
                            code.name,
                            try std.fmt.allocPrint(allocator, "-femit-bin={s}{c}{s}", .{
                                tmp_dir_name, fs.path.sep, name_plus_obj_ext,
                            }),
                        });
                        if (!code.is_inline) {
                            try out.print("<pre><code class=\"shell\">$ zig build-obj {s}.zig", .{code.name});
                        }

                        switch (code.mode) {
                            .Debug => {},
                            else => {
                                try build_args.appendSlice(&[_][]const u8{ "-O", @tagName(code.mode) });
                                if (!code.is_inline) {
                                    try out.print(" -O {s}", .{@tagName(code.mode)});
                                }
                            },
                        }

                        if (code.target_str) |triple| {
                            try build_args.appendSlice(&[_][]const u8{ "-target", triple });
                            try out.print(" -target {s}", .{triple});
                        }

                        if (maybe_error_match) |error_match| {
                            const result = try ChildProcess.exec(.{
                                .allocator = allocator,
                                .argv = build_args.items,
                                .env_map = &env_map,
                                .max_output_bytes = max_doc_file_size,
                            });
                            switch (result.term) {
                                .Exited => |exit_code| {
                                    if (exit_code == 0) {
                                        print("{s}\nThe following command incorrectly succeeded:\n", .{result.stderr});
                                        dumpArgs(build_args.items);
                                        return parseError(tokenizer, code.source_token, "example build incorrectly succeeded", .{});
                                    }
                                },
                                else => {
                                    print("{s}\nThe following command crashed:\n", .{result.stderr});
                                    dumpArgs(build_args.items);
                                    return parseError(tokenizer, code.source_token, "example compile crashed", .{});
                                },
                            }
                            if (mem.indexOf(u8, result.stderr, error_match) == null) {
                                print("{s}\nExpected to find '{s}' in stderr\n", .{ result.stderr, error_match });
                                return parseError(tokenizer, code.source_token, "example did not have expected compile error message", .{});
                            }
                            const escaped_stderr = try escapeHtml(allocator, result.stderr);
                            const colored_stderr = try termColor(allocator, escaped_stderr);
                            try out.print("\n{s}", .{colored_stderr});
                        } else {
                            _ = exec(allocator, &env_map, build_args.items) catch return parseError(tokenizer, code.source_token, "example failed to compile", .{});
                        }
                        if (!code.is_inline) {
                            try out.print("</code></pre>\n", .{});
                        }
                    },
                    Code.Id.Lib => {
                        const bin_basename = try std.zig.binNameAlloc(allocator, .{
                            .root_name = code.name,
                            .target = std.Target.current,
                            .output_mode = .Lib,
                        });

                        var test_args = std.ArrayList([]const u8).init(allocator);
                        defer test_args.deinit();

                        try test_args.appendSlice(&[_][]const u8{
                            zig_exe,
                            "build-lib",
                            tmp_source_file_name,
                            try std.fmt.allocPrint(allocator, "-femit-bin={s}{s}{s}", .{
                                tmp_dir_name, fs.path.sep_str, bin_basename,
                            }),
                        });
                        try out.print("<pre><code class=\"shell\">$ zig build-lib {s}.zig", .{code.name});
                        switch (code.mode) {
                            .Debug => {},
                            else => {
                                try test_args.appendSlice(&[_][]const u8{ "-O", @tagName(code.mode) });
                                try out.print(" -O {s}", .{@tagName(code.mode)});
                            },
                        }
                        if (code.target_str) |triple| {
                            try test_args.appendSlice(&[_][]const u8{ "-target", triple });
                            try out.print(" -target {s}", .{triple});
                        }
                        const result = exec(allocator, &env_map, test_args.items) catch return parseError(tokenizer, code.source_token, "test failed", .{});
                        const escaped_stderr = try escapeHtml(allocator, result.stderr);
                        const escaped_stdout = try escapeHtml(allocator, result.stdout);
                        try out.print("\n{s}{s}</code></pre>\n", .{ escaped_stderr, escaped_stdout });
                    },
                }
                print("OK\n", .{});
            },
        }
    }
}

fn exec(allocator: *mem.Allocator, env_map: *std.BufMap, args: []const []const u8) !ChildProcess.ExecResult {
    const result = try ChildProcess.exec(.{
        .allocator = allocator,
        .argv = args,
        .env_map = env_map,
        .max_output_bytes = max_doc_file_size,
    });
    switch (result.term) {
        .Exited => |exit_code| {
            if (exit_code != 0) {
                print("{s}\nThe following command exited with code {}:\n", .{ result.stderr, exit_code });
                dumpArgs(args);
                return error.ChildExitError;
            }
        },
        else => {
            print("{s}\nThe following command crashed:\n", .{result.stderr});
            dumpArgs(args);
            return error.ChildCrashed;
        },
    }
    return result;
}

fn getBuiltinCode(allocator: *mem.Allocator, env_map: *std.BufMap, zig_exe: []const u8) ![]const u8 {
    const result = try exec(allocator, env_map, &[_][]const u8{ zig_exe, "build-obj", "--show-builtin" });
    return result.stdout;
}

fn dumpArgs(args: []const []const u8) void {
    for (args) |arg|
        print("{s} ", .{arg})
    else
        print("\n", .{});
}
