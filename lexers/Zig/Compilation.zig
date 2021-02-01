const Compilation = @This();

const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const log = std.log.scoped(.compilation);
const Target = std.Target;

const Value = @import("value.zig").Value;
const Type = @import("type.zig").Type;
const target_util = @import("target.zig");
const Package = @import("Package.zig");
const link = @import("link.zig");
const trace = @import("tracy.zig").trace;
const liveness = @import("liveness.zig");
const build_options = @import("build_options");
const LibCInstallation = @import("libc_installation.zig").LibCInstallation;
const glibc = @import("glibc.zig");
const musl = @import("musl.zig");
const mingw = @import("mingw.zig");
const libunwind = @import("libunwind.zig");
const libcxx = @import("libcxx.zig");
const fatal = @import("main.zig").fatal;
const Module = @import("Module.zig");
const Cache = @import("Cache.zig");
const stage1 = @import("stage1.zig");
const translate_c = @import("translate_c.zig");
const c_codegen = @import("codegen/c.zig");
const ThreadPool = @import("ThreadPool.zig");
const WaitGroup = @import("WaitGroup.zig");
const libtsan = @import("libtsan.zig");

/// General-purpose allocator. Used for both temporary and long-term storage.
gpa: *Allocator,
/// Arena-allocated memory used during initialization. Should be untouched until deinit.
arena_state: std.heap.ArenaAllocator.State,
bin_file: *link.File,
c_object_table: std.AutoArrayHashMapUnmanaged(*CObject, void) = .{},
c_object_cache_digest_set: std.AutoHashMapUnmanaged(Cache.BinDigest, void) = .{},
stage1_lock: ?Cache.Lock = null,
stage1_cache_manifest: *Cache.Manifest = undefined,

link_error_flags: link.File.ErrorFlags = .{},

work_queue: std.fifo.LinearFifo(Job, .Dynamic),

/// These jobs are to invoke the Clang compiler to create an object file, which
/// gets linked with the Compilation.
c_object_work_queue: std.fifo.LinearFifo(*CObject, .Dynamic),

/// The ErrorMsg memory is owned by the `CObject`, using Compilation's general purpose allocator.
/// This data is accessed by multiple threads and is protected by `mutex`.
failed_c_objects: std.AutoArrayHashMapUnmanaged(*CObject, *CObject.ErrorMsg) = .{},

keep_source_files_loaded: bool,
use_clang: bool,
sanitize_c: bool,
/// When this is `true` it means invoking clang as a sub-process is expected to inherit
/// stdin, stdout, stderr, and if it returns non success, to forward the exit code.
/// Otherwise we attempt to parse the error messages and expose them via the Compilation API.
/// This is `true` for `zig cc`, `zig c++`, and `zig translate-c`.
clang_passthrough_mode: bool,
clang_preprocessor_mode: ClangPreprocessorMode,
/// Whether to print clang argvs to stdout.
verbose_cc: bool,
verbose_tokenize: bool,
verbose_ast: bool,
verbose_ir: bool,
verbose_llvm_ir: bool,
verbose_cimport: bool,
verbose_llvm_cpu_features: bool,
disable_c_depfile: bool,
time_report: bool,
stack_report: bool,

c_source_files: []const CSourceFile,
clang_argv: []const []const u8,
cache_parent: *Cache,
/// Path to own executable for invoking `zig clang`.
self_exe_path: ?[]const u8,
zig_lib_directory: Directory,
local_cache_directory: Directory,
global_cache_directory: Directory,
libc_include_dir_list: []const []const u8,
thread_pool: *ThreadPool,

/// Populated when we build the libc++ static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
libcxx_static_lib: ?CRTFile = null,
/// Populated when we build the libc++abi static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
libcxxabi_static_lib: ?CRTFile = null,
/// Populated when we build the libunwind static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
libunwind_static_lib: ?CRTFile = null,
/// Populated when we build the TSAN static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
tsan_static_lib: ?CRTFile = null,
/// Populated when we build the libssp static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
libssp_static_lib: ?CRTFile = null,
/// Populated when we build the libc static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
libc_static_lib: ?CRTFile = null,
/// Populated when we build the libcompiler_rt static library. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
compiler_rt_static_lib: ?CRTFile = null,
/// Populated when we build the compiler_rt_obj object. A Job to build this is placed in the queue
/// and resolved before calling linker.flush().
compiler_rt_obj: ?CRTFile = null,

glibc_so_files: ?glibc.BuiltSharedObjects = null,

/// For example `Scrt1.o` and `libc_nonshared.a`. These are populated after building libc from source,
/// The set of needed CRT (C runtime) files differs depending on the target and compilation settings.
/// The key is the basename, and the value is the absolute path to the completed build artifact.
crt_files: std.StringHashMapUnmanaged(CRTFile) = .{},

/// Keeping track of this possibly open resource so we can close it later.
owned_link_dir: ?std.fs.Dir,

/// This is for stage1 and should be deleted upon completion of self-hosting.
/// Don't use this for anything other than stage1 compatibility.
color: @import("main.zig").Color = .auto,

/// This mutex guards all `Compilation` mutable state.
mutex: std.Thread.Mutex = .{},

test_filter: ?[]const u8,
test_name_prefix: ?[]const u8,
test_evented_io: bool,
debug_compiler_runtime_libs: bool,

emit_asm: ?EmitLoc,
emit_llvm_ir: ?EmitLoc,
emit_analysis: ?EmitLoc,
emit_docs: ?EmitLoc,

work_queue_wait_group: WaitGroup,

pub const InnerError = Module.InnerError;

pub const CRTFile = struct {
    lock: Cache.Lock,
    full_object_path: []const u8,

    fn deinit(self: *CRTFile, gpa: *Allocator) void {
        self.lock.release();
        gpa.free(self.full_object_path);
        self.* = undefined;
    }
};

/// For passing to a C compiler.
pub const CSourceFile = struct {
    src_path: []const u8,
    extra_flags: []const []const u8 = &[0][]const u8{},
};

const Job = union(enum) {
    /// Write the machine code for a Decl to the output file.
    codegen_decl: *Module.Decl,
    /// Render the .h file snippet for the Decl.
    emit_h_decl: *Module.Decl,
    /// The Decl needs to be analyzed and possibly export itself.
    /// It may have already be analyzed, or it may have been determined
    /// to be outdated; in this case perform semantic analysis again.
    analyze_decl: *Module.Decl,
    /// The source file containing the Decl has been updated, and so the
    /// Decl may need its line number information updated in the debug info.
    update_line_number: *Module.Decl,

    /// one of the glibc static objects
    glibc_crt_file: glibc.CRTFile,
    /// all of the glibc shared objects
    glibc_shared_objects,
    /// one of the musl static objects
    musl_crt_file: musl.CRTFile,
    /// one of the mingw-w64 static objects
    mingw_crt_file: mingw.CRTFile,
    /// libunwind.a, usually needed when linking libc
    libunwind: void,
    libcxx: void,
    libcxxabi: void,
    libtsan: void,
    libssp: void,
    compiler_rt_lib: void,
    compiler_rt_obj: void,
    /// needed when not linking libc and using LLVM for code generation because it generates
    /// calls to, for example, memcpy and memset.
    zig_libc: void,

    /// Generate builtin.zig source code and write it into the correct place.
    generate_builtin_zig: void,
    /// Use stage1 C++ code to compile zig code into an object file.
    stage1_module: void,

    /// The value is the index into `link.File.Options.system_libs`.
    windows_import_lib: usize,
};

pub const CObject = struct {
    /// Relative to cwd. Owned by arena.
    src: CSourceFile,
    status: union(enum) {
        new,
        success: struct {
            /// The outputted result. Owned by gpa.
            object_path: []u8,
            /// This is a file system lock on the cache hash manifest representing this
            /// object. It prevents other invocations of the Zig compiler from interfering
            /// with this object until released.
            lock: Cache.Lock,
        },
        /// There will be a corresponding ErrorMsg in Compilation.failed_c_objects.
        failure,
        /// A transient failure happened when trying to compile the C Object; it may
        /// succeed if we try again. There may be a corresponding ErrorMsg in
        /// Compilation.failed_c_objects. If there is not, the failure is out of memory.
        failure_retryable,
    },

    pub const ErrorMsg = struct {
        msg: []const u8,
        line: u32,
        column: u32,

        pub fn destroy(em: *ErrorMsg, gpa: *Allocator) void {
            gpa.free(em.msg);
            gpa.destroy(em);
            em.* = undefined;
        }
    };

    /// Returns if there was failure.
    pub fn clearStatus(self: *CObject, gpa: *Allocator) bool {
        switch (self.status) {
            .new => return false,
            .failure, .failure_retryable => {
                self.status = .new;
                return true;
            },
            .success => |*success| {
                gpa.free(success.object_path);
                success.lock.release();
                self.status = .new;
                return false;
            },
        }
    }

    pub fn destroy(self: *CObject, gpa: *Allocator) void {
        _ = self.clearStatus(gpa);
        gpa.destroy(self);
    }
};

/// To support incremental compilation, errors are stored in various places
/// so that they can be created and destroyed appropriately. This structure
/// is used to collect all the errors from the various places into one
/// convenient place for API users to consume. It is allocated into 1 heap
/// and freed all at once.
pub const AllErrors = struct {
    arena: std.heap.ArenaAllocator.State,
    list: []const Message,

    pub const Message = union(enum) {
        src: struct {
            src_path: []const u8,
            line: usize,
            column: usize,
            byte_offset: usize,
            msg: []const u8,
            notes: []Message = &.{},
        },
        plain: struct {
            msg: []const u8,
        },

        pub fn renderToStdErr(msg: Message) void {
            return msg.renderToStdErrInner("error");
        }

        fn renderToStdErrInner(msg: Message, kind: []const u8) void {
            switch (msg) {
                .src => |src| {
                    std.debug.print("{s}:{d}:{d}: {s}: {s}\n", .{
                        src.src_path,
                        src.line + 1,
                        src.column + 1,
                        kind,
                        src.msg,
                    });
                    for (src.notes) |note| {
                        note.renderToStdErrInner("note");
                    }
                },
                .plain => |plain| {
                    std.debug.print("{s}: {s}\n", .{ kind, plain.msg });
                },
            }
        }
    };

    pub fn deinit(self: *AllErrors, gpa: *Allocator) void {
        self.arena.promote(gpa).deinit();
    }

    fn add(
        module: *Module,
        arena: *std.heap.ArenaAllocator,
        errors: *std.ArrayList(Message),
        module_err_msg: Module.ErrorMsg,
    ) !void {
        const notes = try arena.allocator.alloc(Message, module_err_msg.notes.len);
        for (notes) |*note, i| {
            const module_note = module_err_msg.notes[i];
            const source = try module_note.src_loc.file_scope.getSource(module);
            const loc = std.zig.findLineColumn(source, module_note.src_loc.byte_offset);
            const sub_file_path = module_note.src_loc.file_scope.sub_file_path;
            note.* = .{
                .src = .{
                    .src_path = try arena.allocator.dupe(u8, sub_file_path),
                    .msg = try arena.allocator.dupe(u8, module_note.msg),
                    .byte_offset = module_note.src_loc.byte_offset,
                    .line = loc.line,
                    .column = loc.column,
                },
            };
        }
        const source = try module_err_msg.src_loc.file_scope.getSource(module);
        const loc = std.zig.findLineColumn(source, module_err_msg.src_loc.byte_offset);
        const sub_file_path = module_err_msg.src_loc.file_scope.sub_file_path;
        try errors.append(.{
            .src = .{
                .src_path = try arena.allocator.dupe(u8, sub_file_path),
                .msg = try arena.allocator.dupe(u8, module_err_msg.msg),
                .byte_offset = module_err_msg.src_loc.byte_offset,
                .line = loc.line,
                .column = loc.column,
                .notes = notes,
            },
        });
    }

    fn addPlain(
        arena: *std.heap.ArenaAllocator,
        errors: *std.ArrayList(Message),
        msg: []const u8,
    ) !void {
        try errors.append(.{ .plain = .{ .msg = msg } });
    }
};

pub const Directory = struct {
    /// This field is redundant for operations that can act on the open directory handle
    /// directly, but it is needed when passing the directory to a child process.
    /// `null` means cwd.
    path: ?[]const u8,
    handle: std.fs.Dir,

    pub fn join(self: Directory, allocator: *Allocator, paths: []const []const u8) ![]u8 {
        if (self.path) |p| {
            // TODO clean way to do this with only 1 allocation
            const part2 = try std.fs.path.join(allocator, paths);
            defer allocator.free(part2);
            return std.fs.path.join(allocator, &[_][]const u8{ p, part2 });
        } else {
            return std.fs.path.join(allocator, paths);
        }
    }
};

pub const EmitLoc = struct {
    /// If this is `null` it means the file will be output to the cache directory.
    /// When provided, both the open file handle and the path name must outlive the `Compilation`.
    directory: ?Compilation.Directory,
    /// This may not have sub-directories in it.
    basename: []const u8,
};

pub const ClangPreprocessorMode = enum {
    no,
    /// This means we are doing `zig cc -E -o <path>`.
    yes,
    /// This means we are doing `zig cc -E`.
    stdout,
};

pub const InitOptions = struct {
    zig_lib_directory: Directory,
    local_cache_directory: Directory,
    global_cache_directory: Directory,
    target: Target,
    root_name: []const u8,
    root_pkg: ?*Package,
    output_mode: std.builtin.OutputMode,
    thread_pool: *ThreadPool,
    dynamic_linker: ?[]const u8 = null,
    /// `null` means to not emit a binary file.
    emit_bin: ?EmitLoc,
    /// `null` means to not emit a C header file.
    emit_h: ?EmitLoc = null,
    /// `null` means to not emit assembly.
    emit_asm: ?EmitLoc = null,
    /// `null` means to not emit LLVM IR.
    emit_llvm_ir: ?EmitLoc = null,
    /// `null` means to not emit semantic analysis JSON.
    emit_analysis: ?EmitLoc = null,
    /// `null` means to not emit docs.
    emit_docs: ?EmitLoc = null,
    link_mode: ?std.builtin.LinkMode = null,
    dll_export_fns: ?bool = false,
    /// Normally when using LLD to link, Zig uses a file named "lld.id" in the
    /// same directory as the output binary which contains the hash of the link
    /// operation, allowing Zig to skip linking when the hash would be unchanged.
    /// In the case that the output binary is being emitted into a directory which
    /// is externally modified - essentially anything other than zig-cache - then
    /// this flag would be set to disable this machinery to avoid false positives.
    disable_lld_caching: bool = false,
    object_format: ?std.builtin.ObjectFormat = null,
    optimize_mode: std.builtin.Mode = .Debug,
    keep_source_files_loaded: bool = false,
    clang_argv: []const []const u8 = &[0][]const u8{},
    lld_argv: []const []const u8 = &[0][]const u8{},
    lib_dirs: []const []const u8 = &[0][]const u8{},
    rpath_list: []const []const u8 = &[0][]const u8{},
    c_source_files: []const CSourceFile = &[0]CSourceFile{},
    link_objects: []const []const u8 = &[0][]const u8{},
    framework_dirs: []const []const u8 = &[0][]const u8{},
    frameworks: []const []const u8 = &[0][]const u8{},
    system_libs: []const []const u8 = &[0][]const u8{},
    link_libc: bool = false,
    link_libcpp: bool = false,
    want_pic: ?bool = null,
    /// This means that if the output mode is an executable it will be a
    /// Position Independent Executable. If the output mode is not an
    /// executable this field is ignored.
    want_pie: ?bool = null,
    want_sanitize_c: ?bool = null,
    want_stack_check: ?bool = null,
    want_red_zone: ?bool = null,
    want_valgrind: ?bool = null,
    want_tsan: ?bool = null,
    want_compiler_rt: ?bool = null,
    use_llvm: ?bool = null,
    use_lld: ?bool = null,
    use_clang: ?bool = null,
    rdynamic: bool = false,
    strip: bool = false,
    single_threaded: bool = false,
    function_sections: bool = false,
    is_native_os: bool,
    is_native_abi: bool,
    time_report: bool = false,
    stack_report: bool = false,
    link_eh_frame_hdr: bool = false,
    link_emit_relocs: bool = false,
    linker_script: ?[]const u8 = null,
    version_script: ?[]const u8 = null,
    soname: ?[]const u8 = null,
    linker_gc_sections: ?bool = null,
    linker_allow_shlib_undefined: ?bool = null,
    linker_bind_global_refs_locally: ?bool = null,
    each_lib_rpath: ?bool = null,
    disable_c_depfile: bool = false,
    linker_z_nodelete: bool = false,
    linker_z_defs: bool = false,
    clang_passthrough_mode: bool = false,
    verbose_cc: bool = false,
    verbose_link: bool = false,
    verbose_tokenize: bool = false,
    verbose_ast: bool = false,
    verbose_ir: bool = false,
    verbose_llvm_ir: bool = false,
    verbose_cimport: bool = false,
    verbose_llvm_cpu_features: bool = false,
    is_test: bool = false,
    test_evented_io: bool = false,
    debug_compiler_runtime_libs: bool = false,
    /// Normally when you create a `Compilation`, Zig will automatically build
    /// and link in required dependencies, such as compiler-rt and libc. When
    /// building such dependencies themselves, this flag must be set to avoid
    /// infinite recursion.
    skip_linker_dependencies: bool = false,
    parent_compilation_link_libc: bool = false,
    stack_size_override: ?u64 = null,
    image_base_override: ?u64 = null,
    self_exe_path: ?[]const u8 = null,
    version: ?std.builtin.Version = null,
    libc_installation: ?*const LibCInstallation = null,
    machine_code_model: std.builtin.CodeModel = .default,
    clang_preprocessor_mode: ClangPreprocessorMode = .no,
    /// This is for stage1 and should be deleted upon completion of self-hosting.
    color: @import("main.zig").Color = .auto,
    test_filter: ?[]const u8 = null,
    test_name_prefix: ?[]const u8 = null,
    subsystem: ?std.Target.SubSystem = null,
};

fn addPackageTableToCacheHash(
    hash: *Cache.HashHelper,
    arena: *std.heap.ArenaAllocator,
    pkg_table: Package.Table,
    hash_type: union(enum) { path_bytes, files: *Cache.Manifest },
) (error{OutOfMemory} || std.os.GetCwdError)!void {
    const allocator = &arena.allocator;

    const packages = try allocator.alloc(Package.Table.Entry, pkg_table.count());
    {
        // Copy over the hashmap entries to our slice
        var table_it = pkg_table.iterator();
        var idx: usize = 0;
        while (table_it.next()) |entry| : (idx += 1) {
            packages[idx] = entry.*;
        }
    }
    // Sort the slice by package name
    std.sort.sort(Package.Table.Entry, packages, {}, struct {
        fn lessThan(_: void, lhs: Package.Table.Entry, rhs: Package.Table.Entry) bool {
            return std.mem.lessThan(u8, lhs.key, rhs.key);
        }
    }.lessThan);

    for (packages) |pkg| {
        // Finally insert the package name and path to the cache hash.
        hash.addBytes(pkg.key);
        switch (hash_type) {
            .path_bytes => {
                hash.addBytes(pkg.value.root_src_path);
                hash.addOptionalBytes(pkg.value.root_src_directory.path);
            },
            .files => |man| {
                const pkg_zig_file = try pkg.value.root_src_directory.join(allocator, &[_][]const u8{
                    pkg.value.root_src_path,
                });
                _ = try man.addFile(pkg_zig_file, null);
            },
        }
        // Recurse to handle the package's dependencies
        try addPackageTableToCacheHash(hash, arena, pkg.value.table, hash_type);
    }
}

pub fn create(gpa: *Allocator, options: InitOptions) !*Compilation {
    const is_dyn_lib = switch (options.output_mode) {
        .Obj, .Exe => false,
        .Lib => (options.link_mode orelse .Static) == .Dynamic,
    };
    const is_exe_or_dyn_lib = switch (options.output_mode) {
        .Obj => false,
        .Lib => is_dyn_lib,
        .Exe => true,
    };
    const needs_c_symbols = !options.skip_linker_dependencies and
        (is_exe_or_dyn_lib or (options.target.isWasm() and options.output_mode != .Obj));

    const comp: *Compilation = comp: {
        // For allocations that have the same lifetime as Compilation. This arena is used only during this
        // initialization and then is freed in deinit().
        var arena_allocator = std.heap.ArenaAllocator.init(gpa);
        errdefer arena_allocator.deinit();
        const arena = &arena_allocator.allocator;

        // We put the `Compilation` itself in the arena. Freeing the arena will free the module.
        // It's initialized later after we prepare the initialization options.
        const comp = try arena.create(Compilation);
        const root_name = try arena.dupe(u8, options.root_name);

        const ofmt = options.object_format orelse options.target.getObjectFormat();

        // Make a decision on whether to use LLVM or our own backend.
        const use_llvm = if (options.use_llvm) |explicit| explicit else blk: {
            // If we have no zig code to compile, no need for LLVM.
            if (options.root_pkg == null)
                break :blk false;

            // If we are outputting .c code we must use Zig backend.
            if (ofmt == .c)
                break :blk false;

            // If we are the stage1 compiler, we depend on the stage1 c++ llvm backend
            // to compile zig code.
            if (build_options.is_stage1)
                break :blk true;

            // We would want to prefer LLVM for release builds when it is available, however
            // we don't have an LLVM backend yet :)
            // We would also want to prefer LLVM for architectures that we don't have self-hosted support for too.
            break :blk false;
        };
        if (!use_llvm and options.machine_code_model != .default) {
            return error.MachineCodeModelNotSupported;
        }

        // Make a decision on whether to use LLD or our own linker.
        const use_lld = if (options.use_lld) |explicit| explicit else blk: {
            if (!build_options.have_llvm)
                break :blk false;

            if (ofmt == .c)
                break :blk false;

            // Our linker can't handle objects or most advanced options yet.
            if (options.link_objects.len != 0 or
                options.c_source_files.len != 0 or
                options.frameworks.len != 0 or
                options.system_libs.len != 0 or
                options.link_libc or options.link_libcpp or
                options.link_eh_frame_hdr or
                options.link_emit_relocs or
                options.output_mode == .Lib or
                options.lld_argv.len != 0 or
                options.image_base_override != null or
                options.linker_script != null or options.version_script != null)
            {
                break :blk true;
            }

            if (use_llvm) {
                // If stage1 generates an object file, self-hosted linker is not
                // yet sophisticated enough to handle that.
                break :blk options.root_pkg != null;
            }

            break :blk false;
        };

        const DarwinOptions = struct {
            syslibroot: ?[]const u8 = null,
            system_linker_hack: bool = false,
        };

        const darwin_options: DarwinOptions = if (build_options.have_llvm and comptime std.Target.current.isDarwin()) outer: {
            const opts: DarwinOptions = if (use_lld and options.is_native_os and options.target.isDarwin()) inner: {
                // TODO Revisit this targeting versions lower than macOS 11 when LLVM 12 is out.
                // See https://github.com/ziglang/zig/issues/6996
                const at_least_big_sur = options.target.os.getVersionRange().semver.min.major >= 11;
                const syslibroot = if (at_least_big_sur) try std.zig.system.getSDKPath(arena) else null;
                const system_linker_hack = std.os.getenv("ZIG_SYSTEM_LINKER_HACK") != null;
                break :inner .{
                    .syslibroot = syslibroot,
                    .system_linker_hack = system_linker_hack,
                };
            } else .{};
            break :outer opts;
        } else .{};

        const tsan = options.want_tsan orelse false;

        const link_libc = options.link_libc or target_util.osRequiresLibC(options.target) or tsan;

        const must_dynamic_link = dl: {
            if (target_util.cannotDynamicLink(options.target))
                break :dl false;
            if (is_exe_or_dyn_lib and link_libc and
                (options.target.isGnuLibC() or target_util.osRequiresLibC(options.target)))
            {
                break :dl true;
            }
            const any_dyn_libs: bool = x: {
                if (options.system_libs.len != 0)
                    break :x true;
                for (options.link_objects) |obj| {
                    switch (classifyFileExt(obj)) {
                        .shared_library => break :x true,
                        else => continue,
                    }
                }
                break :x false;
            };
            if (any_dyn_libs) {
                // When creating a executable that links to system libraries,
                // we require dynamic linking, but we must not link static libraries
                // or object files dynamically!
                break :dl (options.output_mode == .Exe);
            }

            break :dl false;
        };
        const default_link_mode: std.builtin.LinkMode = blk: {
            if (must_dynamic_link) {
                break :blk .Dynamic;
            } else if (is_exe_or_dyn_lib and link_libc and
                options.is_native_abi and options.target.abi.isMusl())
            {
                // If targeting the system's native ABI and the system's
                // libc is musl, link dynamically by default.
                break :blk .Dynamic;
            } else {
                break :blk .Static;
            }
        };
        const link_mode: std.builtin.LinkMode = if (options.link_mode) |lm| blk: {
            if (lm == .Static and must_dynamic_link) {
                return error.UnableToStaticLink;
            }
            break :blk lm;
        } else default_link_mode;

        const dll_export_fns = if (options.dll_export_fns) |explicit| explicit else is_dyn_lib;

        const libc_dirs = try detectLibCIncludeDirs(
            arena,
            options.zig_lib_directory.path.?,
            options.target,
            options.is_native_os,
            link_libc,
            options.libc_installation,
        );

        const must_pie = target_util.requiresPIE(options.target);
        const pie: bool = if (options.want_pie) |explicit| pie: {
            if (!explicit and must_pie) {
                return error.TargetRequiresPIE;
            }
            break :pie explicit;
        } else must_pie or tsan;

        const must_pic: bool = b: {
            if (target_util.requiresPIC(options.target, link_libc))
                break :b true;
            break :b link_mode == .Dynamic;
        };
        const pic = if (options.want_pic) |explicit| pic: {
            if (!explicit) {
                if (must_pic) {
                    return error.TargetRequiresPIC;
                }
                if (pie) {
                    return error.PIERequiresPIC;
                }
            }
            break :pic explicit;
        } else pie or must_pic;

        // TSAN is implemented in C++ so it requires linking libc++.
        const link_libcpp = options.link_libcpp or tsan;

        // Make a decision on whether to use Clang for translate-c and compiling C files.
        const use_clang = if (options.use_clang) |explicit| explicit else blk: {
            if (build_options.have_llvm) {
                // Can't use it if we don't have it!
                break :blk false;
            }
            // It's not planned to do our own translate-c or C compilation.
            break :blk true;
        };

        const is_safe_mode = switch (options.optimize_mode) {
            .Debug, .ReleaseSafe => true,
            .ReleaseFast, .ReleaseSmall => false,
        };

        const sanitize_c = options.want_sanitize_c orelse is_safe_mode;

        const stack_check: bool = b: {
            if (!target_util.supportsStackProbing(options.target))
                break :b false;
            break :b options.want_stack_check orelse is_safe_mode;
        };

        const valgrind: bool = b: {
            if (!target_util.hasValgrindSupport(options.target))
                break :b false;
            break :b options.want_valgrind orelse (options.optimize_mode == .Debug);
        };

        const include_compiler_rt = options.want_compiler_rt orelse needs_c_symbols;

        const single_threaded = options.single_threaded or target_util.isSingleThreaded(options.target);

        const llvm_cpu_features: ?[*:0]const u8 = if (build_options.have_llvm and use_llvm) blk: {
            var buf = std.ArrayList(u8).init(arena);
            for (options.target.cpu.arch.allFeaturesList()) |feature, index_usize| {
                const index = @intCast(Target.Cpu.Feature.Set.Index, index_usize);
                const is_enabled = options.target.cpu.features.isEnabled(index);

                if (feature.llvm_name) |llvm_name| {
                    const plus_or_minus = "-+"[@boolToInt(is_enabled)];
                    try buf.ensureCapacity(buf.items.len + 2 + llvm_name.len);
                    buf.appendAssumeCapacity(plus_or_minus);
                    buf.appendSliceAssumeCapacity(llvm_name);
                    buf.appendSliceAssumeCapacity(",");
                }
            }
            assert(mem.endsWith(u8, buf.items, ","));
            buf.items[buf.items.len - 1] = 0;
            buf.shrinkAndFree(buf.items.len);
            break :blk buf.items[0 .. buf.items.len - 1 :0].ptr;
        } else null;

        const strip = options.strip or !target_util.hasDebugInfo(options.target);
        const red_zone = options.want_red_zone orelse target_util.hasRedZone(options.target);

        // We put everything into the cache hash that *cannot be modified during an incremental update*.
        // For example, one cannot change the target between updates, but one can change source files,
        // so the target goes into the cache hash, but source files do not. This is so that we can
        // find the same binary and incrementally update it even if there are modified source files.
        // We do this even if outputting to the current directory because we need somewhere to store
        // incremental compilation metadata.
        const cache = try arena.create(Cache);
        cache.* = .{
            .gpa = gpa,
            .manifest_dir = try options.local_cache_directory.handle.makeOpenPath("h", .{}),
        };
        errdefer cache.manifest_dir.close();

        // This is shared hasher state common to zig source and all C source files.
        cache.hash.addBytes(build_options.version);
        cache.hash.addBytes(options.zig_lib_directory.path orelse ".");
        cache.hash.add(options.optimize_mode);
        cache.hash.add(options.target.cpu.arch);
        cache.hash.addBytes(options.target.cpu.model.name);
        cache.hash.add(options.target.cpu.features.ints);
        cache.hash.add(options.target.os.tag);
        cache.hash.add(options.target.os.getVersionRange());
        cache.hash.add(options.is_native_os);
        cache.hash.add(options.target.abi);
        cache.hash.add(ofmt);
        cache.hash.add(pic);
        cache.hash.add(pie);
        cache.hash.add(tsan);
        cache.hash.add(stack_check);
        cache.hash.add(red_zone);
        cache.hash.add(link_mode);
        cache.hash.add(options.function_sections);
        cache.hash.add(strip);
        cache.hash.add(link_libc);
        cache.hash.add(link_libcpp);
        cache.hash.add(options.output_mode);
        cache.hash.add(options.machine_code_model);
        cache.hash.addOptionalEmitLoc(options.emit_bin);
        cache.hash.addBytes(options.root_name);
        // TODO audit this and make sure everything is in it

        const module: ?*Module = if (options.root_pkg) |root_pkg| blk: {
            // Options that are specific to zig source files, that cannot be
            // modified between incremental updates.
            var hash = cache.hash;

            // Here we put the root source file path name, but *not* with addFile. We want the
            // hash to be the same regardless of the contents of the source file, because
            // incremental compilation will handle it, but we do want to namespace different
            // source file names because they are likely different compilations and therefore this
            // would be likely to cause cache hits.
            hash.addBytes(root_pkg.root_src_path);
            hash.addOptionalBytes(root_pkg.root_src_directory.path);
            {
                var local_arena = std.heap.ArenaAllocator.init(gpa);
                defer local_arena.deinit();
                try addPackageTableToCacheHash(&hash, &local_arena, root_pkg.table, .path_bytes);
            }
            hash.add(valgrind);
            hash.add(single_threaded);
            hash.add(dll_export_fns);
            hash.add(options.is_test);
            hash.add(options.skip_linker_dependencies);
            hash.add(options.parent_compilation_link_libc);

            const digest = hash.final();
            const artifact_sub_dir = try std.fs.path.join(arena, &[_][]const u8{ "o", &digest });
            var artifact_dir = try options.local_cache_directory.handle.makeOpenPath(artifact_sub_dir, .{});
            errdefer artifact_dir.close();
            const zig_cache_artifact_directory: Directory = .{
                .handle = artifact_dir,
                .path = if (options.local_cache_directory.path) |p|
                    try std.fs.path.join(arena, &[_][]const u8{ p, artifact_sub_dir })
                else
                    artifact_sub_dir,
            };

            // TODO when we implement serialization and deserialization of incremental compilation metadata,
            // this is where we would load it. We have open a handle to the directory where
            // the output either already is, or will be.
            // However we currently do not have serialization of such metadata, so for now
            // we set up an empty Module that does the entire compilation fresh.

            const root_scope = rs: {
                if (mem.endsWith(u8, root_pkg.root_src_path, ".zig")) {
                    const root_scope = try gpa.create(Module.Scope.File);
                    const struct_ty = try Type.Tag.empty_struct.create(
                        gpa,
                        &root_scope.root_container,
                    );
                    root_scope.* = .{
                        // TODO this is duped so it can be freed in Container.deinit
                        .sub_file_path = try gpa.dupe(u8, root_pkg.root_src_path),
                        .source = .{ .unloaded = {} },
                        .contents = .{ .not_available = {} },
                        .status = .never_loaded,
                        .pkg = root_pkg,
                        .root_container = .{
                            .file_scope = root_scope,
                            .decls = .{},
                            .ty = struct_ty,
                        },
                    };
                    break :rs root_scope;
                } else if (mem.endsWith(u8, root_pkg.root_src_path, ".zir")) {
                    return error.ZirFilesUnsupported;
                } else {
                    unreachable;
                }
            };

            const module = try arena.create(Module);
            module.* = .{
                .gpa = gpa,
                .comp = comp,
                .root_pkg = root_pkg,
                .root_scope = root_scope,
                .zig_cache_artifact_directory = zig_cache_artifact_directory,
                .emit_h = options.emit_h,
            };
            break :blk module;
        } else blk: {
            if (options.emit_h != null) return error.NoZigModuleForCHeader;
            break :blk null;
        };
        errdefer if (module) |zm| zm.deinit();

        const error_return_tracing = !strip and switch (options.optimize_mode) {
            .Debug, .ReleaseSafe => true,
            .ReleaseFast, .ReleaseSmall => false,
        };

        // For resource management purposes.
        var owned_link_dir: ?std.fs.Dir = null;
        errdefer if (owned_link_dir) |*dir| dir.close();

        const bin_file_emit: ?link.Emit = blk: {
            const emit_bin = options.emit_bin orelse break :blk null;
            if (emit_bin.directory) |directory| {
                break :blk link.Emit{
                    .directory = directory,
                    .sub_path = emit_bin.basename,
                };
            }
            if (module) |zm| {
                break :blk link.Emit{
                    .directory = zm.zig_cache_artifact_directory,
                    .sub_path = emit_bin.basename,
                };
            }
            // We could use the cache hash as is no problem, however, we increase
            // the likelihood of cache hits by adding the first C source file
            // path name (not contents) to the hash. This way if the user is compiling
            // foo.c and bar.c as separate compilations, they get different cache
            // directories.
            var hash = cache.hash;
            if (options.c_source_files.len >= 1) {
                hash.addBytes(options.c_source_files[0].src_path);
            } else if (options.link_objects.len >= 1) {
                hash.addBytes(options.link_objects[0]);
            }

            const digest = hash.final();
            const artifact_sub_dir = try std.fs.path.join(arena, &[_][]const u8{ "o", &digest });
            var artifact_dir = try options.local_cache_directory.handle.makeOpenPath(artifact_sub_dir, .{});
            owned_link_dir = artifact_dir;
            const link_artifact_directory: Directory = .{
                .handle = artifact_dir,
                .path = try options.local_cache_directory.join(arena, &[_][]const u8{artifact_sub_dir}),
            };
            break :blk link.Emit{
                .directory = link_artifact_directory,
                .sub_path = emit_bin.basename,
            };
        };

        var system_libs: std.StringArrayHashMapUnmanaged(void) = .{};
        errdefer system_libs.deinit(gpa);
        try system_libs.ensureCapacity(gpa, options.system_libs.len);
        for (options.system_libs) |lib_name| {
            system_libs.putAssumeCapacity(lib_name, {});
        }

        const bin_file = try link.File.openPath(gpa, .{
            .emit = bin_file_emit,
            .root_name = root_name,
            .module = module,
            .target = options.target,
            .dynamic_linker = options.dynamic_linker,
            .output_mode = options.output_mode,
            .link_mode = link_mode,
            .object_format = ofmt,
            .optimize_mode = options.optimize_mode,
            .use_lld = use_lld,
            .use_llvm = use_llvm,
            .system_linker_hack = darwin_options.system_linker_hack,
            .link_libc = link_libc,
            .link_libcpp = link_libcpp,
            .objects = options.link_objects,
            .frameworks = options.frameworks,
            .framework_dirs = options.framework_dirs,
            .system_libs = system_libs,
            .syslibroot = darwin_options.syslibroot,
            .lib_dirs = options.lib_dirs,
            .rpath_list = options.rpath_list,
            .strip = strip,
            .is_native_os = options.is_native_os,
            .is_native_abi = options.is_native_abi,
            .function_sections = options.function_sections,
            .allow_shlib_undefined = options.linker_allow_shlib_undefined,
            .bind_global_refs_locally = options.linker_bind_global_refs_locally orelse false,
            .z_nodelete = options.linker_z_nodelete,
            .z_defs = options.linker_z_defs,
            .stack_size_override = options.stack_size_override,
            .image_base_override = options.image_base_override,
            .include_compiler_rt = include_compiler_rt,
            .linker_script = options.linker_script,
            .version_script = options.version_script,
            .gc_sections = options.linker_gc_sections,
            .eh_frame_hdr = options.link_eh_frame_hdr,
            .emit_relocs = options.link_emit_relocs,
            .rdynamic = options.rdynamic,
            .extra_lld_args = options.lld_argv,
            .soname = options.soname,
            .version = options.version,
            .libc_installation = libc_dirs.libc_installation,
            .pic = pic,
            .pie = pie,
            .valgrind = valgrind,
            .tsan = tsan,
            .stack_check = stack_check,
            .red_zone = red_zone,
            .single_threaded = single_threaded,
            .verbose_link = options.verbose_link,
            .machine_code_model = options.machine_code_model,
            .dll_export_fns = dll_export_fns,
            .error_return_tracing = error_return_tracing,
            .llvm_cpu_features = llvm_cpu_features,
            .skip_linker_dependencies = options.skip_linker_dependencies,
            .parent_compilation_link_libc = options.parent_compilation_link_libc,
            .each_lib_rpath = options.each_lib_rpath orelse options.is_native_os,
            .disable_lld_caching = options.disable_lld_caching,
            .subsystem = options.subsystem,
            .is_test = options.is_test,
        });
        errdefer bin_file.destroy();
        comp.* = .{
            .gpa = gpa,
            .arena_state = arena_allocator.state,
            .zig_lib_directory = options.zig_lib_directory,
            .local_cache_directory = options.local_cache_directory,
            .global_cache_directory = options.global_cache_directory,
            .bin_file = bin_file,
            .emit_asm = options.emit_asm,
            .emit_llvm_ir = options.emit_llvm_ir,
            .emit_analysis = options.emit_analysis,
            .emit_docs = options.emit_docs,
            .work_queue = std.fifo.LinearFifo(Job, .Dynamic).init(gpa),
            .c_object_work_queue = std.fifo.LinearFifo(*CObject, .Dynamic).init(gpa),
            .keep_source_files_loaded = options.keep_source_files_loaded,
            .use_clang = use_clang,
            .clang_argv = options.clang_argv,
            .c_source_files = options.c_source_files,
            .cache_parent = cache,
            .self_exe_path = options.self_exe_path,
            .libc_include_dir_list = libc_dirs.libc_include_dir_list,
            .sanitize_c = sanitize_c,
            .thread_pool = options.thread_pool,
            .clang_passthrough_mode = options.clang_passthrough_mode,
            .clang_preprocessor_mode = options.clang_preprocessor_mode,
            .verbose_cc = options.verbose_cc,
            .verbose_tokenize = options.verbose_tokenize,
            .verbose_ast = options.verbose_ast,
            .verbose_ir = options.verbose_ir,
            .verbose_llvm_ir = options.verbose_llvm_ir,
            .verbose_cimport = options.verbose_cimport,
            .verbose_llvm_cpu_features = options.verbose_llvm_cpu_features,
            .disable_c_depfile = options.disable_c_depfile,
            .owned_link_dir = owned_link_dir,
            .color = options.color,
            .time_report = options.time_report,
            .stack_report = options.stack_report,
            .test_filter = options.test_filter,
            .test_name_prefix = options.test_name_prefix,
            .test_evented_io = options.test_evented_io,
            .debug_compiler_runtime_libs = options.debug_compiler_runtime_libs,
            .work_queue_wait_group = undefined,
        };
        break :comp comp;
    };
    errdefer comp.destroy();

    try comp.work_queue_wait_group.init();
    errdefer comp.work_queue_wait_group.deinit();

    if (comp.bin_file.options.module) |mod| {
        try comp.work_queue.writeItem(.{ .generate_builtin_zig = {} });
    }

    // Add a `CObject` for each `c_source_files`.
    try comp.c_object_table.ensureCapacity(gpa, options.c_source_files.len);
    for (options.c_source_files) |c_source_file| {
        const c_object = try gpa.create(CObject);
        errdefer gpa.destroy(c_object);

        c_object.* = .{
            .status = .{ .new = {} },
            .src = c_source_file,
        };
        comp.c_object_table.putAssumeCapacityNoClobber(c_object, {});
    }

    if (comp.bin_file.options.emit != null and !comp.bin_file.options.skip_linker_dependencies) {
        // If we need to build glibc for the target, add work items for it.
        // We go through the work queue so that building can be done in parallel.
        if (comp.wantBuildGLibCFromSource()) {
            try comp.addBuildingGLibCJobs();
        }
        if (comp.wantBuildMuslFromSource()) {
            try comp.work_queue.ensureUnusedCapacity(6);
            if (target_util.libc_needs_crti_crtn(comp.getTarget())) {
                comp.work_queue.writeAssumeCapacity(&[_]Job{
                    .{ .musl_crt_file = .crti_o },
                    .{ .musl_crt_file = .crtn_o },
                });
            }
            comp.work_queue.writeAssumeCapacity(&[_]Job{
                .{ .musl_crt_file = .crt1_o },
                .{ .musl_crt_file = .scrt1_o },
                .{ .musl_crt_file = .rcrt1_o },
                switch (comp.bin_file.options.link_mode) {
                    .Static => .{ .musl_crt_file = .libc_a },
                    .Dynamic => .{ .musl_crt_file = .libc_so },
                },
            });
        }
        if (comp.wantBuildMinGWFromSource()) {
            const static_lib_jobs = [_]Job{
                .{ .mingw_crt_file = .mingw32_lib },
                .{ .mingw_crt_file = .msvcrt_os_lib },
                .{ .mingw_crt_file = .mingwex_lib },
                .{ .mingw_crt_file = .uuid_lib },
            };
            const crt_job: Job = .{ .mingw_crt_file = if (is_dyn_lib) .dllcrt2_o else .crt2_o };
            try comp.work_queue.ensureUnusedCapacity(static_lib_jobs.len + 1);
            comp.work_queue.writeAssumeCapacity(&static_lib_jobs);
            comp.work_queue.writeItemAssumeCapacity(crt_job);

            // When linking mingw-w64 there are some import libs we always need.
            for (mingw.always_link_libs) |name| {
                try comp.bin_file.options.system_libs.put(comp.gpa, name, .{});
            }
        }
        // Generate Windows import libs.
        if (comp.getTarget().os.tag == .windows) {
            const count = comp.bin_file.options.system_libs.count();
            try comp.work_queue.ensureUnusedCapacity(count);
            var i: usize = 0;
            while (i < count) : (i += 1) {
                comp.work_queue.writeItemAssumeCapacity(.{ .windows_import_lib = i });
            }
        }
        if (comp.wantBuildLibUnwindFromSource()) {
            try comp.work_queue.writeItem(.{ .libunwind = {} });
        }
        if (build_options.have_llvm and is_exe_or_dyn_lib and comp.bin_file.options.link_libcpp) {
            try comp.work_queue.writeItem(.libcxx);
            try comp.work_queue.writeItem(.libcxxabi);
        }
        if (build_options.have_llvm and comp.bin_file.options.tsan) {
            try comp.work_queue.writeItem(.libtsan);
        }

        // The `is_stage1` condition is here only because stage2 cannot yet build compiler-rt.
        // Once it is capable this condition should be removed.
        if (build_options.is_stage1) {
            if (comp.bin_file.options.include_compiler_rt) {
                if (is_exe_or_dyn_lib or comp.getTarget().isWasm()) {
                    try comp.work_queue.writeItem(.{ .compiler_rt_lib = {} });
                } else {
                    try comp.work_queue.writeItem(.{ .compiler_rt_obj = {} });
                    if (comp.bin_file.options.object_format != .elf and
                        comp.bin_file.options.output_mode == .Obj)
                    {
                        // For ELF we can rely on using -r to link multiple objects together into one,
                        // but to truly support `build-obj -fcompiler-rt` will require virtually
                        // injecting `_ = @import("compiler_rt.zig")` into the root source file of
                        // the compilation.
                        fatal("Embedding compiler-rt into {s} objects is not yet implemented.", .{
                            @tagName(comp.bin_file.options.object_format),
                        });
                    }
                }
            }
            if (needs_c_symbols) {
                // MinGW provides no libssp, use our own implementation.
                if (comp.getTarget().isMinGW()) {
                    try comp.work_queue.writeItem(.{ .libssp = {} });
                }
                if (!comp.bin_file.options.link_libc) {
                    try comp.work_queue.writeItem(.{ .zig_libc = {} });
                }
            }
        }
    }

    if (build_options.is_stage1 and comp.bin_file.options.use_llvm) {
        try comp.work_queue.writeItem(.{ .stage1_module = {} });
    }

    return comp;
}

fn releaseStage1Lock(comp: *Compilation) void {
    if (comp.stage1_lock) |*lock| {
        lock.release();
        comp.stage1_lock = null;
    }
}

pub fn destroy(self: *Compilation) void {
    const optional_module = self.bin_file.options.module;
    self.bin_file.destroy();
    if (optional_module) |module| module.deinit();

    self.releaseStage1Lock();

    const gpa = self.gpa;
    self.work_queue.deinit();
    self.c_object_work_queue.deinit();

    {
        var it = self.crt_files.iterator();
        while (it.next()) |entry| {
            gpa.free(entry.key);
            entry.value.deinit(gpa);
        }
        self.crt_files.deinit(gpa);
    }

    if (self.libunwind_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }
    if (self.libcxx_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }
    if (self.libcxxabi_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }
    if (self.compiler_rt_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }
    if (self.libssp_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }
    if (self.libc_static_lib) |*crt_file| {
        crt_file.deinit(gpa);
    }

    if (self.glibc_so_files) |*glibc_file| {
        glibc_file.deinit(gpa);
    }

    for (self.c_object_table.items()) |entry| {
        entry.key.destroy(gpa);
    }
    self.c_object_table.deinit(gpa);
    self.c_object_cache_digest_set.deinit(gpa);

    for (self.failed_c_objects.items()) |entry| {
        entry.value.destroy(gpa);
    }
    self.failed_c_objects.deinit(gpa);

    self.cache_parent.manifest_dir.close();
    if (self.owned_link_dir) |*dir| dir.close();

    self.work_queue_wait_group.deinit();

    // This destroys `self`.
    self.arena_state.promote(gpa).deinit();
}

pub fn getTarget(self: Compilation) Target {
    return self.bin_file.options.target;
}

/// Detect changes to source files, perform semantic analysis, and update the output files.
pub fn update(self: *Compilation) !void {
    const tracy = trace(@src());
    defer tracy.end();

    self.c_object_cache_digest_set.clearRetainingCapacity();

    // For compiling C objects, we rely on the cache hash system to avoid duplicating work.
    // Add a Job for each C object.
    try self.c_object_work_queue.ensureUnusedCapacity(self.c_object_table.items().len);
    for (self.c_object_table.items()) |entry| {
        self.c_object_work_queue.writeItemAssumeCapacity(entry.key);
    }

    const use_stage1 = build_options.is_stage1 and self.bin_file.options.use_llvm;
    if (!use_stage1) {
        if (self.bin_file.options.module) |module| {
            module.compile_log_text.shrinkAndFree(module.gpa, 0);
            module.generation += 1;

            // TODO Detect which source files changed.
            // Until then we simulate a full cache miss. Source files could have been loaded for any reason;
            // to force a refresh we unload now.
            module.root_scope.unload(module.gpa);
            module.failed_root_src_file = null;
            module.analyzeContainer(&module.root_scope.root_container) catch |err| switch (err) {
                error.AnalysisFail => {
                    assert(self.totalErrorCount() != 0);
                },
                error.OutOfMemory => return error.OutOfMemory,
                else => |e| {
                    module.failed_root_src_file = e;
                },
            };

            // TODO only analyze imports if they are still referenced
            for (module.import_table.items()) |entry| {
                entry.value.unload(module.gpa);
                module.analyzeContainer(&entry.value.root_container) catch |err| switch (err) {
                    error.AnalysisFail => {
                        assert(self.totalErrorCount() != 0);
                    },
                    else => |e| return e,
                };
            }
        }
    }

    try self.performAllTheWork();

    if (!use_stage1) {
        if (self.bin_file.options.module) |module| {
            // Process the deletion set.
            while (module.deletion_set.popOrNull()) |decl| {
                if (decl.dependants.items().len != 0) {
                    decl.deletion_flag = false;
                    continue;
                }
                try module.deleteDecl(decl);
            }
        }
    }

    if (self.totalErrorCount() != 0) {
        // Skip flushing.
        self.link_error_flags = .{};
        return;
    }

    // This is needed before reading the error flags.
    try self.bin_file.flush(self);
    self.link_error_flags = self.bin_file.errorFlags();

    if (!use_stage1) {
        if (self.bin_file.options.module) |module| {
            try link.File.C.flushEmitH(module);
        }
    }

    // If there are any errors, we anticipate the source files being loaded
    // to report error messages. Otherwise we unload all source files to save memory.
    if (self.totalErrorCount() == 0 and !self.keep_source_files_loaded) {
        if (self.bin_file.options.module) |module| {
            module.root_scope.unload(self.gpa);
        }
    }
}

/// Having the file open for writing is problematic as far as executing the
/// binary is concerned. This will remove the write flag, or close the file,
/// or whatever is needed so that it can be executed.
/// After this, one must call` makeFileWritable` before calling `update`.
pub fn makeBinFileExecutable(self: *Compilation) !void {
    return self.bin_file.makeExecutable();
}

pub fn makeBinFileWritable(self: *Compilation) !void {
    return self.bin_file.makeWritable();
}

pub fn totalErrorCount(self: *Compilation) usize {
    var total: usize = self.failed_c_objects.items().len;

    if (self.bin_file.options.module) |module| {
        total += module.failed_decls.count() +
            module.emit_h_failed_decls.count() +
            module.failed_exports.items().len +
            module.failed_files.items().len +
            @boolToInt(module.failed_root_src_file != null);
    }

    // The "no entry point found" error only counts if there are no other errors.
    if (total == 0) {
        total += @boolToInt(self.link_error_flags.no_entry_point_found);
    }

    // Compile log errors only count if there are no other errors.
    if (total == 0) {
        if (self.bin_file.options.module) |module| {
            total += @boolToInt(module.compile_log_decls.items().len != 0);
        }
    }

    return total;
}

pub fn getAllErrorsAlloc(self: *Compilation) !AllErrors {
    var arena = std.heap.ArenaAllocator.init(self.gpa);
    errdefer arena.deinit();

    var errors = std.ArrayList(AllErrors.Message).init(self.gpa);
    defer errors.deinit();

    for (self.failed_c_objects.items()) |entry| {
        const c_object = entry.key;
        const err_msg = entry.value;
        // TODO these fields will need to be adjusted when we have proper
        // C error reporting bubbling up.
        try errors.append(.{
            .src = .{
                .src_path = try arena.allocator.dupe(u8, c_object.src.src_path),
                .msg = try std.fmt.allocPrint(&arena.allocator, "unable to build C object: {s}", .{
                    err_msg.msg,
                }),
                .byte_offset = 0,
                .line = err_msg.line,
                .column = err_msg.column,
            },
        });
    }
    if (self.bin_file.options.module) |module| {
        for (module.failed_files.items()) |entry| {
            try AllErrors.add(module, &arena, &errors, entry.value.*);
        }
        for (module.failed_decls.items()) |entry| {
            try AllErrors.add(module, &arena, &errors, entry.value.*);
        }
        for (module.emit_h_failed_decls.items()) |entry| {
            try AllErrors.add(module, &arena, &errors, entry.value.*);
        }
        for (module.failed_exports.items()) |entry| {
            try AllErrors.add(module, &arena, &errors, entry.value.*);
        }
        if (module.failed_root_src_file) |err| {
            const file_path = try module.root_pkg.root_src_directory.join(&arena.allocator, &[_][]const u8{
                module.root_pkg.root_src_path,
            });
            const msg = try std.fmt.allocPrint(&arena.allocator, "unable to read {s}: {s}", .{
                file_path, @errorName(err),
            });
            try AllErrors.addPlain(&arena, &errors, msg);
        }
    }

    if (errors.items.len == 0 and self.link_error_flags.no_entry_point_found) {
        try errors.append(.{
            .plain = .{
                .msg = try std.fmt.allocPrint(&arena.allocator, "no entry point found", .{}),
            },
        });
    }

    if (self.bin_file.options.module) |module| {
        const compile_log_items = module.compile_log_decls.items();
        if (errors.items.len == 0 and compile_log_items.len != 0) {
            // First one will be the error; subsequent ones will be notes.
            const err_msg = Module.ErrorMsg{
                .src_loc = compile_log_items[0].value,
                .msg = "found compile log statement",
                .notes = try self.gpa.alloc(Module.ErrorMsg, compile_log_items.len - 1),
            };
            defer self.gpa.free(err_msg.notes);

            for (compile_log_items[1..]) |entry, i| {
                err_msg.notes[i] = .{
                    .src_loc = entry.value,
                    .msg = "also here",
                };
            }

            try AllErrors.add(module, &arena, &errors, err_msg);
        }
    }

    assert(errors.items.len == self.totalErrorCount());

    return AllErrors{
        .list = try arena.allocator.dupe(AllErrors.Message, errors.items),
        .arena = arena.state,
    };
}

pub fn getCompileLogOutput(self: *Compilation) []const u8 {
    const module = self.bin_file.options.module orelse return &[0]u8{};
    return module.compile_log_text.items;
}

pub fn performAllTheWork(self: *Compilation) error{ TimerUnsupported, OutOfMemory }!void {
    var progress: std.Progress = .{};
    var main_progress_node = try progress.start("", 0);
    defer main_progress_node.end();
    if (self.color == .off) progress.terminal = null;

    var c_comp_progress_node = main_progress_node.start("Compile C Objects", self.c_source_files.len);
    defer c_comp_progress_node.end();

    self.work_queue_wait_group.reset();
    defer self.work_queue_wait_group.wait();

    while (self.c_object_work_queue.readItem()) |c_object| {
        self.work_queue_wait_group.start();
        try self.thread_pool.spawn(workerUpdateCObject, .{
            self, c_object, &c_comp_progress_node, &self.work_queue_wait_group,
        });
    }

    while (self.work_queue.readItem()) |work_item| switch (work_item) {
        .codegen_decl => |decl| switch (decl.analysis) {
            .unreferenced => unreachable,
            .in_progress => unreachable,
            .outdated => unreachable,

            .sema_failure,
            .codegen_failure,
            .dependency_failure,
            .sema_failure_retryable,
            => continue,

            .complete, .codegen_failure_retryable => {
                if (build_options.omit_stage2)
                    @panic("sadly stage2 is omitted from this build to save memory on the CI server");
                const module = self.bin_file.options.module.?;
                if (decl.typed_value.most_recent.typed_value.val.castTag(.function)) |payload| {
                    const func = payload.data;
                    switch (func.state) {
                        .queued => module.analyzeFnBody(decl, func) catch |err| switch (err) {
                            error.AnalysisFail => {
                                assert(func.state != .in_progress);
                                continue;
                            },
                            error.OutOfMemory => return error.OutOfMemory,
                        },
                        .in_progress => unreachable,
                        .inline_only => unreachable, // don't queue work for this
                        .sema_failure, .dependency_failure => continue,
                        .success => {},
                    }
                    // Here we tack on additional allocations to the Decl's arena. The allocations
                    // are lifetime annotations in the ZIR.
                    var decl_arena = decl.typed_value.most_recent.arena.?.promote(module.gpa);
                    defer decl.typed_value.most_recent.arena.?.* = decl_arena.state;
                    log.debug("analyze liveness of {s}", .{decl.name});
                    try liveness.analyze(module.gpa, &decl_arena.allocator, func.body);

                    if (std.builtin.mode == .Debug and self.verbose_ir) {
                        func.dump(module.*);
                    }
                }

                log.debug("calling updateDecl on '{s}', type={}", .{
                    decl.name, decl.typed_value.most_recent.typed_value.ty,
                });
                assert(decl.typed_value.most_recent.typed_value.ty.hasCodeGenBits());

                self.bin_file.updateDecl(module, decl) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.AnalysisFail => {
                        decl.analysis = .codegen_failure;
                        continue;
                    },
                    else => {
                        try module.failed_decls.ensureCapacity(module.gpa, module.failed_decls.items().len + 1);
                        module.failed_decls.putAssumeCapacityNoClobber(decl, try Module.ErrorMsg.create(
                            module.gpa,
                            decl.srcLoc(),
                            "unable to codegen: {s}",
                            .{@errorName(err)},
                        ));
                        decl.analysis = .codegen_failure_retryable;
                        continue;
                    },
                };
            },
        },
        .emit_h_decl => |decl| switch (decl.analysis) {
            .unreferenced => unreachable,
            .in_progress => unreachable,
            .outdated => unreachable,

            .sema_failure,
            .dependency_failure,
            .sema_failure_retryable,
            => continue,

            // emit-h only requires semantic analysis of the Decl to be complete,
            // it does not depend on machine code generation to succeed.
            .codegen_failure, .codegen_failure_retryable, .complete => {
                if (build_options.omit_stage2)
                    @panic("sadly stage2 is omitted from this build to save memory on the CI server");
                const module = self.bin_file.options.module.?;
                const emit_loc = module.emit_h.?;
                const tv = decl.typed_value.most_recent.typed_value;
                const emit_h = decl.getEmitH(module);
                const fwd_decl = &emit_h.fwd_decl;
                fwd_decl.shrinkRetainingCapacity(0);

                var dg: c_codegen.DeclGen = .{
                    .module = module,
                    .error_msg = null,
                    .decl = decl,
                    .fwd_decl = fwd_decl.toManaged(module.gpa),
                };
                defer dg.fwd_decl.deinit();

                c_codegen.genHeader(&dg) catch |err| switch (err) {
                    error.AnalysisFail => {
                        try module.emit_h_failed_decls.put(module.gpa, decl, dg.error_msg.?);
                        continue;
                    },
                    else => |e| return e,
                };

                fwd_decl.* = dg.fwd_decl.moveToUnmanaged();
                fwd_decl.shrinkAndFree(module.gpa, fwd_decl.items.len);
            },
        },
        .analyze_decl => |decl| {
            if (build_options.omit_stage2)
                @panic("sadly stage2 is omitted from this build to save memory on the CI server");
            const module = self.bin_file.options.module.?;
            module.ensureDeclAnalyzed(decl) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AnalysisFail => continue,
            };
        },
        .update_line_number => |decl| {
            if (build_options.omit_stage2)
                @panic("sadly stage2 is omitted from this build to save memory on the CI server");
            const module = self.bin_file.options.module.?;
            self.bin_file.updateDeclLineNumber(module, decl) catch |err| {
                try module.failed_decls.ensureCapacity(module.gpa, module.failed_decls.items().len + 1);
                module.failed_decls.putAssumeCapacityNoClobber(decl, try Module.ErrorMsg.create(
                    module.gpa,
                    decl.srcLoc(),
                    "unable to update line number: {s}",
                    .{@errorName(err)},
                ));
                decl.analysis = .codegen_failure_retryable;
            };
        },
        .glibc_crt_file => |crt_file| {
            glibc.buildCRTFile(self, crt_file) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build glibc CRT file: {s}", .{@errorName(err)});
            };
        },
        .glibc_shared_objects => {
            glibc.buildSharedObjects(self) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build glibc shared objects: {s}", .{@errorName(err)});
            };
        },
        .musl_crt_file => |crt_file| {
            musl.buildCRTFile(self, crt_file) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build musl CRT file: {s}", .{@errorName(err)});
            };
        },
        .mingw_crt_file => |crt_file| {
            mingw.buildCRTFile(self, crt_file) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build mingw-w64 CRT file: {s}", .{@errorName(err)});
            };
        },
        .windows_import_lib => |index| {
            const link_lib = self.bin_file.options.system_libs.items()[index].key;
            mingw.buildImportLib(self, link_lib) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to generate DLL import .lib file: {s}", .{@errorName(err)});
            };
        },
        .libunwind => {
            libunwind.buildStaticLib(self) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build libunwind: {s}", .{@errorName(err)});
            };
        },
        .libcxx => {
            libcxx.buildLibCXX(self) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build libcxx: {s}", .{@errorName(err)});
            };
        },
        .libcxxabi => {
            libcxx.buildLibCXXABI(self) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build libcxxabi: {s}", .{@errorName(err)});
            };
        },
        .libtsan => {
            libtsan.buildTsan(self) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build TSAN library: {s}", .{@errorName(err)});
            };
        },
        .compiler_rt_lib => {
            self.buildOutputFromZig("compiler_rt.zig", .Lib, &self.compiler_rt_static_lib) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build compiler_rt: {s}", .{@errorName(err)});
            };
        },
        .compiler_rt_obj => {
            self.buildOutputFromZig("compiler_rt.zig", .Obj, &self.compiler_rt_obj) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build compiler_rt: {s}", .{@errorName(err)});
            };
        },
        .libssp => {
            self.buildOutputFromZig("ssp.zig", .Lib, &self.libssp_static_lib) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build libssp: {s}", .{@errorName(err)});
            };
        },
        .zig_libc => {
            self.buildOutputFromZig("c.zig", .Lib, &self.libc_static_lib) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to build zig's multitarget libc: {s}", .{@errorName(err)});
            };
        },
        .generate_builtin_zig => {
            // This Job is only queued up if there is a zig module.
            self.updateBuiltinZigFile(self.bin_file.options.module.?) catch |err| {
                // TODO Expose this as a normal compile error rather than crashing here.
                fatal("unable to update builtin.zig file: {s}", .{@errorName(err)});
            };
        },
        .stage1_module => {
            if (!build_options.is_stage1)
                unreachable;

            self.updateStage1Module(main_progress_node) catch |err| {
                fatal("unable to build stage1 zig object: {s}", .{@errorName(err)});
            };
        },
    };
}

pub fn obtainCObjectCacheManifest(comp: *const Compilation) Cache.Manifest {
    var man = comp.cache_parent.obtain();

    // Only things that need to be added on top of the base hash, and only things
    // that apply both to @cImport and compiling C objects. No linking stuff here!
    // Also nothing that applies only to compiling .zig code.
    man.hash.add(comp.sanitize_c);
    man.hash.addListOfBytes(comp.clang_argv);
    man.hash.add(comp.bin_file.options.link_libcpp);
    man.hash.addListOfBytes(comp.libc_include_dir_list);

    return man;
}

test "cImport" {
    _ = cImport;
}

const CImportResult = struct {
    out_zig_path: []u8,
    errors: []translate_c.ClangErrMsg,
};

/// Caller owns returned memory.
/// This API is currently coupled pretty tightly to stage1's needs; it will need to be reworked
/// a bit when we want to start using it from self-hosted.
pub fn cImport(comp: *Compilation, c_src: []const u8) !CImportResult {
    if (!build_options.have_llvm)
        return error.ZigCompilerNotBuiltWithLLVMExtensions;

    const tracy = trace(@src());
    defer tracy.end();

    const cimport_zig_basename = "cimport.zig";

    var man = comp.obtainCObjectCacheManifest();
    defer man.deinit();

    man.hash.add(@as(u16, 0xb945)); // Random number to distinguish translate-c from compiling C objects
    man.hash.addBytes(c_src);

    // If the previous invocation resulted in clang errors, we will see a hit
    // here with 0 files in the manifest, in which case it is actually a miss.
    // We need to "unhit" in this case, to keep the digests matching.
    const prev_hash_state = man.hash.peekBin();
    const actual_hit = hit: {
        const is_hit = try man.hit();
        if (man.files.items.len == 0) {
            man.unhit(prev_hash_state, 0);
            break :hit false;
        }
        break :hit true;
    };
    const digest = if (!actual_hit) digest: {
        var arena_allocator = std.heap.ArenaAllocator.init(comp.gpa);
        defer arena_allocator.deinit();
        const arena = &arena_allocator.allocator;

        const tmp_digest = man.hash.peek();
        const tmp_dir_sub_path = try std.fs.path.join(arena, &[_][]const u8{ "o", &tmp_digest });
        var zig_cache_tmp_dir = try comp.local_cache_directory.handle.makeOpenPath(tmp_dir_sub_path, .{});
        defer zig_cache_tmp_dir.close();
        const cimport_basename = "cimport.h";
        const out_h_path = try comp.local_cache_directory.join(arena, &[_][]const u8{
            tmp_dir_sub_path, cimport_basename,
        });
        const out_dep_path = try std.fmt.allocPrint(arena, "{s}.d", .{out_h_path});

        try zig_cache_tmp_dir.writeFile(cimport_basename, c_src);
        if (comp.verbose_cimport) {
            log.info("C import source: {s}", .{out_h_path});
        }

        var argv = std.ArrayList([]const u8).init(comp.gpa);
        defer argv.deinit();

        try comp.addTranslateCCArgs(arena, &argv, .c, out_dep_path);

        try argv.append(out_h_path);

        if (comp.verbose_cc) {
            dump_argv(argv.items);
        }

        // Convert to null terminated args.
        const new_argv_with_sentinel = try arena.alloc(?[*:0]const u8, argv.items.len + 1);
        new_argv_with_sentinel[argv.items.len] = null;
        const new_argv = new_argv_with_sentinel[0..argv.items.len :null];
        for (argv.items) |arg, i| {
            new_argv[i] = try arena.dupeZ(u8, arg);
        }

        const c_headers_dir_path = try comp.zig_lib_directory.join(arena, &[_][]const u8{"include"});
        const c_headers_dir_path_z = try arena.dupeZ(u8, c_headers_dir_path);
        var clang_errors: []translate_c.ClangErrMsg = &[0]translate_c.ClangErrMsg{};
        const tree = translate_c.translate(
            comp.gpa,
            new_argv.ptr,
            new_argv.ptr + new_argv.len,
            &clang_errors,
            c_headers_dir_path_z,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ASTUnitFailure => {
                log.warn("clang API returned errors but due to a clang bug, it is not exposing the errors for zig to see. For more details: https://github.com/ziglang/zig/issues/4455", .{});
                return error.ASTUnitFailure;
            },
            error.SemanticAnalyzeFail => {
                return CImportResult{
                    .out_zig_path = "",
                    .errors = clang_errors,
                };
            },
        };
        defer tree.deinit();

        if (comp.verbose_cimport) {
            log.info("C import .d file: {s}", .{out_dep_path});
        }

        const dep_basename = std.fs.path.basename(out_dep_path);
        try man.addDepFilePost(zig_cache_tmp_dir, dep_basename);
        try comp.stage1_cache_manifest.addDepFilePost(zig_cache_tmp_dir, dep_basename);

        const digest = man.final();
        const o_sub_path = try std.fs.path.join(arena, &[_][]const u8{ "o", &digest });
        var o_dir = try comp.local_cache_directory.handle.makeOpenPath(o_sub_path, .{});
        defer o_dir.close();

        var out_zig_file = try o_dir.createFile(cimport_zig_basename, .{});
        defer out_zig_file.close();

        var bos = std.io.bufferedWriter(out_zig_file.writer());
        _ = try std.zig.render(comp.gpa, bos.writer(), tree);
        try bos.flush();

        man.writeManifest() catch |err| {
            log.warn("failed to write cache manifest for C import: {s}", .{@errorName(err)});
        };

        break :digest digest;
    } else man.final();

    const out_zig_path = try comp.local_cache_directory.join(comp.gpa, &[_][]const u8{
        "o", &digest, cimport_zig_basename,
    });
    if (comp.verbose_cimport) {
        log.info("C import output: {s}\n", .{out_zig_path});
    }
    return CImportResult{
        .out_zig_path = out_zig_path,
        .errors = &[0]translate_c.ClangErrMsg{},
    };
}

fn workerUpdateCObject(
    comp: *Compilation,
    c_object: *CObject,
    progress_node: *std.Progress.Node,
    wg: *WaitGroup,
) void {
    defer wg.finish();

    comp.updateCObject(c_object, progress_node) catch |err| switch (err) {
        error.AnalysisFail => return,
        else => {
            comp.reportRetryableCObjectError(c_object, err) catch |oom| switch (oom) {
                // Swallowing this error is OK because it's implied to be OOM when
                // there is a missing failed_c_objects error message.
                error.OutOfMemory => {},
            };
        },
    };
}

fn reportRetryableCObjectError(
    comp: *Compilation,
    c_object: *CObject,
    err: anyerror,
) error{OutOfMemory}!void {
    c_object.status = .failure_retryable;

    const c_obj_err_msg = try comp.gpa.create(CObject.ErrorMsg);
    errdefer comp.gpa.destroy(c_obj_err_msg);
    const msg = try std.fmt.allocPrint(comp.gpa, "unable to build C object: {s}", .{@errorName(err)});
    errdefer comp.gpa.free(msg);
    c_obj_err_msg.* = .{
        .msg = msg,
        .line = 0,
        .column = 0,
    };
    {
        const lock = comp.mutex.acquire();
        defer lock.release();
        try comp.failed_c_objects.putNoClobber(comp.gpa, c_object, c_obj_err_msg);
    }
}

fn updateCObject(comp: *Compilation, c_object: *CObject, c_comp_progress_node: *std.Progress.Node) !void {
    if (!build_options.have_llvm) {
        return comp.failCObj(c_object, "clang not available: compiler built without LLVM extensions", .{});
    }
    const self_exe_path = comp.self_exe_path orelse
        return comp.failCObj(c_object, "clang compilation disabled", .{});

    const tracy = trace(@src());
    defer tracy.end();

    if (c_object.clearStatus(comp.gpa)) {
        // There was previous failure.
        const lock = comp.mutex.acquire();
        defer lock.release();
        // If the failure was OOM, there will not be an entry here, so we do
        // not assert discard.
        _ = comp.failed_c_objects.swapRemove(c_object);
    }

    var man = comp.obtainCObjectCacheManifest();
    defer man.deinit();

    man.hash.add(comp.clang_preprocessor_mode);

    _ = try man.addFile(c_object.src.src_path, null);
    {
        // Hash the extra flags, with special care to call addFile for file parameters.
        // TODO this logic can likely be improved by utilizing clang_options_data.zig.
        const file_args = [_][]const u8{"-include"};
        var arg_i: usize = 0;
        while (arg_i < c_object.src.extra_flags.len) : (arg_i += 1) {
            const arg = c_object.src.extra_flags[arg_i];
            man.hash.addBytes(arg);
            for (file_args) |file_arg| {
                if (mem.eql(u8, file_arg, arg) and arg_i + 1 < c_object.src.extra_flags.len) {
                    arg_i += 1;
                    _ = try man.addFile(c_object.src.extra_flags[arg_i], null);
                }
            }
        }
    }

    {
        const is_collision = blk: {
            const bin_digest = man.hash.peekBin();

            const lock = comp.mutex.acquire();
            defer lock.release();

            const gop = try comp.c_object_cache_digest_set.getOrPut(comp.gpa, bin_digest);
            break :blk gop.found_existing;
        };
        if (is_collision) {
            return comp.failCObj(
                c_object,
                "the same source file was already added to the same compilation with the same flags",
                .{},
            );
        }
    }

    var arena_allocator = std.heap.ArenaAllocator.init(comp.gpa);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;

    const c_source_basename = std.fs.path.basename(c_object.src.src_path);

    c_comp_progress_node.activate();
    var child_progress_node = c_comp_progress_node.start(c_source_basename, 0);
    child_progress_node.activate();
    defer child_progress_node.end();

    // Special case when doing build-obj for just one C file. When there are more than one object
    // file and building an object we need to link them together, but with just one it should go
    // directly to the output file.
    const direct_o = comp.c_source_files.len == 1 and comp.bin_file.options.module == null and
        comp.bin_file.options.output_mode == .Obj and comp.bin_file.options.objects.len == 0;
    const o_basename_noext = if (direct_o)
        comp.bin_file.options.root_name
    else
        c_source_basename[0 .. c_source_basename.len - std.fs.path.extension(c_source_basename).len];
    const o_basename = try std.fmt.allocPrint(arena, "{s}{s}", .{ o_basename_noext, comp.getTarget().oFileExt() });

    const digest = if (!comp.disable_c_depfile and try man.hit()) man.final() else blk: {
        var argv = std.ArrayList([]const u8).init(comp.gpa);
        defer argv.deinit();

        // We can't know the digest until we do the C compiler invocation, so we need a temporary filename.
        const out_obj_path = try comp.tmpFilePath(arena, o_basename);
        var zig_cache_tmp_dir = try comp.local_cache_directory.handle.makeOpenPath("tmp", .{});
        defer zig_cache_tmp_dir.close();

        try argv.appendSlice(&[_][]const u8{ self_exe_path, "clang" });

        const ext = classifyFileExt(c_object.src.src_path);
        const out_dep_path: ?[]const u8 = if (comp.disable_c_depfile or !ext.clangSupportsDepFile())
            null
        else
            try std.fmt.allocPrint(arena, "{s}.d", .{out_obj_path});
        try comp.addCCArgs(arena, &argv, ext, out_dep_path);

        try argv.ensureCapacity(argv.items.len + 3);
        switch (comp.clang_preprocessor_mode) {
            .no => argv.appendSliceAssumeCapacity(&[_][]const u8{ "-c", "-o", out_obj_path }),
            .yes => argv.appendSliceAssumeCapacity(&[_][]const u8{ "-E", "-o", out_obj_path }),
            .stdout => argv.appendAssumeCapacity("-E"),
        }

        try argv.append(c_object.src.src_path);
        try argv.appendSlice(c_object.src.extra_flags);

        if (comp.verbose_cc) {
            dump_argv(argv.items);
        }

        const child = try std.ChildProcess.init(argv.items, arena);
        defer child.deinit();

        if (comp.clang_passthrough_mode) {
            child.stdin_behavior = .Inherit;
            child.stdout_behavior = .Inherit;
            child.stderr_behavior = .Inherit;

            const term = child.spawnAndWait() catch |err| {
                return comp.failCObj(c_object, "unable to spawn {s}: {s}", .{ argv.items[0], @errorName(err) });
            };
            switch (term) {
                .Exited => |code| {
                    if (code != 0) {
                        // TODO https://github.com/ziglang/zig/issues/6342
                        std.process.exit(1);
                    }
                    if (comp.clang_preprocessor_mode == .stdout)
                        std.process.exit(0);
                },
                else => std.process.abort(),
            }
        } else {
            child.stdin_behavior = .Ignore;
            child.stdout_behavior = .Ignore;
            child.stderr_behavior = .Pipe;

            try child.spawn();

            const stderr_reader = child.stderr.?.reader();

            // TODO https://github.com/ziglang/zig/issues/6343
            // Please uncomment and use stdout once this issue is fixed
            // const stdout = try stdout_reader.readAllAlloc(arena, std.math.maxInt(u32));
            const stderr = try stderr_reader.readAllAlloc(arena, 10 * 1024 * 1024);

            const term = child.wait() catch |err| {
                return comp.failCObj(c_object, "unable to spawn {s}: {s}", .{ argv.items[0], @errorName(err) });
            };

            switch (term) {
                .Exited => |code| {
                    if (code != 0) {
                        // TODO parse clang stderr and turn it into an error message
                        // and then call failCObjWithOwnedErrorMsg
                        log.err("clang failed with stderr: {s}", .{stderr});
                        return comp.failCObj(c_object, "clang exited with code {d}", .{code});
                    }
                },
                else => {
                    log.err("clang terminated with stderr: {s}", .{stderr});
                    return comp.failCObj(c_object, "clang terminated unexpectedly", .{});
                },
            }
        }

        if (out_dep_path) |dep_file_path| {
            const dep_basename = std.fs.path.basename(dep_file_path);
            // Add the files depended on to the cache system.
            try man.addDepFilePost(zig_cache_tmp_dir, dep_basename);
            // Just to save disk space, we delete the file because it is never needed again.
            zig_cache_tmp_dir.deleteFile(dep_basename) catch |err| {
                log.warn("failed to delete '{s}': {s}", .{ dep_file_path, @errorName(err) });
            };
        }

        // We don't actually care whether it's a cache hit or miss; we just need the digest and the lock.
        if (comp.disable_c_depfile) _ = try man.hit();

        // Rename into place.
        const digest = man.final();
        const o_sub_path = try std.fs.path.join(arena, &[_][]const u8{ "o", &digest });
        var o_dir = try comp.local_cache_directory.handle.makeOpenPath(o_sub_path, .{});
        defer o_dir.close();
        const tmp_basename = std.fs.path.basename(out_obj_path);
        try std.fs.rename(zig_cache_tmp_dir, tmp_basename, o_dir, o_basename);

        man.writeManifest() catch |err| {
            log.warn("failed to write cache manifest when compiling '{s}': {s}", .{ c_object.src.src_path, @errorName(err) });
        };
        break :blk digest;
    };

    c_object.status = .{
        .success = .{
            .object_path = try comp.local_cache_directory.join(comp.gpa, &[_][]const u8{
                "o", &digest, o_basename,
            }),
            .lock = man.toOwnedLock(),
        },
    };
}

pub fn tmpFilePath(comp: *Compilation, arena: *Allocator, suffix: []const u8) error{OutOfMemory}![]const u8 {
    const s = std.fs.path.sep_str;
    const rand_int = std.crypto.random.int(u64);
    if (comp.local_cache_directory.path) |p| {
        return std.fmt.allocPrint(arena, "{s}" ++ s ++ "tmp" ++ s ++ "{x}-{s}", .{ p, rand_int, suffix });
    } else {
        return std.fmt.allocPrint(arena, "tmp" ++ s ++ "{x}-{s}", .{ rand_int, suffix });
    }
}

pub fn addTranslateCCArgs(
    comp: *Compilation,
    arena: *Allocator,
    argv: *std.ArrayList([]const u8),
    ext: FileExt,
    out_dep_path: ?[]const u8,
) !void {
    try argv.appendSlice(&[_][]const u8{ "-x", "c" });
    try comp.addCCArgs(arena, argv, ext, out_dep_path);
    // This gives us access to preprocessing entities, presumably at the cost of performance.
    try argv.appendSlice(&[_][]const u8{ "-Xclang", "-detailed-preprocessing-record" });
}

/// Add common C compiler args between translate-c and C object compilation.
pub fn addCCArgs(
    comp: *const Compilation,
    arena: *Allocator,
    argv: *std.ArrayList([]const u8),
    ext: FileExt,
    out_dep_path: ?[]const u8,
) !void {
    const target = comp.getTarget();

    if (ext == .cpp) {
        try argv.append("-nostdinc++");
    }

    // We don't ever put `-fcolor-diagnostics` or `-fno-color-diagnostics` because in passthrough mode
    // we want Clang to infer it, and in normal mode we always want it off, which will be true since
    // clang will detect stderr as a pipe rather than a terminal.
    if (!comp.clang_passthrough_mode) {
        // Make stderr more easily parseable.
        try argv.append("-fno-caret-diagnostics");
    }

    if (comp.bin_file.options.function_sections) {
        try argv.append("-ffunction-sections");
    }

    if (comp.bin_file.options.link_libcpp) {
        const libcxx_include_path = try std.fs.path.join(arena, &[_][]const u8{
            comp.zig_lib_directory.path.?, "libcxx", "include",
        });
        const libcxxabi_include_path = try std.fs.path.join(arena, &[_][]const u8{
            comp.zig_lib_directory.path.?, "libcxxabi", "include",
        });

        try argv.append("-isystem");
        try argv.append(libcxx_include_path);

        try argv.append("-isystem");
        try argv.append(libcxxabi_include_path);

        if (target.abi.isMusl()) {
            try argv.append("-D_LIBCPP_HAS_MUSL_LIBC");
        }
        try argv.append("-D_LIBCPP_DISABLE_VISIBILITY_ANNOTATIONS");
        try argv.append("-D_LIBCXXABI_DISABLE_VISIBILITY_ANNOTATIONS");
    }

    const llvm_triple = try @import("codegen/llvm.zig").targetTriple(arena, target);
    try argv.appendSlice(&[_][]const u8{ "-target", llvm_triple });

    switch (ext) {
        .c, .cpp, .h => {
            try argv.appendSlice(&[_][]const u8{
                "-nostdinc",
                "-fno-spell-checking",
            });

            // According to Rich Felker libc headers are supposed to go before C language headers.
            // However as noted by @dimenus, appending libc headers before c_headers breaks intrinsics
            // and other compiler specific items.
            const c_headers_dir = try std.fs.path.join(arena, &[_][]const u8{ comp.zig_lib_directory.path.?, "include" });
            try argv.append("-isystem");
            try argv.append(c_headers_dir);

            for (comp.libc_include_dir_list) |include_dir| {
                try argv.append("-isystem");
                try argv.append(include_dir);
            }

            if (target.cpu.model.llvm_name) |llvm_name| {
                try argv.appendSlice(&[_][]const u8{
                    "-Xclang", "-target-cpu", "-Xclang", llvm_name,
                });
            }

            // It would be really nice if there was a more compact way to communicate this info to Clang.
            const all_features_list = target.cpu.arch.allFeaturesList();
            try argv.ensureCapacity(argv.items.len + all_features_list.len * 4);
            for (all_features_list) |feature, index_usize| {
                const index = @intCast(std.Target.Cpu.Feature.Set.Index, index_usize);
                const is_enabled = target.cpu.features.isEnabled(index);

                if (feature.llvm_name) |llvm_name| {
                    argv.appendSliceAssumeCapacity(&[_][]const u8{ "-Xclang", "-target-feature", "-Xclang" });
                    const plus_or_minus = "-+"[@boolToInt(is_enabled)];
                    const arg = try std.fmt.allocPrint(arena, "{c}{s}", .{ plus_or_minus, llvm_name });
                    argv.appendAssumeCapacity(arg);
                }
            }
            const mcmodel = comp.bin_file.options.machine_code_model;
            if (mcmodel != .default) {
                try argv.append(try std.fmt.allocPrint(arena, "-mcmodel={s}", .{@tagName(mcmodel)}));
            }

            switch (target.os.tag) {
                .windows => {
                    // windows.h has files such as pshpack1.h which do #pragma packing,
                    // triggering a clang warning. So for this target, we disable this warning.
                    if (target.abi.isGnu()) {
                        try argv.append("-Wno-pragma-pack");
                    }
                },
                .macos => {
                    // Pass the proper -m<os>-version-min argument for darwin.
                    const ver = target.os.version_range.semver.min;
                    try argv.append(try std.fmt.allocPrint(arena, "-mmacos-version-min={d}.{d}.{d}", .{
                        ver.major, ver.minor, ver.patch,
                    }));
                },
                .ios, .tvos, .watchos => switch (target.cpu.arch) {
                    // Pass the proper -m<os>-version-min argument for darwin.
                    .i386, .x86_64 => {
                        const ver = target.os.version_range.semver.min;
                        try argv.append(try std.fmt.allocPrint(
                            arena,
                            "-m{s}-simulator-version-min={d}.{d}.{d}",
                            .{ @tagName(target.os.tag), ver.major, ver.minor, ver.patch },
                        ));
                    },
                    else => {
                        const ver = target.os.version_range.semver.min;
                        try argv.append(try std.fmt.allocPrint(arena, "-m{s}-version-min={d}.{d}.{d}", .{
                            @tagName(target.os.tag), ver.major, ver.minor, ver.patch,
                        }));
                    },
                },
                else => {},
            }

            if (!comp.bin_file.options.strip) {
                try argv.append("-g");
                switch (comp.bin_file.options.object_format) {
                    .coff, .pe => try argv.append("-gcodeview"),
                    else => {},
                }
            }

            if (comp.haveFramePointer()) {
                try argv.append("-fno-omit-frame-pointer");
            } else {
                try argv.append("-fomit-frame-pointer");
            }

            if (comp.sanitize_c and !comp.bin_file.options.tsan) {
                try argv.append("-fsanitize=undefined");
                try argv.append("-fsanitize-trap=undefined");
            } else if (comp.sanitize_c and comp.bin_file.options.tsan) {
                try argv.append("-fsanitize=undefined,thread");
                try argv.append("-fsanitize-trap=undefined");
            } else if (!comp.sanitize_c and comp.bin_file.options.tsan) {
                try argv.append("-fsanitize=thread");
            }

            if (comp.bin_file.options.red_zone) {
                try argv.append("-mred-zone");
            } else if (target_util.hasRedZone(target)) {
                try argv.append("-mno-red-zone");
            }

            switch (comp.bin_file.options.optimize_mode) {
                .Debug => {
                    // windows c runtime requires -D_DEBUG if using debug libraries
                    try argv.append("-D_DEBUG");
                    try argv.append("-Og");

                    if (comp.bin_file.options.link_libc) {
                        try argv.append("-fstack-protector-strong");
                        try argv.append("--param");
                        try argv.append("ssp-buffer-size=4");
                    } else {
                        try argv.append("-fno-stack-protector");
                    }
                },
                .ReleaseSafe => {
                    // See the comment in the BuildModeFastRelease case for why we pass -O2 rather
                    // than -O3 here.
                    try argv.append("-O2");
                    if (comp.bin_file.options.link_libc) {
                        try argv.append("-D_FORTIFY_SOURCE=2");
                        try argv.append("-fstack-protector-strong");
                        try argv.append("--param");
                        try argv.append("ssp-buffer-size=4");
                    } else {
                        try argv.append("-fno-stack-protector");
                    }
                },
                .ReleaseFast => {
                    try argv.append("-DNDEBUG");
                    // Here we pass -O2 rather than -O3 because, although we do the equivalent of
                    // -O3 in Zig code, the justification for the difference here is that Zig
                    // has better detection and prevention of undefined behavior, so -O3 is safer for
                    // Zig code than it is for C code. Also, C programmers are used to their code
                    // running in -O2 and thus the -O3 path has been tested less.
                    try argv.append("-O2");
                    try argv.append("-fno-stack-protector");
                },
                .ReleaseSmall => {
                    try argv.append("-DNDEBUG");
                    try argv.append("-Os");
                    try argv.append("-fno-stack-protector");
                },
            }

            if (target_util.supports_fpic(target) and comp.bin_file.options.pic) {
                try argv.append("-fPIC");
            }
        },
        .shared_library, .assembly, .ll, .bc, .unknown, .static_library, .object, .zig, .zir => {},
    }
    if (out_dep_path) |p| {
        try argv.appendSlice(&[_][]const u8{ "-MD", "-MV", "-MF", p });
    }
    // Argh, why doesn't the assembler accept the list of CPU features?!
    // I don't see a way to do this other than hard coding everything.
    switch (target.cpu.arch) {
        .riscv32, .riscv64 => {
            if (std.Target.riscv.featureSetHas(target.cpu.features, .relax)) {
                try argv.append("-mrelax");
            } else {
                try argv.append("-mno-relax");
            }
        },
        else => {
            // TODO
        },
    }

    if (target.os.tag == .freestanding) {
        try argv.append("-ffreestanding");
    }

    try argv.appendSlice(comp.clang_argv);
}

fn failCObj(comp: *Compilation, c_object: *CObject, comptime format: []const u8, args: anytype) InnerError {
    @setCold(true);
    const err_msg = blk: {
        const msg = try std.fmt.allocPrint(comp.gpa, format, args);
        errdefer comp.gpa.free(msg);
        const err_msg = try comp.gpa.create(CObject.ErrorMsg);
        errdefer comp.gpa.destroy(err_msg);
        err_msg.* = .{
            .msg = msg,
            .line = 0,
            .column = 0,
        };
        break :blk err_msg;
    };
    return comp.failCObjWithOwnedErrorMsg(c_object, err_msg);
}

fn failCObjWithOwnedErrorMsg(
    comp: *Compilation,
    c_object: *CObject,
    err_msg: *CObject.ErrorMsg,
) InnerError {
    @setCold(true);
    {
        const lock = comp.mutex.acquire();
        defer lock.release();
        {
            errdefer err_msg.destroy(comp.gpa);
            try comp.failed_c_objects.ensureCapacity(comp.gpa, comp.failed_c_objects.items().len + 1);
        }
        comp.failed_c_objects.putAssumeCapacityNoClobber(c_object, err_msg);
    }
    c_object.status = .failure;
    return error.AnalysisFail;
}

pub const FileExt = enum {
    c,
    cpp,
    h,
    ll,
    bc,
    assembly,
    shared_library,
    object,
    static_library,
    zig,
    zir,
    unknown,

    pub fn clangSupportsDepFile(ext: FileExt) bool {
        return switch (ext) {
            .c, .cpp, .h => true,

            .ll,
            .bc,
            .assembly,
            .shared_library,
            .object,
            .static_library,
            .zig,
            .zir,
            .unknown,
            => false,
        };
    }
};

pub fn hasObjectExt(filename: []const u8) bool {
    return mem.endsWith(u8, filename, ".o") or mem.endsWith(u8, filename, ".obj");
}

pub fn hasStaticLibraryExt(filename: []const u8) bool {
    return mem.endsWith(u8, filename, ".a") or mem.endsWith(u8, filename, ".lib");
}

pub fn hasCExt(filename: []const u8) bool {
    return mem.endsWith(u8, filename, ".c");
}

pub fn hasCppExt(filename: []const u8) bool {
    return mem.endsWith(u8, filename, ".C") or
        mem.endsWith(u8, filename, ".cc") or
        mem.endsWith(u8, filename, ".cpp") or
        mem.endsWith(u8, filename, ".cxx");
}

pub fn hasAsmExt(filename: []const u8) bool {
    return mem.endsWith(u8, filename, ".s") or mem.endsWith(u8, filename, ".S");
}

pub fn hasSharedLibraryExt(filename: []const u8) bool {
    if (mem.endsWith(u8, filename, ".so") or
        mem.endsWith(u8, filename, ".dll") or
        mem.endsWith(u8, filename, ".dylib") or
        mem.endsWith(u8, filename, ".tbd"))
    {
        return true;
    }
    // Look for .so.X, .so.X.Y, .so.X.Y.Z
    var it = mem.split(filename, ".");
    _ = it.next().?;
    var so_txt = it.next() orelse return false;
    while (!mem.eql(u8, so_txt, "so")) {
        so_txt = it.next() orelse return false;
    }
    const n1 = it.next() orelse return false;
    const n2 = it.next();
    const n3 = it.next();

    _ = std.fmt.parseInt(u32, n1, 10) catch return false;
    if (n2) |x| _ = std.fmt.parseInt(u32, x, 10) catch return false;
    if (n3) |x| _ = std.fmt.parseInt(u32, x, 10) catch return false;
    if (it.next() != null) return false;

    return true;
}

pub fn classifyFileExt(filename: []const u8) FileExt {
    if (hasCExt(filename)) {
        return .c;
    } else if (hasCppExt(filename)) {
        return .cpp;
    } else if (mem.endsWith(u8, filename, ".ll")) {
        return .ll;
    } else if (mem.endsWith(u8, filename, ".bc")) {
        return .bc;
    } else if (hasAsmExt(filename)) {
        return .assembly;
    } else if (mem.endsWith(u8, filename, ".h")) {
        return .h;
    } else if (mem.endsWith(u8, filename, ".zig")) {
        return .zig;
    } else if (mem.endsWith(u8, filename, ".zir")) {
        return .zir;
    } else if (hasSharedLibraryExt(filename)) {
        return .shared_library;
    } else if (hasStaticLibraryExt(filename)) {
        return .static_library;
    } else if (hasObjectExt(filename)) {
        return .object;
    } else {
        return .unknown;
    }
}

test "classifyFileExt" {
    std.testing.expectEqual(FileExt.cpp, classifyFileExt("foo.cc"));
    std.testing.expectEqual(FileExt.unknown, classifyFileExt("foo.nim"));
    std.testing.expectEqual(FileExt.shared_library, classifyFileExt("foo.so"));
    std.testing.expectEqual(FileExt.shared_library, classifyFileExt("foo.so.1"));
    std.testing.expectEqual(FileExt.shared_library, classifyFileExt("foo.so.1.2"));
    std.testing.expectEqual(FileExt.shared_library, classifyFileExt("foo.so.1.2.3"));
    std.testing.expectEqual(FileExt.unknown, classifyFileExt("foo.so.1.2.3~"));
    std.testing.expectEqual(FileExt.zig, classifyFileExt("foo.zig"));
    std.testing.expectEqual(FileExt.zir, classifyFileExt("foo.zir"));
}

fn haveFramePointer(comp: *const Compilation) bool {
    // If you complicate this logic make sure you update the parent cache hash.
    // Right now it's not in the cache hash because the value depends on optimize_mode
    // and strip which are both already part of the hash.
    return switch (comp.bin_file.options.optimize_mode) {
        .Debug, .ReleaseSafe => !comp.bin_file.options.strip,
        .ReleaseSmall, .ReleaseFast => false,
    };
}

const LibCDirs = struct {
    libc_include_dir_list: []const []const u8,
    libc_installation: ?*const LibCInstallation,
};

fn detectLibCIncludeDirs(
    arena: *Allocator,
    zig_lib_dir: []const u8,
    target: Target,
    is_native_os: bool,
    link_libc: bool,
    libc_installation: ?*const LibCInstallation,
) !LibCDirs {
    if (!link_libc) {
        return LibCDirs{
            .libc_include_dir_list = &[0][]u8{},
            .libc_installation = null,
        };
    }

    if (libc_installation) |lci| {
        return detectLibCFromLibCInstallation(arena, target, lci);
    }

    if (target_util.canBuildLibC(target)) {
        const generic_name = target_util.libCGenericName(target);
        // Some architectures are handled by the same set of headers.
        const arch_name = if (target.abi.isMusl()) target_util.archMuslName(target.cpu.arch) else @tagName(target.cpu.arch);
        const os_name = @tagName(target.os.tag);
        // Musl's headers are ABI-agnostic and so they all have the "musl" ABI name.
        const abi_name = if (target.abi.isMusl()) "musl" else @tagName(target.abi);
        const s = std.fs.path.sep_str;
        const arch_include_dir = try std.fmt.allocPrint(
            arena,
            "{s}" ++ s ++ "libc" ++ s ++ "include" ++ s ++ "{s}-{s}-{s}",
            .{ zig_lib_dir, arch_name, os_name, abi_name },
        );
        const generic_include_dir = try std.fmt.allocPrint(
            arena,
            "{s}" ++ s ++ "libc" ++ s ++ "include" ++ s ++ "generic-{s}",
            .{ zig_lib_dir, generic_name },
        );
        const arch_os_include_dir = try std.fmt.allocPrint(
            arena,
            "{s}" ++ s ++ "libc" ++ s ++ "include" ++ s ++ "{s}-{s}-any",
            .{ zig_lib_dir, @tagName(target.cpu.arch), os_name },
        );
        const generic_os_include_dir = try std.fmt.allocPrint(
            arena,
            "{s}" ++ s ++ "libc" ++ s ++ "include" ++ s ++ "any-{s}-any",
            .{ zig_lib_dir, os_name },
        );

        const list = try arena.alloc([]const u8, 4);
        list[0] = arch_include_dir;
        list[1] = generic_include_dir;
        list[2] = arch_os_include_dir;
        list[3] = generic_os_include_dir;
        return LibCDirs{
            .libc_include_dir_list = list,
            .libc_installation = null,
        };
    }

    if (is_native_os) {
        const libc = try arena.create(LibCInstallation);
        libc.* = try LibCInstallation.findNative(.{ .allocator = arena });
        return detectLibCFromLibCInstallation(arena, target, libc);
    }

    return LibCDirs{
        .libc_include_dir_list = &[0][]u8{},
        .libc_installation = null,
    };
}

fn detectLibCFromLibCInstallation(arena: *Allocator, target: Target, lci: *const LibCInstallation) !LibCDirs {
    var list = std.ArrayList([]const u8).init(arena);
    try list.ensureCapacity(4);

    list.appendAssumeCapacity(lci.include_dir.?);

    const is_redundant = mem.eql(u8, lci.sys_include_dir.?, lci.include_dir.?);
    if (!is_redundant) list.appendAssumeCapacity(lci.sys_include_dir.?);

    if (target.os.tag == .windows) {
        if (std.fs.path.dirname(lci.include_dir.?)) |include_dir_parent| {
            const um_dir = try std.fs.path.join(arena, &[_][]const u8{ include_dir_parent, "um" });
            list.appendAssumeCapacity(um_dir);

            const shared_dir = try std.fs.path.join(arena, &[_][]const u8{ include_dir_parent, "shared" });
            list.appendAssumeCapacity(shared_dir);
        }
    }
    return LibCDirs{
        .libc_include_dir_list = list.items,
        .libc_installation = lci,
    };
}

pub fn get_libc_crt_file(comp: *Compilation, arena: *Allocator, basename: []const u8) ![]const u8 {
    if (comp.wantBuildGLibCFromSource() or
        comp.wantBuildMuslFromSource() or
        comp.wantBuildMinGWFromSource())
    {
        return comp.crt_files.get(basename).?.full_object_path;
    }
    const lci = comp.bin_file.options.libc_installation orelse return error.LibCInstallationNotAvailable;
    const crt_dir_path = lci.crt_dir orelse return error.LibCInstallationMissingCRTDir;
    const full_path = try std.fs.path.join(arena, &[_][]const u8{ crt_dir_path, basename });
    return full_path;
}

fn addBuildingGLibCJobs(comp: *Compilation) !void {
    try comp.work_queue.write(&[_]Job{
        .{ .glibc_crt_file = .crti_o },
        .{ .glibc_crt_file = .crtn_o },
        .{ .glibc_crt_file = .scrt1_o },
        .{ .glibc_crt_file = .libc_nonshared_a },
        .{ .glibc_shared_objects = {} },
    });
}

fn wantBuildLibCFromSource(comp: Compilation) bool {
    const is_exe_or_dyn_lib = switch (comp.bin_file.options.output_mode) {
        .Obj => false,
        .Lib => comp.bin_file.options.link_mode == .Dynamic,
        .Exe => true,
    };
    return comp.bin_file.options.link_libc and is_exe_or_dyn_lib and
        comp.bin_file.options.libc_installation == null;
}

fn wantBuildGLibCFromSource(comp: Compilation) bool {
    return comp.wantBuildLibCFromSource() and comp.getTarget().isGnuLibC();
}

fn wantBuildMuslFromSource(comp: Compilation) bool {
    return comp.wantBuildLibCFromSource() and comp.getTarget().isMusl() and
        !comp.getTarget().isWasm();
}

fn wantBuildMinGWFromSource(comp: Compilation) bool {
    return comp.wantBuildLibCFromSource() and comp.getTarget().isMinGW();
}

fn wantBuildLibUnwindFromSource(comp: *Compilation) bool {
    const is_exe_or_dyn_lib = switch (comp.bin_file.options.output_mode) {
        .Obj => false,
        .Lib => comp.bin_file.options.link_mode == .Dynamic,
        .Exe => true,
    };
    return comp.bin_file.options.link_libc and is_exe_or_dyn_lib and
        comp.bin_file.options.libc_installation == null and
        target_util.libcNeedsLibUnwind(comp.getTarget());
}

fn updateBuiltinZigFile(comp: *Compilation, mod: *Module) !void {
    const tracy = trace(@src());
    defer tracy.end();

    const source = try comp.generateBuiltinZigSource(comp.gpa);
    defer comp.gpa.free(source);
    try mod.zig_cache_artifact_directory.handle.writeFile("builtin.zig", source);
}

pub fn dump_argv(argv: []const []const u8) void {
    for (argv[0 .. argv.len - 1]) |arg| {
        std.debug.print("{s} ", .{arg});
    }
    std.debug.print("{s}\n", .{argv[argv.len - 1]});
}

pub fn generateBuiltinZigSource(comp: *Compilation, allocator: *Allocator) ![]u8 {
    const tracy = trace(@src());
    defer tracy.end();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    const target = comp.getTarget();
    const generic_arch_name = target.cpu.arch.genericName();

    @setEvalBranchQuota(4000);
    try buffer.writer().print(
        \\usingnamespace @import("std").builtin;
        \\/// Deprecated
        \\pub const arch = Target.current.cpu.arch;
        \\/// Deprecated
        \\pub const endian = Target.current.cpu.arch.endian();
        \\
        \\/// Zig version. When writing code that supports multiple versions of Zig, prefer
        \\/// feature detection (i.e. with `@hasDecl` or `@hasField`) over version checks.
        \\pub const zig_version = try @import("std").SemanticVersion.parse("{s}");
        \\
        \\pub const output_mode = OutputMode.{};
        \\pub const link_mode = LinkMode.{};
        \\pub const is_test = {};
        \\pub const single_threaded = {};
        \\pub const abi = Abi.{};
        \\pub const cpu: Cpu = Cpu{{
        \\    .arch = .{},
        \\    .model = &Target.{}.cpu.{},
        \\    .features = Target.{}.featureSet(&[_]Target.{}.Feature{{
        \\
    , .{
        build_options.version,
        std.zig.fmtId(@tagName(comp.bin_file.options.output_mode)),
        std.zig.fmtId(@tagName(comp.bin_file.options.link_mode)),
        comp.bin_file.options.is_test,
        comp.bin_file.options.single_threaded,
        std.zig.fmtId(@tagName(target.abi)),
        std.zig.fmtId(@tagName(target.cpu.arch)),
        std.zig.fmtId(generic_arch_name),
        std.zig.fmtId(target.cpu.model.name),
        std.zig.fmtId(generic_arch_name),
        std.zig.fmtId(generic_arch_name),
    });

    for (target.cpu.arch.allFeaturesList()) |feature, index_usize| {
        const index = @intCast(std.Target.Cpu.Feature.Set.Index, index_usize);
        const is_enabled = target.cpu.features.isEnabled(index);
        if (is_enabled) {
            // TODO some kind of "zig identifier escape" function rather than
            // unconditionally using @"" syntax
            try buffer.appendSlice("        .@\"");
            try buffer.appendSlice(feature.name);
            try buffer.appendSlice("\",\n");
        }
    }

    try buffer.writer().print(
        \\    }}),
        \\}};
        \\pub const os = Os{{
        \\    .tag = .{},
        \\    .version_range = .{{
    ,
        .{std.zig.fmtId(@tagName(target.os.tag))},
    );

    switch (target.os.getVersionRange()) {
        .none => try buffer.appendSlice(" .none = {} }\n"),
        .semver => |semver| try buffer.writer().print(
            \\ .semver = .{{
            \\        .min = .{{
            \\            .major = {},
            \\            .minor = {},
            \\            .patch = {},
            \\        }},
            \\        .max = .{{
            \\            .major = {},
            \\            .minor = {},
            \\            .patch = {},
            \\        }},
            \\    }}}},
            \\
        , .{
            semver.min.major,
            semver.min.minor,
            semver.min.patch,

            semver.max.major,
            semver.max.minor,
            semver.max.patch,
        }),
        .linux => |linux| try buffer.writer().print(
            \\ .linux = .{{
            \\        .range = .{{
            \\            .min = .{{
            \\                .major = {},
            \\                .minor = {},
            \\                .patch = {},
            \\            }},
            \\            .max = .{{
            \\                .major = {},
            \\                .minor = {},
            \\                .patch = {},
            \\            }},
            \\        }},
            \\        .glibc = .{{
            \\            .major = {},
            \\            .minor = {},
            \\            .patch = {},
            \\        }},
            \\    }}}},
            \\
        , .{
            linux.range.min.major,
            linux.range.min.minor,
            linux.range.min.patch,

            linux.range.max.major,
            linux.range.max.minor,
            linux.range.max.patch,

            linux.glibc.major,
            linux.glibc.minor,
            linux.glibc.patch,
        }),
        .windows => |windows| try buffer.writer().print(
            \\ .windows = .{{
            \\        .min = {s},
            \\        .max = {s},
            \\    }}}},
            \\
        ,
            .{ windows.min, windows.max },
        ),
    }
    try buffer.appendSlice("};\n");

    // This is so that compiler_rt and libc.zig libraries know whether they
    // will eventually be linked with libc. They make different decisions
    // about what to export depending on whether another libc will be linked
    // in. For example, compiler_rt will not export the __chkstk symbol if it
    // knows libc will provide it, and likewise c.zig will not export memcpy.
    const link_libc = comp.bin_file.options.link_libc or
        (comp.bin_file.options.skip_linker_dependencies and comp.bin_file.options.parent_compilation_link_libc);

    try buffer.writer().print(
        \\pub const object_format = ObjectFormat.{};
        \\pub const mode = Mode.{};
        \\pub const link_libc = {};
        \\pub const link_libcpp = {};
        \\pub const have_error_return_tracing = {};
        \\pub const valgrind_support = {};
        \\pub const position_independent_code = {};
        \\pub const position_independent_executable = {};
        \\pub const strip_debug_info = {};
        \\pub const code_model = CodeModel.{};
        \\
    , .{
        std.zig.fmtId(@tagName(comp.bin_file.options.object_format)),
        std.zig.fmtId(@tagName(comp.bin_file.options.optimize_mode)),
        link_libc,
        comp.bin_file.options.link_libcpp,
        comp.bin_file.options.error_return_tracing,
        comp.bin_file.options.valgrind,
        comp.bin_file.options.pic,
        comp.bin_file.options.pie,
        comp.bin_file.options.strip,
        std.zig.fmtId(@tagName(comp.bin_file.options.machine_code_model)),
    });

    if (comp.bin_file.options.is_test) {
        try buffer.appendSlice(
            \\pub var test_functions: []TestFn = undefined; // overwritten later
            \\
        );
        if (comp.test_evented_io) {
            try buffer.appendSlice(
                \\pub const test_io_mode = .evented;
                \\
            );
        } else {
            try buffer.appendSlice(
                \\pub const test_io_mode = .blocking;
                \\
            );
        }
    }

    return buffer.toOwnedSlice();
}

pub fn updateSubCompilation(sub_compilation: *Compilation) !void {
    try sub_compilation.update();

    // Look for compilation errors in this sub_compilation
    var errors = try sub_compilation.getAllErrorsAlloc();
    defer errors.deinit(sub_compilation.gpa);

    if (errors.list.len != 0) {
        for (errors.list) |full_err_msg| {
            switch (full_err_msg) {
                .src => |src| {
                    log.err("{s}:{d}:{d}: {s}\n", .{
                        src.src_path,
                        src.line + 1,
                        src.column + 1,
                        src.msg,
                    });
                },
                .plain => |plain| {
                    log.err("{s}", .{plain.msg});
                },
            }
        }
        return error.BuildingLibCObjectFailed;
    }
}

fn buildOutputFromZig(
    comp: *Compilation,
    src_basename: []const u8,
    output_mode: std.builtin.OutputMode,
    out: *?CRTFile,
) !void {
    const tracy = trace(@src());
    defer tracy.end();

    std.debug.assert(output_mode != .Exe);
    const special_sub = "std" ++ std.fs.path.sep_str ++ "special";
    const special_path = try comp.zig_lib_directory.join(comp.gpa, &[_][]const u8{special_sub});
    defer comp.gpa.free(special_path);

    var special_dir = try comp.zig_lib_directory.handle.openDir(special_sub, .{});
    defer special_dir.close();

    var root_pkg: Package = .{
        .root_src_directory = .{
            .path = special_path,
            .handle = special_dir,
        },
        .root_src_path = src_basename,
    };
    const root_name = src_basename[0 .. src_basename.len - std.fs.path.extension(src_basename).len];
    const target = comp.getTarget();
    const fixed_output_mode = if (target.cpu.arch.isWasm()) .Obj else output_mode;
    const bin_basename = try std.zig.binNameAlloc(comp.gpa, .{
        .root_name = root_name,
        .target = target,
        .output_mode = fixed_output_mode,
    });
    defer comp.gpa.free(bin_basename);

    const emit_bin = Compilation.EmitLoc{
        .directory = null, // Put it in the cache directory.
        .basename = bin_basename,
    };
    const sub_compilation = try Compilation.create(comp.gpa, .{
        .global_cache_directory = comp.global_cache_directory,
        .local_cache_directory = comp.global_cache_directory,
        .zig_lib_directory = comp.zig_lib_directory,
        .target = target,
        .root_name = root_name,
        .root_pkg = &root_pkg,
        .output_mode = fixed_output_mode,
        .thread_pool = comp.thread_pool,
        .libc_installation = comp.bin_file.options.libc_installation,
        .emit_bin = emit_bin,
        .optimize_mode = comp.compilerRtOptMode(),
        .link_mode = .Static,
        .function_sections = true,
        .want_sanitize_c = false,
        .want_stack_check = false,
        .want_red_zone = comp.bin_file.options.red_zone,
        .want_valgrind = false,
        .want_tsan = false,
        .want_pic = comp.bin_file.options.pic,
        .want_pie = comp.bin_file.options.pie,
        .emit_h = null,
        .strip = comp.compilerRtStrip(),
        .is_native_os = comp.bin_file.options.is_native_os,
        .is_native_abi = comp.bin_file.options.is_native_abi,
        .self_exe_path = comp.self_exe_path,
        .verbose_cc = comp.verbose_cc,
        .verbose_link = comp.bin_file.options.verbose_link,
        .verbose_tokenize = comp.verbose_tokenize,
        .verbose_ast = comp.verbose_ast,
        .verbose_ir = comp.verbose_ir,
        .verbose_llvm_ir = comp.verbose_llvm_ir,
        .verbose_cimport = comp.verbose_cimport,
        .verbose_llvm_cpu_features = comp.verbose_llvm_cpu_features,
        .clang_passthrough_mode = comp.clang_passthrough_mode,
        .skip_linker_dependencies = true,
        .parent_compilation_link_libc = comp.bin_file.options.link_libc,
    });
    defer sub_compilation.destroy();

    try sub_compilation.updateSubCompilation();

    assert(out.* == null);
    out.* = Compilation.CRTFile{
        .full_object_path = try sub_compilation.bin_file.options.emit.?.directory.join(comp.gpa, &[_][]const u8{
            sub_compilation.bin_file.options.emit.?.sub_path,
        }),
        .lock = sub_compilation.bin_file.toOwnedLock(),
    };
}

fn updateStage1Module(comp: *Compilation, main_progress_node: *std.Progress.Node) !void {
    const tracy = trace(@src());
    defer tracy.end();

    var arena_allocator = std.heap.ArenaAllocator.init(comp.gpa);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;

    // Here we use the legacy stage1 C++ compiler to compile Zig code.
    const mod = comp.bin_file.options.module.?;
    const directory = mod.zig_cache_artifact_directory; // Just an alias to make it shorter to type.
    const main_zig_file = try mod.root_pkg.root_src_directory.join(arena, &[_][]const u8{
        mod.root_pkg.root_src_path,
    });
    const zig_lib_dir = comp.zig_lib_directory.path.?;
    const builtin_zig_path = try directory.join(arena, &[_][]const u8{"builtin.zig"});
    const target = comp.getTarget();
    const id_symlink_basename = "stage1.id";
    const libs_txt_basename = "libs.txt";

    // We are about to obtain this lock, so here we give other processes a chance first.
    comp.releaseStage1Lock();

    // Unlike with the self-hosted Zig module, stage1 does not support incremental compilation,
    // so we input all the zig source files into the cache hash system. We're going to keep
    // the artifact directory the same, however, so we take the same strategy as linking
    // does where we have a file which specifies the hash of the output directory so that we can
    // skip the expensive compilation step if the hash matches.
    var man = comp.cache_parent.obtain();
    defer man.deinit();

    _ = try man.addFile(main_zig_file, null);
    {
        var local_arena = std.heap.ArenaAllocator.init(comp.gpa);
        defer local_arena.deinit();
        try addPackageTableToCacheHash(&man.hash, &local_arena, mod.root_pkg.table, .{ .files = &man });
    }
    man.hash.add(comp.bin_file.options.valgrind);
    man.hash.add(comp.bin_file.options.single_threaded);
    man.hash.add(target.os.getVersionRange());
    man.hash.add(comp.bin_file.options.dll_export_fns);
    man.hash.add(comp.bin_file.options.function_sections);
    man.hash.add(comp.bin_file.options.is_test);
    man.hash.add(comp.bin_file.options.emit != null);
    man.hash.add(mod.emit_h != null);
    if (mod.emit_h) |emit_h| {
        man.hash.addEmitLoc(emit_h);
    }
    man.hash.addOptionalEmitLoc(comp.emit_asm);
    man.hash.addOptionalEmitLoc(comp.emit_llvm_ir);
    man.hash.addOptionalEmitLoc(comp.emit_analysis);
    man.hash.addOptionalEmitLoc(comp.emit_docs);
    man.hash.add(comp.test_evented_io);
    man.hash.addOptionalBytes(comp.test_filter);
    man.hash.addOptionalBytes(comp.test_name_prefix);

    // Capture the state in case we come back from this branch where the hash doesn't match.
    const prev_hash_state = man.hash.peekBin();
    const input_file_count = man.files.items.len;

    const hit = man.hit() catch |err| {
        const i = man.failed_file_index orelse return err;
        const file_path = man.files.items[i].path orelse return err;
        fatal("unable to build stage1 zig object: {s}: {s}", .{ @errorName(err), file_path });
    };
    if (hit) {
        const digest = man.final();

        // We use an extra hex-encoded byte here to store some flags.
        var prev_digest_buf: [digest.len + 2]u8 = undefined;
        const prev_digest: []u8 = Cache.readSmallFile(
            directory.handle,
            id_symlink_basename,
            &prev_digest_buf,
        ) catch |err| blk: {
            log.debug("stage1 {s} new_digest={} error: {s}", .{ mod.root_pkg.root_src_path, digest, @errorName(err) });
            // Handle this as a cache miss.
            break :blk prev_digest_buf[0..0];
        };
        if (prev_digest.len >= digest.len + 2) hit: {
            if (!mem.eql(u8, prev_digest[0..digest.len], &digest))
                break :hit;

            log.debug("stage1 {s} digest={} match - skipping invocation", .{ mod.root_pkg.root_src_path, digest });
            var flags_bytes: [1]u8 = undefined;
            _ = std.fmt.hexToBytes(&flags_bytes, prev_digest[digest.len..]) catch {
                log.warn("bad cache stage1 digest: '{s}'", .{prev_digest});
                break :hit;
            };

            if (directory.handle.readFileAlloc(comp.gpa, libs_txt_basename, 10 * 1024 * 1024)) |libs_txt| {
                var it = mem.tokenize(libs_txt, "\n");
                while (it.next()) |lib_name| {
                    try comp.stage1AddLinkLib(lib_name);
                }
            } else |err| switch (err) {
                error.FileNotFound => {}, // That's OK, it just means 0 libs.
                else => {
                    log.warn("unable to read cached list of link libs: {s}", .{@errorName(err)});
                    break :hit;
                },
            }
            comp.stage1_lock = man.toOwnedLock();
            mod.stage1_flags = @bitCast(@TypeOf(mod.stage1_flags), flags_bytes[0]);
            return;
        }
        log.debug("stage1 {s} prev_digest={} new_digest={}", .{ mod.root_pkg.root_src_path, prev_digest, digest });
        man.unhit(prev_hash_state, input_file_count);
    }

    // We are about to change the output file to be different, so we invalidate the build hash now.
    directory.handle.deleteFile(id_symlink_basename) catch |err| switch (err) {
        error.FileNotFound => {},
        else => |e| return e,
    };

    const stage2_target = try arena.create(stage1.Stage2Target);
    stage2_target.* = .{
        .arch = @enumToInt(target.cpu.arch) + 1, // skip over ZigLLVM_UnknownArch
        .os = @enumToInt(target.os.tag),
        .abi = @enumToInt(target.abi),
        .is_native_os = comp.bin_file.options.is_native_os,
        .is_native_cpu = false, // Only true when bootstrapping the compiler.
        .llvm_cpu_name = if (target.cpu.model.llvm_name) |s| s.ptr else null,
        .llvm_cpu_features = comp.bin_file.options.llvm_cpu_features.?,
    };

    comp.stage1_cache_manifest = &man;

    const main_pkg_path = mod.root_pkg.root_src_directory.path orelse "";

    const stage1_module = stage1.create(
        @enumToInt(comp.bin_file.options.optimize_mode),
        main_pkg_path.ptr,
        main_pkg_path.len,
        main_zig_file.ptr,
        main_zig_file.len,
        zig_lib_dir.ptr,
        zig_lib_dir.len,
        stage2_target,
        comp.bin_file.options.is_test,
    ) orelse return error.OutOfMemory;

    const emit_bin_path = if (comp.bin_file.options.emit != null) blk: {
        const bin_basename = try std.zig.binNameAlloc(arena, .{
            .root_name = comp.bin_file.options.root_name,
            .target = target,
            .output_mode = .Obj,
        });
        break :blk try directory.join(arena, &[_][]const u8{bin_basename});
    } else "";
    if (mod.emit_h != null) {
        log.warn("-femit-h is not available in the stage1 backend; no .h file will be produced", .{});
    }
    const emit_h_path = try stage1LocPath(arena, mod.emit_h, directory);
    const emit_asm_path = try stage1LocPath(arena, comp.emit_asm, directory);
    const emit_llvm_ir_path = try stage1LocPath(arena, comp.emit_llvm_ir, directory);
    const emit_analysis_path = try stage1LocPath(arena, comp.emit_analysis, directory);
    const emit_docs_path = try stage1LocPath(arena, comp.emit_docs, directory);
    const stage1_pkg = try createStage1Pkg(arena, "root", mod.root_pkg, null);
    const test_filter = comp.test_filter orelse ""[0..0];
    const test_name_prefix = comp.test_name_prefix orelse ""[0..0];
    const subsystem = if (comp.bin_file.options.subsystem) |s|
        @intToEnum(stage1.TargetSubsystem, @enumToInt(s))
    else
        stage1.TargetSubsystem.Auto;
    stage1_module.* = .{
        .root_name_ptr = comp.bin_file.options.root_name.ptr,
        .root_name_len = comp.bin_file.options.root_name.len,
        .emit_o_ptr = emit_bin_path.ptr,
        .emit_o_len = emit_bin_path.len,
        .emit_h_ptr = emit_h_path.ptr,
        .emit_h_len = emit_h_path.len,
        .emit_asm_ptr = emit_asm_path.ptr,
        .emit_asm_len = emit_asm_path.len,
        .emit_llvm_ir_ptr = emit_llvm_ir_path.ptr,
        .emit_llvm_ir_len = emit_llvm_ir_path.len,
        .emit_analysis_json_ptr = emit_analysis_path.ptr,
        .emit_analysis_json_len = emit_analysis_path.len,
        .emit_docs_ptr = emit_docs_path.ptr,
        .emit_docs_len = emit_docs_path.len,
        .builtin_zig_path_ptr = builtin_zig_path.ptr,
        .builtin_zig_path_len = builtin_zig_path.len,
        .test_filter_ptr = test_filter.ptr,
        .test_filter_len = test_filter.len,
        .test_name_prefix_ptr = test_name_prefix.ptr,
        .test_name_prefix_len = test_name_prefix.len,
        .userdata = @ptrToInt(comp),
        .root_pkg = stage1_pkg,
        .code_model = @enumToInt(comp.bin_file.options.machine_code_model),
        .subsystem = subsystem,
        .err_color = @enumToInt(comp.color),
        .pic = comp.bin_file.options.pic,
        .pie = comp.bin_file.options.pie,
        .link_libc = comp.bin_file.options.link_libc,
        .link_libcpp = comp.bin_file.options.link_libcpp,
        .strip = comp.bin_file.options.strip,
        .is_single_threaded = comp.bin_file.options.single_threaded,
        .dll_export_fns = comp.bin_file.options.dll_export_fns,
        .link_mode_dynamic = comp.bin_file.options.link_mode == .Dynamic,
        .valgrind_enabled = comp.bin_file.options.valgrind,
        .tsan_enabled = comp.bin_file.options.tsan,
        .function_sections = comp.bin_file.options.function_sections,
        .enable_stack_probing = comp.bin_file.options.stack_check,
        .red_zone = comp.bin_file.options.red_zone,
        .enable_time_report = comp.time_report,
        .enable_stack_report = comp.stack_report,
        .test_is_evented = comp.test_evented_io,
        .verbose_tokenize = comp.verbose_tokenize,
        .verbose_ast = comp.verbose_ast,
        .verbose_ir = comp.verbose_ir,
        .verbose_llvm_ir = comp.verbose_llvm_ir,
        .verbose_cimport = comp.verbose_cimport,
        .verbose_llvm_cpu_features = comp.verbose_llvm_cpu_features,
        .main_progress_node = main_progress_node,
        .have_c_main = false,
        .have_winmain = false,
        .have_wwinmain = false,
        .have_winmain_crt_startup = false,
        .have_wwinmain_crt_startup = false,
        .have_dllmain_crt_startup = false,
    };

    const inferred_lib_start_index = comp.bin_file.options.system_libs.count();
    stage1_module.build_object();

    if (comp.bin_file.options.system_libs.count() > inferred_lib_start_index) {
        // We need to save the inferred link libs to the cache, otherwise if we get a cache hit
        // next time we will be missing these libs.
        var libs_txt = std.ArrayList(u8).init(arena);
        for (comp.bin_file.options.system_libs.items()[inferred_lib_start_index..]) |entry| {
            try libs_txt.writer().print("{s}\n", .{entry.key});
        }
        try directory.handle.writeFile(libs_txt_basename, libs_txt.items);
    }

    mod.stage1_flags = .{
        .have_c_main = stage1_module.have_c_main,
        .have_winmain = stage1_module.have_winmain,
        .have_wwinmain = stage1_module.have_wwinmain,
        .have_winmain_crt_startup = stage1_module.have_winmain_crt_startup,
        .have_wwinmain_crt_startup = stage1_module.have_wwinmain_crt_startup,
        .have_dllmain_crt_startup = stage1_module.have_dllmain_crt_startup,
    };

    stage1_module.destroy();

    const digest = man.final();

    // Update the small file with the digest. If it fails we can continue; it only
    // means that the next invocation will have an unnecessary cache miss.
    const stage1_flags_byte = @bitCast(u8, mod.stage1_flags);
    log.debug("stage1 {s} final digest={} flags={x}", .{
        mod.root_pkg.root_src_path, digest, stage1_flags_byte,
    });
    var digest_plus_flags: [digest.len + 2]u8 = undefined;
    digest_plus_flags[0..digest.len].* = digest;
    assert(std.fmt.formatIntBuf(digest_plus_flags[digest.len..], stage1_flags_byte, 16, false, .{
        .width = 2,
        .fill = '0',
    }) == 2);
    log.debug("saved digest + flags: '{s}' (byte = {}) have_winmain_crt_startup={}", .{
        digest_plus_flags, stage1_flags_byte, mod.stage1_flags.have_winmain_crt_startup,
    });
    Cache.writeSmallFile(directory.handle, id_symlink_basename, &digest_plus_flags) catch |err| {
        log.warn("failed to save stage1 hash digest file: {s}", .{@errorName(err)});
    };
    // Failure here only means an unnecessary cache miss.
    man.writeManifest() catch |err| {
        log.warn("failed to write cache manifest when linking: {s}", .{@errorName(err)});
    };
    // We hang on to this lock so that the output file path can be used without
    // other processes clobbering it.
    comp.stage1_lock = man.toOwnedLock();
}

fn stage1LocPath(arena: *Allocator, opt_loc: ?EmitLoc, cache_directory: Directory) ![]const u8 {
    const loc = opt_loc orelse return "";
    const directory = loc.directory orelse cache_directory;
    return directory.join(arena, &[_][]const u8{loc.basename});
}

fn createStage1Pkg(
    arena: *Allocator,
    name: []const u8,
    pkg: *Package,
    parent_pkg: ?*stage1.Pkg,
) error{OutOfMemory}!*stage1.Pkg {
    const child_pkg = try arena.create(stage1.Pkg);

    const pkg_children = blk: {
        var children = std.ArrayList(*stage1.Pkg).init(arena);
        var it = pkg.table.iterator();
        while (it.next()) |entry| {
            try children.append(try createStage1Pkg(arena, entry.key, entry.value, child_pkg));
        }
        break :blk children.items;
    };

    const src_path = try pkg.root_src_directory.join(arena, &[_][]const u8{pkg.root_src_path});

    child_pkg.* = .{
        .name_ptr = name.ptr,
        .name_len = name.len,
        .path_ptr = src_path.ptr,
        .path_len = src_path.len,
        .children_ptr = pkg_children.ptr,
        .children_len = pkg_children.len,
        .parent = parent_pkg,
    };
    return child_pkg;
}

pub fn build_crt_file(
    comp: *Compilation,
    root_name: []const u8,
    output_mode: std.builtin.OutputMode,
    c_source_files: []const Compilation.CSourceFile,
) !void {
    const tracy = trace(@src());
    defer tracy.end();

    const target = comp.getTarget();
    const basename = try std.zig.binNameAlloc(comp.gpa, .{
        .root_name = root_name,
        .target = target,
        .output_mode = output_mode,
    });
    errdefer comp.gpa.free(basename);

    // TODO: This is extracted into a local variable to work around a stage1 miscompilation.
    const emit_bin = Compilation.EmitLoc{
        .directory = null, // Put it in the cache directory.
        .basename = basename,
    };
    const sub_compilation = try Compilation.create(comp.gpa, .{
        .local_cache_directory = comp.global_cache_directory,
        .global_cache_directory = comp.global_cache_directory,
        .zig_lib_directory = comp.zig_lib_directory,
        .target = target,
        .root_name = root_name,
        .root_pkg = null,
        .output_mode = output_mode,
        .thread_pool = comp.thread_pool,
        .libc_installation = comp.bin_file.options.libc_installation,
        .emit_bin = emit_bin,
        .optimize_mode = comp.compilerRtOptMode(),
        .want_sanitize_c = false,
        .want_stack_check = false,
        .want_red_zone = comp.bin_file.options.red_zone,
        .want_valgrind = false,
        .want_tsan = false,
        .want_pic = comp.bin_file.options.pic,
        .want_pie = comp.bin_file.options.pie,
        .emit_h = null,
        .strip = comp.compilerRtStrip(),
        .is_native_os = comp.bin_file.options.is_native_os,
        .is_native_abi = comp.bin_file.options.is_native_abi,
        .self_exe_path = comp.self_exe_path,
        .c_source_files = c_source_files,
        .verbose_cc = comp.verbose_cc,
        .verbose_link = comp.bin_file.options.verbose_link,
        .verbose_tokenize = comp.verbose_tokenize,
        .verbose_ast = comp.verbose_ast,
        .verbose_ir = comp.verbose_ir,
        .verbose_llvm_ir = comp.verbose_llvm_ir,
        .verbose_cimport = comp.verbose_cimport,
        .verbose_llvm_cpu_features = comp.verbose_llvm_cpu_features,
        .clang_passthrough_mode = comp.clang_passthrough_mode,
        .skip_linker_dependencies = true,
        .parent_compilation_link_libc = comp.bin_file.options.link_libc,
    });
    defer sub_compilation.destroy();

    try sub_compilation.updateSubCompilation();

    try comp.crt_files.ensureCapacity(comp.gpa, comp.crt_files.count() + 1);

    comp.crt_files.putAssumeCapacityNoClobber(basename, .{
        .full_object_path = try sub_compilation.bin_file.options.emit.?.directory.join(comp.gpa, &[_][]const u8{
            sub_compilation.bin_file.options.emit.?.sub_path,
        }),
        .lock = sub_compilation.bin_file.toOwnedLock(),
    });
}

pub fn stage1AddLinkLib(comp: *Compilation, lib_name: []const u8) !void {
    // Avoid deadlocking on building import libs such as kernel32.lib
    // This can happen when the user uses `build-exe foo.obj -lkernel32` and then
    // when we create a sub-Compilation for zig libc, it also tries to build kernel32.lib.
    if (comp.bin_file.options.skip_linker_dependencies) return;

    // This happens when an `extern "foo"` function is referenced by the stage1 backend.
    // If we haven't seen this library yet and we're targeting Windows, we need to queue up
    // a work item to produce the DLL import library for this.
    const gop = try comp.bin_file.options.system_libs.getOrPut(comp.gpa, lib_name);
    if (!gop.found_existing and comp.getTarget().os.tag == .windows) {
        try comp.work_queue.writeItem(.{
            .windows_import_lib = comp.bin_file.options.system_libs.count() - 1,
        });
    }
}

/// This decides the optimization mode for all zig-provided libraries, including
/// compiler-rt, libcxx, libc, libunwind, etc.
pub fn compilerRtOptMode(comp: Compilation) std.builtin.Mode {
    if (comp.debug_compiler_runtime_libs) {
        return comp.bin_file.options.optimize_mode;
    }
    switch (comp.bin_file.options.optimize_mode) {
        .Debug, .ReleaseSafe => return target_util.defaultCompilerRtOptimizeMode(comp.getTarget()),
        .ReleaseFast => return .ReleaseFast,
        .ReleaseSmall => return .ReleaseSmall,
    }
}

/// This decides whether to strip debug info for all zig-provided libraries, including
/// compiler-rt, libcxx, libc, libunwind, etc.
pub fn compilerRtStrip(comp: Compilation) bool {
    if (comp.debug_compiler_runtime_libs) {
        return comp.bin_file.options.strip;
    } else {
        return true;
    }
}
