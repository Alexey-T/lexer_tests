const std = @import("std");
const tests = @import("tests.zig");

pub fn addCases(cases: *tests.StandaloneContext) void {
    cases.add("test/standalone/hello_world/hello.zig");
    cases.addC("test/standalone/hello_world/hello_libc.zig");
    cases.add("test/standalone/cat/main.zig");
    cases.add("test/standalone/guess_number/main.zig");
    cases.add("test/standalone/main_return_error/error_u8.zig");
    cases.add("test/standalone/main_return_error/error_u8_non_zero.zig");
    cases.addBuildFile("test/standalone/main_pkg_path/build.zig");
    cases.addBuildFile("test/standalone/shared_library/build.zig");
    cases.addBuildFile("test/standalone/mix_o_files/build.zig");
    cases.addBuildFile("test/standalone/global_linkage/build.zig");
    cases.addBuildFile("test/standalone/static_c_lib/build.zig");
    cases.addBuildFile("test/standalone/issue_339/build.zig");
    cases.addBuildFile("test/standalone/issue_794/build.zig");
    cases.addBuildFile("test/standalone/issue_5825/build.zig");
    cases.addBuildFile("test/standalone/pkg_import/build.zig");
    cases.addBuildFile("test/standalone/use_alias/build.zig");
    cases.addBuildFile("test/standalone/brace_expansion/build.zig");
    cases.addBuildFile("test/standalone/empty_env/build.zig");
    cases.addBuildFile("test/standalone/issue_7030/build.zig");
    if (std.Target.current.os.tag != .wasi) {
        cases.addBuildFile("test/standalone/load_dynamic_library/build.zig");
    }
    if (std.Target.current.cpu.arch == .x86_64) { // TODO add C ABI support for other architectures
        cases.addBuildFile("test/stage1/c_abi/build.zig");
    }
}
