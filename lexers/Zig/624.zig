const std = @import("std");
const expect = std.testing.expect;

const TestContext = struct {
    server_context: *ListenerContext,
};

const ListenerContext = struct {
    context_alloc: *ContextAllocator,
};

const ContextAllocator = MemoryPool(TestContext);

fn MemoryPool(comptime T: type) type {
    return struct {
        n: usize,
    };
}

test "foo" {
    var allocator = ContextAllocator{ .n = 10 };
    expect(allocator.n == 10);
}
