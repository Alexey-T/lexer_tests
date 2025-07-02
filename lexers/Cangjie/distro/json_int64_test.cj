package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*

@Test
class JsonInt64Test {
    @TestCase
    func testInt64Option() {
        @Expect(Json(str: #"{ "int64" : 123 }"#)["int64"].int64, Option.Some(123))
    }

    @TestCase
    func testInt64() {
        @Expect(Json(str: #"{ "int64" : 123 }"#)["int64"].int64Value, 123)
    }

    @TestCase
    func testInt64Default() {
        @Expect(Json(str: #"{}"#)["int64"].int64Value, 0)
    }

    @TestCase
    func testInt64OptionNone() {
        @Expect(Json(str: #"{}"#)["int64"].int64, Option.None)
    }
}
