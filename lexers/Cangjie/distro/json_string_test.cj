package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*

@Test
class JsonStringTest {
    @TestCase
    func testStringOption() {
        @Expect(Json(str: #"{ "string" : "123" }"#)["string"].string, Option.Some("123"))
    }

    @TestCase
    func testString() {
        @Expect(Json(str: #"{ "string" : "123" }"#)["string"].stringValue, "123")
    }

    @TestCase
    func testStringDefault() {
        @Expect(Json(str: #"{}"#)["string"].stringValue, "")
    }

    @TestCase
    func testStringOptionNone() {
        @Expect(Json(str: #"{}"#)["string"].string, Option.None)
    }
}
