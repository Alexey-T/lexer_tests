package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*

@Test
class JsonBoolTest {
    @TestCase
    func testBoolOptionTrue() {
        @Expect(Json(str: #"{ "true" : true }"#)["true"].bool, Option.Some(true))
    }

    @TestCase
    func testBoolTrue() {
        @Expect(Json(str: #"{ "true" : true }"#)["true"].boolValue, true)
    }

    @TestCase
    func testBoolFalse() {
        @Expect(Json(str: #"{ "false" : false }"#)["false"].boolValue, false)
    }

    @TestCase
    func testBoolOptionFalse() {
        @Expect(Json(str: #"{ "false" : false }"#)["false"].boolValue, Option.Some(false))
    }

    @TestCase
    func testBoolDefault() {
        @Expect(Json(str: #"{}"#)["key"].boolValue, false)
    }

    @TestCase
    func testBoolOptionNone() {
        @Expect(Json(str: #"{}"#)["key"].bool, Option.None)
    }
}
