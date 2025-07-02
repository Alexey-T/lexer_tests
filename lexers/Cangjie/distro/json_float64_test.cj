package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*

@Test
class JsonFloat64Test {
    @TestCase
    func testFloat64Option() {
        @Expect(Json(str: #"{ "float64" : 123.0 }"#)["float64"].float64, Option.Some(123.0))
    }

    @TestCase
    func testFloat64() {
        @Expect(Json(str: #"{ "float64" : 123.0 }"#)["float64"].float64Value, 123.0)
    }

    @TestCase
    func testFloat64Default() {
        @Expect(Json(str: #"{}"#)["float64"].float64Value, 0.0)
    }

    @TestCase
    func testFloat64OptionNone() {
        @Expect(Json(str: #"{}"#)["float64"].float64, Option.None)
    }
}
