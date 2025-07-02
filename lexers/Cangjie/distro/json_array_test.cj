package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*
import encoding.json.*

@Test
class JsonArrayTest {

    @TestCase
    func test() {
        let jsonStr = #"
        {
            "array": [1, "2", 3.0, {"key": "value"}]
        }
        "#
        let json = Json(str: jsonStr)
        @Expect(json["array"].arrayValue.size, 4)
        @Expect(json["array"].arrayValue[3]["key"].stringValue, "value")
    }
}