package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*

@Test
class JsonChainTest {
    private let json = Json(str: #"
    {
        "array": [1, 2, 3],
        "struct": {
            "key": "value"
        }
    }"#)

    @TestCase
    func testExist() {        
        @Expect(json["struct"]["key"].stringValue, "value")
    }

    @TestCase
    func testNotExist() {        
        @Expect(json["a"]["b"][99]["c"]["*"]["@"][-1].stringValue, "")
    }
}
