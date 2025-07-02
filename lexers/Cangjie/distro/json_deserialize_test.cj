package SimpleJsonReader

import std.unittest.*
import std.unittest.testmacro.*
import serialization.serialization.*

private struct Person <: Serializable<Person> {

    let name: String

    init(name!: String) {
        this.name = name
    }

    static public func deserialize(dm: DataModel): Person {
        let dms = match(dm) {
            case d: DataModelStruct => d
            case _ => throw Exception("This data is not DataModelStruct")
        }
        return Person(name: String.deserialize(dms.get("name")))
    }
    
    public func serialize(): DataModel {
        let dms = DataModelStruct()
        dms.add(field("name", this.name))
        return dms
    }
}

@Test
class JsonDeserializeTest {

    @TestCase
    func test() {
        let jsonStr = #"
        {
            "name": "Arror"
        }"#
        let json = Json(str: jsonStr)
        let person = json.deserialize<Person>()
        @Expect(person.isSome())
    }
}
