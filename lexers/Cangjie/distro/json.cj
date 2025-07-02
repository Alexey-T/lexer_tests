package SimpleJsonReader

import encoding.json.*
import std.collection.*
import serialization.serialization.*

public struct Json {
    private let value: JsonValue

    private init(value!: JsonValue) {
        this.value = value
    }

    public init(str!: String) {
        var reval: JsonValue
        try {
            reval = JsonValue.fromStr(str)
        } catch (e: Exception) {
            reval = JsonNull()
        }
        this.value = reval
    }

    public func deserialize<T>(): Option<T> where T <: Serializable<T> {
        try {
            return T.deserialize(this.dataModel)
        } catch (_: Exception) {
            return Option.None
        }
    }

    public operator func [](arg: Int): Json {
        try {
            match (this.value.kind()) {
                case JsonKind.JsArray => Json(value: this.value.asArray()[arg])
                case _ => return Json(value: JsonNull())
            }
        } catch (_: Exception) {
            return Json(value: JsonNull())
        }
    }

    public operator func [](arg: String): Json {
        try {
            match (this.value.kind()) {
                case JsonKind.JsObject => Json(value: this.value.asObject()[arg])
                case _ => return Json(value: JsonNull())
            }
        } catch (_: Exception) {
            return Json(value: JsonNull())
        }
    }

    public prop array: Option<Array<Json>> {
        get() {
            match (this.value.kind()) {
                case JsonKind.JsArray => return collectArray(
                    this.value.asArray().getItems() |> map {e => Json(value: e)})
                case _ => return Option.None
            }
        }
    }

    public prop arrayValue: Array<Json> {
        get() {
            match (this.array) {
                case Option.Some(v) => return v
                case Option.None => return Array()
            }
        }
    }

    public prop dataModel: DataModel {
        get() {
            return DataModel.fromJson(this.value)
        }
    }

    public prop bool: Option<Bool> {
        get() {
            match (this.value.kind()) {
                case JsonKind.JsBool => return Option.Some(this.value.asBool().getValue())
                case _ => return Option.None
            }
        }
    }

    public prop boolValue: Bool {
        get() {
            match (this.bool) {
                case Option.Some(v) => return v
                case Option.None => return false
            }
        }
    }

    public prop int64: Option<Int64> {
        get() {
            match (this.value.kind()) {
                case JsonKind.JsInt => return Option.Some(this.value.asInt().getValue())
                case _ => return Option.None
            }
        }
    }

    public prop int64Value: Int64 {
        get() {
            match (this.int64) {
                case Option.Some(v) => return v
                case Option.None => return 0
            }
        }
    }

    public prop float64: Option<Float64> {
        get() {
            match (this.value.kind()) {
                case JsonKind.JsFloat => return Option.Some(this.value.asFloat().getValue())
                case _ => return Option.None
            }
        }
    }

    public prop float64Value: Float64 {
        get() {
            match (this.float64) {
                case Option.Some(v) => return v
                case Option.None => return 0.0
            }
        }
    }

    public prop string: Option<String> {
        get() {
            match (this.value.kind()) {
                case JsonKind.JsString => return Option.Some(this.value.asString().getValue())
                case _ => return Option.None
            }
        }
    }

    public prop stringValue: String {
        get() {
            match (this.string) {
                case Option.Some(v) => return v
                case Option.None => return ""
            }
        }
    }
}
