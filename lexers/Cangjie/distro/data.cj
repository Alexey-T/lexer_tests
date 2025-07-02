package tokenizer
import encoding.json.stream.*
import std.collection.ArrayList
import std.io.ByteArrayStream

public enum SamplingMethod {
  Greedy | Top_p | Top_k
}

public struct ModelState {
  public var code: Int64
  public var is_end: Bool
  public var message: String = ""
  public init(
    code!: Int64 = 200,
    is_end!: Bool = false,
    message!: String = ""
  ) {
    this.code = code
    this.is_end = is_end
    this.message = message
  }
}

public struct FunctionCall <: JsonDeserializable<FunctionCall> & JsonSerializable {
  public let name: String
  public let arguments: String

  public init(name: String, arguments: String) {
    this.name = name
    this.arguments = arguments
  }

  public static func fromJson(r: JsonReader): FunctionCall {
    var temp_name: String = ""
    var temp_arguments: String = ""
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
              let n = r.readName()
              match (n) {
                  case "name" => temp_name = r.readValue<String>()
                  case "arguments" => temp_arguments = r.readValue<String>()
                  case unkow => println("unkow key ${unkow}")
              }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for FunctionCall")
      }
    }
    return FunctionCall(temp_name, temp_arguments)
  }

  public func toJson(w: JsonWriter) {
    w.startObject()
    w.writeName("name").writeValue<String>(this.name)
    w.writeName("arguments").writeValue<String>(this.arguments)
    w.endObject()
    w.flush()
  }
}

public enum RoleType {
  User | Assistant | System | Function | NULL
}

public func role_type_to_str(role: RoleType): Option<String> {
  return match(role) {
    case RoleType.User => Some("user")
    case RoleType.Assistant => Some("assistant")
    case RoleType.System => Some("system")
    case RoleType.Function => Some("function")
    case RoleType.NULL => None
  }
}

public func str_to_role_type(role_option_str: Option<String>): RoleType {
  return match(role_option_str) {
    case Some(role_option) => 
      match (role_option) {
        case "user" => RoleType.User
        case "assistant" => RoleType.Assistant
        case "system" => RoleType.System
        case "function" => RoleType.Function
        case x => throw Exception("unknow enum ${x} for RoleType")
      }
    case None => RoleType.NULL
  }
}


public struct Message<: JsonDeserializable<Message> & JsonSerializable {
  public let role: RoleType
  public var content: String
  public let function_call: Option<FunctionCall>

  public init(role: RoleType, content: String) {
    this.role = role
    this.content = content
    this.function_call = None
  }

  public init(role: RoleType, content: String, function_call: Option<FunctionCall>) {
    this.role = role
    this.content = content
    this.function_call = function_call
  }

  public static func fromJson(r: JsonReader): Message {
    var temp_role: Option<String> = None // role may null in stream chat
    var temp_content: String = ""
    var temp_functional_call: Option<FunctionCall> = None
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
              let n = r.readName()
              match (n) {
                  case "role" => temp_role = r.readValue<Option<String>>()
                  case "content" => temp_content = r.readValue<String>()
                  case "function_call" => temp_functional_call = r.readValue<Option<FunctionCall>>()
                  case unkow => println("unkow key ${unkow}")
              }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for Message")
      }
    }
    let role_type: RoleType = str_to_role_type(temp_role)
    return Message(role_type, temp_content)
  }

  public func toJson(w: JsonWriter) {
    w.startObject()
    w.writeName("role").writeValue<Option<String>>(role_type_to_str(this.role))
    w.writeName("content").writeValue<String>(this.content)
    w.endObject()
    w.flush()
  }
}