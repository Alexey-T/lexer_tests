package openai_chat
import encoding.json.stream.*
import net.http.ClientBuilder
import net.http.HttpHeaders
import net.http.HttpRequestBuilder
import net.tls.TlsClientConfig
import net.tls.CertificateVerifyMode
import std.collection.ArrayList
import std.io.ByteArrayStream
import std.time.Duration

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

public struct ChatRequest <: JsonSerializable {
  private let model: String
  private let messages: ArrayList<Message>
  private let max_tokens: Int64
  private let temperature: Float64
  private let top_p: Float64
  private let n: Int32
  private let stream: Bool

  public init(
    model: String,
    messages: ArrayList<Message>,
    max_tokens: Int64,
    temperature: Float64,
    top_p: Float64,
    n: Int32,
    stream: Bool
  ) {
    // construction function with messages
    this.model = model
    this.messages = messages
    this.max_tokens = max_tokens
    this.temperature = temperature
    this.top_p = top_p
    this.n = n
    this.stream = stream
  }

  public init(
    model: String,
    prompt: String,
    history: ArrayList<(String, String)>,
    system_prompt: String,
    max_tokens: Int64,
    temperature: Float64,
    top_p: Float64,
    n: Int32,
    stream: Bool
  ){
    // construction function with prompt and system_prompt
    this.model = model
    this.messages = ArrayList<Message>([
      Message(RoleType.System, system_prompt)
    ])
    for ((use_msg, bot_msg) in history) {
      this.messages.append(Message(RoleType.User, use_msg))
      this.messages.append(Message(RoleType.Assistant, bot_msg))
    }
    this.messages.append(Message(RoleType.User, prompt))
    this.max_tokens = max_tokens
    this.temperature = temperature
    this.top_p = top_p
    this.n = n
    this.stream = stream
  }

  public init(
    model: String,
    prompt: String,
    history: ArrayList<(String, String)>,
    system_prompt: String,
    stream: Bool
  ){
    // construction function with prompt and default arguments
    this.model = model
    this.messages = ArrayList<Message>([
      Message(RoleType.System, system_prompt)
    ])
    for ((use_msg, bot_msg) in history) {
      this.messages.append(Message(RoleType.User, use_msg))
      this.messages.append(Message(RoleType.Assistant, bot_msg))
    }
    this.messages.append(Message(RoleType.User, prompt))
    this.max_tokens = 2000
    this.temperature = 0.7
    this.top_p = 1.0
    this.n = 1
    this.stream = stream
  }

  public func toJson(w: JsonWriter) {
    w.startObject()
    w.writeName("model").writeValue<String>(this.model)
    w.writeName("messages").writeValue<ArrayList<Message>>(this.messages)
    w.writeName("max_tokens").writeValue<Int64>(this.max_tokens)
    w.writeName("temperature").writeValue<Float64>(this.temperature)
    w.writeName("top_p").writeValue<Float64>(this.top_p)
    w.writeName("n").writeValue<Int32>(this.n)
    w.writeName("stream").writeValue<Bool>(this.stream)
    w.endObject()
    w.flush()
  }
}


public struct Choice <: JsonDeserializable<Choice> & JsonSerializable {
  public let index: Int32
  public let message: Option<Message>
  public let delta: Option<Message>
  public let finish_reason: Option<String>
  public let logprobs: Option<Float64> // dashscope for qwen need

  public init(
    index: Int32,
    message: Option<Message>,
    delta: Option<Message>,
    finish_reason: Option<String>,
    logprobs: Option<Float64>
  ) {
    this.index = index
    this.message = message
    this.delta = delta
    this.finish_reason = finish_reason
    this.logprobs = logprobs
  }

  public static func fromJson(r: JsonReader): Choice {
    var temp_index: Int32 = -1
    var temp_message: Option<Message> = None
    var temp_delta: Option<Message> = None
    var temp_finish_reason: Option<String> = None
    var temp_logprobs: Option<Float64> = None
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
              let n = r.readName()
              match (n) {
                  case "index" => temp_index = r.readValue<Int32>()
                  case "message" => temp_message = r.readValue<Option<Message>>()
                  case "delta" => temp_delta = r.readValue<Option<Message>>()
                  case "finish_reason" => temp_finish_reason = r.readValue<Option<String>>()
                  case "logprobs" => temp_logprobs = r.readValue<Option<Float64>>()
                  case unkow => println("unkow key ${unkow}")
              }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for Choice")
      }
    }
    return Choice(temp_index, temp_message, temp_delta, temp_finish_reason, temp_logprobs)
  }

  public func toJson(w: JsonWriter) {
    w.startObject()
    w.writeName("index").writeValue<Int32>(this.index)
    w.writeName("message").writeValue<Option<Message>>(this.message)
    w.writeName("delta").writeValue<Option<Message>>(this.delta)
    w.writeName("finish_reason").writeValue<Option<String>>(this.finish_reason)
    w.writeName("logprobs").writeValue<Option<Float64>>(this.logprobs)
    w.endObject()
    w.flush()
  }
}

public struct Usage <: JsonDeserializable<Usage> & JsonSerializable {
  public let prompt_tokens: UInt64
  public let completion_tokens: UInt64
  public let total_tokens: UInt64

  public init(prompt_tokens: UInt64, completion_tokens: UInt64, total_tokens: UInt64) {
    this.prompt_tokens = prompt_tokens
    this.completion_tokens = completion_tokens
    this.total_tokens = total_tokens
  }

  public static func fromJson(r: JsonReader): Usage {
    var temp_prompt_tokens: UInt64 = 0
    var temp_completion_tokens: UInt64 = 0
    var temp_total_tokens: UInt64 = 0
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
              let n = r.readName()
              match (n) {
                  case "prompt_tokens" => temp_prompt_tokens = r.readValue<UInt64>()
                  case "completion_tokens" => temp_completion_tokens = r.readValue<UInt64>()
                  case "total_tokens" => temp_total_tokens = r.readValue<UInt64>()
                  case unkow => println("unkow key ${unkow}")
              }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for Usage")
      }
    }
    return Usage(temp_prompt_tokens, temp_completion_tokens, temp_total_tokens)
  }

  public func toJson(w: JsonWriter) {
    w.startObject()
    w.writeName("prompt_tokens").writeValue<UInt64>(this.prompt_tokens)
    w.writeName("completion_tokens").writeValue<UInt64>(this.completion_tokens)
    w.writeName("total_tokens").writeValue<UInt64>(this.total_tokens)
    w.endObject()
    w.flush()
  }
}

public struct ChatResponse <: JsonDeserializable<ChatResponse> {
  // some api names `id`, and some names `request_id`
  public let id: Option<String>
  public let request_id: Option<String>
  public let system_fingerprint: Option<String>
  public let model: String
  public let object: String
  public let created: UInt64
  public let choices: ArrayList<Choice>
  public let usage: Option<Usage>

  public init(
    id: Option<String>,
    request_id: Option<String>,
    system_fingerprint: Option<String>,
    model: String,
    object: String,
    created: UInt64,
    choices: ArrayList<Choice>,
    usage: Option<Usage>
  ) {
    this.id = id
    this.request_id = request_id
    this.system_fingerprint = system_fingerprint
    this.model = model
    this.object = object
    this.created = created
    this.choices = choices
    this.usage = usage
  }

  public static func fromJson(r: JsonReader): ChatResponse {
    var temp_id: Option<String> = None
    var temp_request_id: Option<String> = None
    var temp_system_fingerprint: Option<String> = None
    var temp_model: String = ""
    var temp_object: String = ""
    var temp_created: UInt64 = 0
    var temp_choices: ArrayList<Choice> = ArrayList<Choice>([])
    var temp_usage: Option<Usage> = None
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
              let n = r.readName()
              match (n) {
                  case "id" => temp_id = r.readValue<Option<String>>()
                  case "request_id" => temp_request_id = r.readValue<Option<String>>()
                  case "system_fingerprint" => temp_system_fingerprint = r.readValue<Option<String>>()
                  case "model" => temp_model = r.readValue<String>()
                  case "object" => temp_object = r.readValue<String>()
                  case "created" => temp_created = r.readValue<UInt64>()
                  case "choices" => temp_choices = r.readValue<ArrayList<Choice>>()
                  case "usage" => temp_usage = r.readValue<Option<Usage>>()
                  case unkow => println("unkow key ${unkow}")
              }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ChatResponse")
      }
    }
    return ChatResponse(
      temp_id,
      temp_request_id,
      temp_system_fingerprint,
      temp_model,
      temp_object,
      temp_created,
      temp_choices,
      temp_usage
    )
  }
}

public func get_domain(
  url: String
): String {
  var temp_url = url
  if (temp_url.startsWith("https://")) {
    temp_url = temp_url["https://".size..]
  } else if (temp_url.startsWith("http://")) {
    temp_url = temp_url["http://".size..]
  }
  let domain: String = temp_url.split("?")[0].split("/")[0]
  return domain
}

public func build_http_client(
  prompt: String,
  env_info: EnvInfo,
  history: ArrayList<(String, String)>,
  stream!: Bool
){
  // prepare input data
  var array_stream = ByteArrayStream()
  let json_writer = JsonWriter(array_stream)
  let chat_res = ChatRequest(
    env_info.model,
    prompt,
    history,
    env_info.system_prompt,
    stream
  )
  chat_res.toJson(json_writer)
  let post_data: Array<UInt8> = array_stream.readToEnd()
  var headers: HttpHeaders = HttpHeaders()
  headers.add("Authorization", "Bearer ${env_info.api_key}")
  headers.add("Content-Type", "application/json")
  if (stream) {
    headers.add("Accept", "text/event-stream")
  }
  let request = HttpRequestBuilder()
    .url(env_info.base_url)
    .method("POST")
    .body(post_data)
    .readTimeout(Duration.second * 120)
    .addHeaders(headers)
    .build()
  let client = if (env_info.base_url.startsWith("https")) {
    var tls_client_config = TlsClientConfig()
    tls_client_config.verifyMode = CertificateVerifyMode.TrustAll
    tls_client_config.domain = get_domain(env_info.base_url)
    ClientBuilder()
      .tlsConfig(tls_client_config)
      .build()
  } else {
    ClientBuilder().build()
  }
  return (request, client)
}

public func chat(
  prompt: String,
  env_info: EnvInfo,
  history: ArrayList<(String, String)>
): Option<String> {
  let (request, client) = build_http_client(
    prompt,
    env_info,
    history,
    stream: false
  ) 
  var result_message: Option<String> = None
  var res_text = ""
  try {
    // call api
    let response = client.send(
      request
    )
    // read result (support max revice 100k data)
    let buffer = Array<Byte>(102400, item: 0)
    let length = response.body.read(buffer)
    res_text = String.fromUtf8(buffer[..length])
    // println("res_text: ${res_text}")
    var input_stream = ByteArrayStream()
    input_stream.write(res_text.toArray())
    // convert text to ChatResponse object
    let json_reader = JsonReader(input_stream)
    let res_object = ChatResponse.fromJson(json_reader)
    let choices: ArrayList<Choice> = res_object.choices
    if (choices.size > 0) {
      let message = choices[0].message.getOrThrow()
      // println("message: ${message.content}")
      result_message = Some(message.content)
      
    } else {
      println("can't found any response")
    }
  } catch (e: Exception) {
    println("ERROR: ${e.message}, reviced text is ${res_text}")
  }
  client.close()
  return result_message
}


public func stream_chat(
  prompt: String,
  env_info: EnvInfo,
  history: ArrayList<(String, String)>
): Option<String> {
  let (request, client) = build_http_client(
    prompt,
    env_info,
    history,
    stream: true
  ) 
  var result_response: String = ""
  var temp_text2 = ""
  try {
    // call api
    let response = client.send(
      request
    )
    // read result
    let buffer = Array<Byte>(10240, item: 0)
    var finish_reason: Option<String> = None
    while(finish_reason.isNone() && temp_text2 != "[DONE]") {
      let length = response.body.read(buffer)
      let res_text = String.fromUtf8(buffer[..length])

      for (temp_text in res_text.split("\n")) {
        temp_text2 =  if (temp_text.startsWith("data: ")) {
          temp_text["data: ".size..]
        } else {
          temp_text
        }
        if (temp_text2.size == 0) {
          continue
        }
        if (temp_text2 == "[DONE]") {
          break
        }
        // println("========================")
        // println("temp_text: ${temp_text2}")
        // println("========================")
        var input_stream = ByteArrayStream()
        input_stream.write(temp_text2.toArray())
        // convert text to ChatResponse object
        let json_reader = JsonReader(input_stream)
        let res_object = ChatResponse.fromJson(json_reader)
        let choices: ArrayList<Choice> = res_object.choices
        if (choices.size > 0) {
          finish_reason = choices[0].finish_reason
          if (finish_reason.isNone()) {
            let delta = choices[0].delta.getOrThrow()
            print("${delta.content}", flush: true)
            result_response += delta.content 
          }
        } else {
          println("can't found any response")
        }
      }
    }
  } catch (e: Exception) {
    println("ERROR: ${e.message}, reviced text is ${temp_text2}")
  }
  client.close()
  if (result_response.size > 0) {
    return Some(result_response)
  } else {
    return None
  }
}