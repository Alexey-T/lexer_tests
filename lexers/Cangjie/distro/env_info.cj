package openai_chat
import encoding.json.stream.*
import std.fs.File
import std.fs.Path
import std.io.ByteArrayStream

public class EnvInfo <: JsonDeserializable<EnvInfo> & JsonSerializable {
  public let model: String          // 模型名称
  public let api_key: String        // api密钥
  public let base_url: String       // 调用接口路径
  public let system_prompt: String  // 预置系统提示词

  public init(model: String, api_key: String, base_url: String, system_prompt: String) {
    this.model = model
    this.api_key = api_key
    this.base_url = base_url
    this.system_prompt = system_prompt
  }
  
  public static func fromJson(r: JsonReader): EnvInfo {
    var temp_model: String = ""
    var temp_api_key: String = "sk-xxx"
    var temp_base_url: String = "http://xxx.xxx.xxx/v1"
    var temp_system_prompt: String = "You are a helpful assistant."
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
                case "model" => temp_model = r.readValue<String>()
                case "api_key" => temp_api_key = r.readValue<String>()
                case "base_url" => temp_base_url = r.readValue<String>()
                case "system_prompt" => temp_system_prompt = r.readValue<String>()
                case _ => ()
            }
          }
          r.endObject()
          break
        case _ => throw Exception()
      }
    }
    return EnvInfo(temp_model, temp_api_key, temp_base_url, temp_system_prompt)
  }
  
  public func toJson(w: JsonWriter): Unit {
      w.startObject()
      w.writeName("model").writeValue(this.model)
      w.writeName("api_key").writeValue(this.api_key)
      w.writeName("base_url").writeValue(this.base_url)
      w.writeName("system_prompt").writeValue(this.system_prompt)
      w.endObject()
      w.flush()
  }
}


public func save_env_info(): Unit {
  // 该函数用于测试EnvInfo类的序列化为json的能力，顺便生成一个env_sample.json样本做为参考
  let env_path = Path("env_sample.json")
  if (File.exists(env_path)) {
    File.delete(env_path)
  }
  let file = File.create(env_path)
  let env_info = EnvInfo(
    "xxxx",
    "sk-xxxxxx",
    "http://xxx.xxx.xxx/v1/chat/completions",
    "You are a helpful assistant."
  )
  var byte_stream = ByteArrayStream()
  var json_writer = JsonWriter(byte_stream)
  let write_config = WriteConfig.pretty
  json_writer.writeConfig = write_config
  env_info.toJson(json_writer)
  file.write(byte_stream.readToEnd())
  println("`env_sample.json` save ok")
  file.close()
}

public func load_env_info(): EnvInfo {
  // 用于加载配置文件
  let env_path = Path("config.json")
  if (!File.exists(env_path)) {
    throw Exception("The config file not exists, please check again")
  }
  let file = File.openRead(env_path)
  let file_str: Array<UInt8> = file.readToEnd()
  var byte_stream = ByteArrayStream()
  byte_stream.write(file_str)
  let json_reader = JsonReader(byte_stream)
  let env_info: EnvInfo = EnvInfo.fromJson(json_reader)
  file.close()
  // println("model: ${env_info.model}")
  // println("api_key: ${env_info.api_key}")
  // println("base_url: ${env_info.base_url}")
  // println("system_prompt: ${env_info.system_prompt}")
  return env_info
}