package tokenizer

internal import encoding.json.stream.*

public struct TokenJson <: JsonDeserializable<TokenJson> {
  let id: UInt32
  let content: String
  let single_word: Bool
  let lstrip: Bool
  let rstrip: Bool
  let normalized: Bool
  let special: Bool

  public init(
    id: UInt32,
    content: String,
    single_word: Bool,
    lstrip: Bool,
    rstrip: Bool,
    normalized: Bool,
    special: Bool
  ) {
    this.id = id
    this.content = content
    this.single_word = single_word
    this.lstrip = lstrip
    this.rstrip = rstrip
    this.normalized = normalized
    this.special = special
  }

  public static func fromJson(r: JsonReader): TokenJson {
    var temp_id: UInt32 = 0
    var temp_content: String = ""
    var temp_single_word: Bool = false
    var temp_lstrip: Bool = false
    var temp_rstrip: Bool = false
    var temp_normalized: Bool = false
    var temp_special: Bool = false
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "id" => temp_id = r.readValue<UInt32>()
              case "content" => temp_content = r.readValue<String>()
              case "single_word" => temp_single_word = r.readValue<Bool>()
              case "lstrip" => temp_lstrip = r.readValue<Bool>()
              case "rstrip" => temp_rstrip = r.readValue<Bool>()
              case "normalized" => temp_normalized = r.readValue<Bool>()
              case "special" => temp_special = r.readValue<Bool>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for TokenJson")
      }
    }
    return TokenJson(
      temp_id,
      temp_content,
      temp_single_word,
      temp_lstrip,
      temp_rstrip,
      temp_normalized,
      temp_special
    )
  }
}

public struct NormalizerJson <: JsonDeserializable<NormalizerJson> {
  let p_type: String

  public init(p_type: String) {
    this.p_type = p_type
  }
  
  public static func fromJson(r: JsonReader): NormalizerJson {
    var temp_p_type: String = ""
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "type" => temp_p_type = r.readValue<String>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for NormalizerJson")
      }
    }
    return NormalizerJson(temp_p_type)
  }
}

public struct PatternJson <: JsonDeserializable<PatternJson> {
  let p_regex: String

  public init(p_regex: String) {
    this.p_regex = p_regex
  }
  
  public static func fromJson(r: JsonReader): PatternJson {
    var temp_p_regex: String = ""
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "Regex" => temp_p_regex = r.readValue<String>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ProcessJson")
      }
    }
    return PatternJson(temp_p_regex)
  }

}


public struct ProcessJson <: JsonDeserializable<ProcessJson> {
  // Union ByteLevel and SplitTokenizer
  let p_type: String
  // use for ByteLevel: decoder/post_processor
  let add_prefix_space: Option<Bool>
  let trim_offsets: Option<Bool>
  let use_regex: Option<Bool>
  // actived when split tokenizer
  let pattern: Option<PatternJson>
  let behavior: Option<String>
  let invert: Option<Bool>

  // construct for decode/post_preprossor
  public init(
    p_type: String,
    add_prefix_space: Bool,
    trim_offsets: Bool,
    use_regex: Bool
  ) {
    this.p_type = p_type
    if (this.p_type != "ByteLevel") {
      throw Exception("this construction function only suppport ByteLevel type")
    }
    this.add_prefix_space = Some(add_prefix_space)
    this.trim_offsets = Some(trim_offsets)
    this.use_regex = Some(use_regex)
    this.pattern = None
    this.behavior = None
    this.invert = None
  }

  public init(
    p_type: String,
    pattern: PatternJson,
    behavior: String,
    invert: Bool
  ) {
    this.p_type = p_type
    if (p_type != "Split") {
      throw Exception("this construction function only support Split type")
    }
    this.add_prefix_space = None
    this.trim_offsets = None
    this.use_regex = None
    this.pattern = Some(pattern)
    this.behavior = Some(behavior)
    this.invert = Some(invert)
  }

  public static func fromJson(r: JsonReader): ProcessJson {
    var temp_p_type: String = ""
    var temp_add_prefix_space: Bool = false
    var temp_trim_offsets: Bool = false
    var temp_use_regex: Bool = false
    var temp_pattern: PatternJson = PatternJson("")
    var temp_behavior: String = ""
    var temp_invert: Bool = false
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "type" => temp_p_type = r.readValue<String>()
              // ByteLevel
              case "add_prefix_space" => temp_add_prefix_space = r.readValue<Bool>()
              case "trim_offsets" => temp_trim_offsets = r.readValue<Bool>()
              case "use_regex" => temp_use_regex = r.readValue<Bool>()
              // Split
              case "pattern" => temp_pattern = r.readValue<PatternJson>()
              case "behavior" => temp_behavior = r.readValue<String>()
              case "invert" => temp_invert = r.readValue<Bool>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ProcessJson")
      }
    }
    match (temp_p_type) {
      case "ByteLevel" =>
        ProcessJson(
          temp_p_type,
          temp_add_prefix_space,
          temp_trim_offsets,
          temp_use_regex,
        )
      case "Split" => 
        ProcessJson(
          temp_p_type,
          temp_pattern,
          temp_behavior,
          temp_invert
        )
      case other => throw Exception("unkonw process type ${other}")
    }
  }
}

public struct PreTokenizerJson <: JsonDeserializable<PreTokenizerJson> {
  let p_type: String
  let pretokenizers: ArrayList<ProcessJson>
  public init(p_type: String, pretokenizers: ArrayList<ProcessJson>) {
    this.p_type = p_type
    this.pretokenizers = pretokenizers
  }

  public init() {
    // default
    this.p_type = ""
    this.pretokenizers = ArrayList<ProcessJson>()
  }

  public static func fromJson(r: JsonReader): PreTokenizerJson {
    var temp_p_type: String = ""
    var temp_pre_tokenizers: ArrayList<ProcessJson> = ArrayList<ProcessJson>()
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "type" => temp_p_type = r.readValue<String>()
              case "pretokenizers" => temp_pre_tokenizers = r.readValue<ArrayList<ProcessJson>>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ProcessJson")
      }
    }
    return PreTokenizerJson(temp_p_type, temp_pre_tokenizers)
  }

}

public struct ModelJson <: JsonDeserializable<ModelJson> {
  let p_type: String
  let dropout: Option<Float32>
  var unk_token: Option<String> = None
  var continuing_subword_prefix: String = ""
  var end_of_word_suffix: String = ""
  var fuse_unk: Bool = false
  var byte_fallback: Bool = false
  let vocab: HashMap<String, UInt32>
  let merges: ArrayList<String>
  public init(
    p_type: String,
    dropout: Option<Float32>,
    vocab: HashMap<String, UInt32>,
    merges: ArrayList<String>,
    unk_token!: Option<String> = None,
    continuing_subword_prefix!: String = "",
    end_of_word_suffix!: String = "",
    fuse_unk!: Bool = false,
    byte_fallback!: Bool = false
  ) {
    this.p_type = p_type
    if (dropout.isSome()) {
      let dropout_value = dropout.getOrThrow()
      if (dropout_value < 0.0 || dropout_value > 1.0) {
        throw Exception("dropout can only between 0~1")
      }
    }
    this.dropout = dropout
    this.unk_token = unk_token
    this.continuing_subword_prefix = continuing_subword_prefix
    this.end_of_word_suffix = end_of_word_suffix
    this.fuse_unk = fuse_unk
    this.byte_fallback = byte_fallback
    this.vocab = vocab
    this.merges = merges
  }

  public static func fromJson(r: JsonReader): ModelJson {
    var temp_p_type: String = ""
    var temp_dropout: Option<Float32> = None
    var temp_unk_token: Option<String> = None
    var temp_continuing_subword_prefix: String = ""
    var temp_end_of_word_suffix: String = ""
    var temp_fuse_unk: Bool = false
    var temp_byte_fallback: Bool = false
    var temp_vocab: HashMap<String, UInt32> = HashMap<String, UInt32>()
    var temp_merges: ArrayList<String> = ArrayList<String>()
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "type" => temp_p_type = r.readValue<String>()
              case "dropout" => temp_dropout = r.readValue<Option<Float32>>()
              case "unk_token" => temp_unk_token = r.readValue<Option<String>>()
              case "continuing_subword_prefix" => temp_continuing_subword_prefix = r.readValue<String>()
              case "end_of_word_suffix" => temp_end_of_word_suffix = r.readValue<String>()
              case "fuse_unk" => temp_fuse_unk = r.readValue<Bool>()
              case "byte_fallback" => temp_byte_fallback = r.readValue<Bool>()
              case "vocab" => temp_vocab = r.readValue<HashMap<String, UInt32>>()
              case "merges" => temp_merges = r.readValue<ArrayList<String>>()
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ModelJson")
      }
    }
    return ModelJson(
      temp_p_type,
      temp_dropout,
      temp_vocab,
      temp_merges,
      unk_token: temp_unk_token,
      continuing_subword_prefix: temp_continuing_subword_prefix,
      end_of_word_suffix: temp_end_of_word_suffix,
      fuse_unk: temp_fuse_unk,
      byte_fallback: temp_byte_fallback
    )
  }
}

public struct TokenizerJson <: JsonDeserializable<TokenizerJson> {
  let version: String
  let truncation: Option<String>
  let padding: Option<String>
  let added_tokens: ArrayList<TokenJson>
  let normalizer: NormalizerJson
  let pre_tokenizer: PreTokenizerJson
  let post_processor: ProcessJson
  let decoder: ProcessJson
  let model: ModelJson

  public init(
    version: String,
    truncation: Option<String>,
    padding: Option<String>,
    added_tokens: ArrayList<TokenJson>,
    normalizer: NormalizerJson,
    pre_tokenizer: PreTokenizerJson,
    post_processor: ProcessJson,
    decoder: ProcessJson,
    model: ModelJson
  ) {
    this.version = version
    this.truncation = truncation
    this.padding = padding
    this.added_tokens = added_tokens
    this.normalizer = normalizer
    this.pre_tokenizer = pre_tokenizer
    this.post_processor = post_processor
    this.decoder = decoder
    this.model = model
  }

  public static func fromJson(r: JsonReader): TokenizerJson {
    var temp_version: String = ""
    var temp_truncation: Option<String> = None
    var temp_padding: Option<String> = None
    var temp_added_tokens: ArrayList<TokenJson> = ArrayList<TokenJson>()
    var temp_normalizer: NormalizerJson = NormalizerJson("")
    var temp_pre_tokenizer: PreTokenizerJson = PreTokenizerJson()
    var temp_post_processor: Option<ProcessJson> = None
    var temp_decoder: Option<ProcessJson> = None
    var temp_model: Option<ModelJson> = None
    while (let Some(v) <- r.peek()) {
      match(v) {
        case BeginObject =>
          r.startObject()
          while(r.peek() != EndObject) {
            let n = r.readName()
            match (n) {
              case "version" => temp_version = r.readValue<String>()
              case "truncation" => temp_truncation = r.readValue<Option<String>>()
              case "padding" => temp_padding = r.readValue<Option<String>>()
              case "added_tokens" => temp_added_tokens = r.readValue<ArrayList<TokenJson>>()
              case "normalizer" => temp_normalizer = r.readValue<NormalizerJson>()
              case "pre_tokenizer" => temp_pre_tokenizer = r.readValue<PreTokenizerJson>()
              case "post_processor" => temp_post_processor = Some(r.readValue<ProcessJson>())
              case "decoder" => temp_decoder = Some(r.readValue<ProcessJson>())
              case "model" => temp_model = Some(r.readValue<ModelJson>())
              case unkow => println("unkow key ${unkow}")
            }
          }
          r.endObject()
          break
        case _ => throw Exception("can't deserialize for ProcessJson")
      }
    }
    return TokenizerJson(
      temp_version,
      temp_truncation,
      temp_padding,
      temp_added_tokens,
      temp_normalizer,
      temp_pre_tokenizer,
      temp_post_processor.getOrThrow(),
      temp_decoder.getOrThrow(),
      temp_model.getOrThrow(),
    )
  }
}