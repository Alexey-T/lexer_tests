// 用分隔符拆分字符串，支持多分隔符
// 仓颉语言团队 刘俊杰 2024.10.24
import std.collection.*

// 处理 ASCII 字符串
// 常规实现
func split_ascii_normal(text: String, sep: String): ArrayList<String> {
    let indices = ArrayList<Int64>()
    var last = true
    for (i in 0..text.size) {
        let current = sep.indexOf(text[i]).isSome()
        if (last != current) {
            indices.append(i)
        }
        last = current
    }
    if (!last) { indices.append(text.size) }

    let result = ArrayList<String>()
    for (i in 0..indices.size:2) {
        result.append(text[indices[i]..indices[i + 1]])
    }
    return result
}

// 函数式编程实现
func split_ascii(text: String, sep: String): ArrayList<String> {
    let indices = ArrayList<Int64>()
    text |> enumerate |> fold(false) { state, e =>
        let current = sep.indexOf(e[1]).isNone()
        if (state != current) { indices.append(e[0]) }
        current
    } |> { valid: Bool => if (valid) {
        indices.append(text.size)
    }}

    let result = ArrayList<String>()
    for (i in 0..indices.size:2) {
        result.append(text[indices[i]..indices[i + 1]])
    }
    return result
}

// 处理 Unicode 字符串
func split_unicode(text: String, sep: String) {
    let indices = ArrayList<Int64>()
    text.runes() |> enumerate |> fold(false) { state, e =>
        let current = !sep.contains(e[1].toString())
        if (state != current) { indices.append(e[0]) }
        current
    } |> { valid: Bool => if (valid) { indices.append(text.size) } }

    let runes = text.toRuneArray()
    let result = ArrayList<String>()
    for (i in 0..indices.size:2) {
        result.append(String(runes[indices[i]..indices[i + 1]]))
    }
    return result
}

main() {
    let text = "123, 456 7&89, , 96^3, 567"
    println(split_ascii(text, "&^, "))
}
