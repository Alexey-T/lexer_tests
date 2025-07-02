/**
 * 输入一个字符串数组 words 和一个字符 separator 
 * 使用 separator 拆分 words 中的每个字符串
 * 返回一个由拆分后的新字符串组成的字符串数组，不包括空字符串
 * LJJ 2024.12.10
 */
import std.collection.*

// 普通实现
func split_normal(words: Array<String>, separator: String): ArrayList<String> {
    let result = ArrayList<String>()
    for (word in words) {
        let array = word.split(separator, removeEmpty: true)
        result.appendAll(array)
    }
    return result
}

// 函数式编程实现
func split(words: Array<String>, separator: String): ArrayList<String> {
    words |> map { text =>
        text.split(separator, removeEmpty: true)
    } |> flatten |> collectArrayList
}

main() {
    let words = ["one.two.three", ".four.five", "six.", "seven"]
    split_normal(words, '.') |> println
    split(words, '.') |> println
}