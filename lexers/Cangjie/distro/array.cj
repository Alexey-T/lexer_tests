// 基于数组构造 DFA 状态表，解析出一个字符串中的所有整数和小数
// LJJ 2024.12.10
import std.collection.* 

class Parser {
    static let states = Array(5, {_ => Array(128, item: 0)})
    static init() {
        states[0][45..58] = [1, 2, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3] // INIT
        states[1][45..58] = [0, 2, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3] // -
        states[2][45..58] = [0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] // . | -. | N. | -N.
        states[3][45..58] = [0, 2, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3] // N or -N
        states[4][45..58] = [0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] // ACCEPT
    }

    static public func parse(text: String) {
        var i = 0
        while (i < text.size) {
            var s = 0
            while (i < text.size && states[s][Int64(text[i])] == 0) {
                i++
            }
            let j = i
            while (i < text.size && states[s][Int64(text[i])] != 0) {
                s = states[s][Int64(text[i])]
                i++
            }
            if (s == 3 || s == 4) {
                print(text[j..i] + ' ')
            }
        }
        println()
    } 
}

main() {
    Parser.parse("1 2 3 456 789")
    Parser.parse("1ab2c3")
    Parser.parse("apple 30 banana 45")
    Parser.parse("1.2 3.4")
    Parser.parse("9- -3 -2")
    Parser.parse("3-2")
    Parser.parse("1 2 3-2")
    Parser.parse("xxx-3.14. .56-238jk12")
}