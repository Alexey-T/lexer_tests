// 基于函数实现二叉树，演示仓颉函数的 first class 及闭包特性
// LJJ 2024.12.10
func node(value: String,
    left!: () -> Unit = {=>}, right!: () -> Unit = {=>}) {
    return { =>
        left()
        print(value)
        right()
    }
}

main() {
    let tree = node('A',
        left: node('B', left: node('C', right: node('D'))),
        right: node('E', left: node('F'), right: node('G')))
    tree()
}
