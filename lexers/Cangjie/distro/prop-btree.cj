// 基于 class 实现二叉树，基于 prop 特性实现各节点参数同步更新
// LJJ 2024.12.10
class Node {
    private var value: Int64 = 0
    public Node(private var name: String,
        private let left!: ?Node = None,
        private let right!: ?Node = None) {}

    public mut prop param: Int64 {
        set(number) {
            value = number
            println("${name}: ${value}")
            left?.param = number / 2
            right?.param = number / 2
        }
        get() { value }
    }
}

main() {
    var tree = Node('A',
        left: Node('B', left: Node('C', right: Node('D'))),
        right: Node('E', left: Node('F'), right: Node('G')))
    println(tree.param)
    tree.param = 128
}