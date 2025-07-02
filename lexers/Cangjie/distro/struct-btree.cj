// 基于 struct 实现二叉树
// LJJ 2024.12.10
struct Node {
    public Node(var value: String,
        let left!: ?Node = None,
        let right!: ?Node = None) {}

    public func traverse(): Unit {
        left?.traverse()
        print(value)
        right?.traverse()
    }
}

main() {
    var tree = Node('A',
        left: Node('B', left: Node('C', right: Node('D'))),
        right: Node('E', left: Node('F'), right: Node('G')))
    tree.traverse()
}
