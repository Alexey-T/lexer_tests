open class NodeA {
    public NodeA(protected var value: String,
        protected let left!: ?NodeA = None,
        protected let right!: ?NodeA = None) {}

    public open func traverse(): Unit {
        left?.traverse()
        print(value)
        right?.traverse()
    }
}

class NodeB <: NodeA {
    public init(value: String,
        left!: ?NodeA = None, right!: ?NodeA = None) {
        super(value, left: left, right: right)
    }

    public func traverse(): Unit {
        print(value)
        left?.traverse()
        right?.traverse()
    }
}

main() {
    var tree = NodeA('A',
        left: NodeA('B', left: NodeA('C', right: NodeA('D'))),
        right: NodeB('E', left: NodeB('F'), right: NodeB('G')))
    tree.traverse()
}
