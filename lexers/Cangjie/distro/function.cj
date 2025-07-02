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
