package simple_math_interpreter

public enum List<A> {
    | Nil
    | Cons(A, List<A>)
}

extend<A> List<A> {
    public static func empty(): List<A> {
        Nil
    }

    public static func cons(hd: A, tl: List<A>): List<A> {
        Cons(hd, tl)
    }

    public static func makeBy(n: Int64, f: (Int64) -> A): List<A> {
        var acc = List<A>.empty()
        for (i in 0..n) {
            acc = acc.add(f(i))
        }
        acc.reverse()
    }

    public static func fromArray(arr: Array<A>): List<A> {
        List<A>.makeBy(arr.size, {i => arr[i]})
    }

    public func add(x: A): List<A> {
        cons(x, this)
    }

    public func reduce<B>(f: (B, A) -> B, acc: B): B {
        var accm = acc
        var x = this
        while (true) {
            match (x) {
                case Nil => break
                case Cons(hd, tl) =>
                    accm = f(accm, hd)
                    x = tl
            }
        }
        accm
    }

    public func reverse(): List<A> {
        this.reduce({acc, x => List<A>.cons(x, acc)}, Nil)
    }

    public func isEmpty(): Bool {
        match (this) {
            case Nil => true
            case _ => false
        }
    }

    public func lenth(): Int64 {
        this.reduce({acc, _ => acc + 1}, 0)
    }

}

extend<A> List<A> <: ToString where A <: ToString {
    public func join(sp: String): String {
        match(this) {
            case Nil => ""
            case Cons(hd, tl)=> tl.reduce<String>({ acc: String, x: A => "${acc}${sp}${x}" }, "${hd}")
        }
    }

    public func toString() {
        "List: [${this.join(",")}]"
    }
}
