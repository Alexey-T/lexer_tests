package simple_math_interpreter

import simple_math_interpreter.ext.*

public class Combinator<I, O> {
    public Combinator(public let parseFunc: (List<I>) -> Option<(O, List<I>)>) {}
}

extend<I, O> Combinator<I, O> {
    public static func make(predicate: (I) -> Bool): Combinator<I, I> {
        Combinator {
            input => match (input) {
                case Cons(hd, tl) => if (predicate(hd)) {
                    Some((hd, tl))
                } else {
                    None
                }
                case Nil => None
            }
        }
    }

    public func map<O2>(f: (O) -> O2): Combinator<I, O2> {
        Combinator {
            input => match (this.parseFunc(input)) {
                case Some((value, rest)) => Some((f(value), rest))
                case None => None
            }
        }
    }

    public func and<O2>(c2: Combinator<I, O2>): Combinator<I, (O, O2)> {
        Combinator {
            input => match (this.parseFunc(input)) {
                case Some((value1, rest1)) => match (c2.parseFunc(rest1)) {
                    case Some((value2, rest2)) => Some(((value1, value2), rest2))
                    case None => None
                }
                case None => None
            }
        }
    }

    public func or(c2: Combinator<I, O>): Combinator<I, O> {
        Combinator {
            input => match (this.parseFunc(input)) {
                case Some(a) => Some(a)
                case None => c2.parseFunc(input)
            }
        }
    }

    public func many(min: Int64): Combinator<I, List<O>> {
        Combinator {
            input =>
            var vlist = List<O>.empty()
            var rest = input
            while (true) {
                match (this.parseFunc(rest)) {
                    case None => break
                    case Some((value, _rest)) =>
                        rest = _rest
                        vlist = vlist.add(value)
                }
            }
            if (vlist.lenth() < min) {
                None
            } else {
                Some((vlist.reverse(), rest))
            }
        }
    }
}