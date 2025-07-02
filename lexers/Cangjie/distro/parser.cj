package simple_math_interpreter

import std.convert.*
import simple_math_interpreter.ext.*

enum Token {
    | Value(Int)
    | LParen
    | RParen
    | Plus
    | Minus
    | Multiply
    | Divide
}

extend Token {
    static func fromRunes(runes: List<Rune>): Result<List<Token>, String> {
        match (tokensFunc(runes).map {t => t[0]}) {
            case Some(l) => Ok(l)
            case None => Err("Invalid Input!")
        }
    }
}

extend Token <: ToString {
    public func toString() {
        match (this) {
            case Value(n) => n.toString()
            case LParen => "("
            case RParen => ")"
            case Plus => "+"
            case Minus => "-"
            case Multiply => "*"
            case Divide => "/"
        }
    }
}

let symbol = Combinator<Rune, Rune>.make {
    ch => match (ch) {
        case r'+' | r'-' | r'*' | r'/' | r'(' | r')' => true
        case _ => false
    }
}.map {
    ch: Rune => match (ch) {
        case '+' => Plus
        case '-' => Minus
        case '*' => Multiply
        case '/' => Divide
        case '(' => LParen
        case ')' => RParen
        case _ => throw Exception()
    }
}
let whiteSpace = Combinator<Rune, Rune>.make {ch => ch == r' '}
let number = Combinator<Rune, Rune>.make {
    ch => match (Int64.tryParse(ch.toString())) {
        case Some(n) where n >= 0 && n <= 9 => true
        case _ => false
    }
}.map {ch => Int64.parse(ch.toString())}
let value = number.many(1).map {
    l => l.reduce<Int64>({acc, x => acc * 10 + x}, 0)
}.map {v => Value(v)}
let tokenAndSpace = value.or(symbol).and(whiteSpace.many(1)).map {s: (Token, List<Rune>) => s[0]}
let token = value.or(symbol)
let tokens = tokenAndSpace.or(token).many(1)

func tokensFunc(input: List<Rune>): Option<(List<Token>, List<Rune>)> {
    tokens.parseFunc(input)
}

enum Expression {
    | Number(Int)
    | Plus(Expression, Expression)
    | Minus(Expression, Expression)
    | Multiply(Expression, Expression)
    | Divide(Expression, Expression)
}

extend Expression {
    static func fromTokens(tokens: List<Token>): Result<Expression, String> {
        match (expressionFunc(tokens).map {t => t[0]}) {
            case Some(e) => Ok(e)
            case None => Err("Invalid Expression!")
        }
    }
}

extend Expression {
    func eval(): Int64 {
        match (this) {
            case Number(n) => n
            case Plus(e1, e2) => e1.eval() + e2.eval()
            case Minus(e1, e2) => e1.eval() - e2.eval()
            case Multiply(e1, e2) => e1.eval() * e2.eval()
            case Divide(e1, e2) => e1.eval() / e2.eval()
        }
    }
}

extend Expression <: ToString {
    public func toString(): String {
        match (this) {
            case Number(n) => "${n}"
            case Plus(e1, e2) => "(+ ${e1} ${e2})"
            case Minus(e1, e2) => "(- ${e1} ${e2})"
            case Multiply(e1, e2) => "(* ${e1} ${e2})"
            case Divide(e1, e2) => "(/ ${e1} ${e2})"
        }
    }
}

let eNumber = Combinator<Token, Token>.make {
    token => match (token) {
        case Value(_) => true
        case _ => false
    }
}.map {
    token => match (token) {
        case Value(i) => Number(i)
        case _ => throw Exception()
    }
}
let lparen = Combinator<Token, Token>.make {
    token => match (token) {
        case LParen => true
        case _ => false
    }
}
let rparen = Combinator<Token, Token>.make {
    token => match (token) {
        case RParen => true
        case _ => false
    }
}
let plus = Combinator<Token, Token>.make {
    token => match (token) {
        case Plus => true
        case _ => false
    }
}
let minus = Combinator<Token, Token>.make {
    token => match (token) {
        case Minus => true
        case _ => false
    }
}
let multiply = Combinator<Token, Token>.make {
    token => match (token) {
        case Multiply => true
        case _ => false
    }
}
let divide = Combinator<Token, Token>.make {
    token => match (token) {
        case Divide => true
        case _ => false
    }
}

func atomicFunc(input: List<Token>): Option<(Expression, List<Token>)> {
    let expression = Combinator(expressionFunc)
    lparen.and(expression).and(rparen).map {t => t[0][1]}.or(eNumber).parseFunc(input)
}

func combineFunc(input: List<Token>): Option<(Expression, List<Token>)> {
    let atomic = Combinator(atomicFunc)
    atomic.and(multiply.or(divide).and(atomic).many(0)).map {
        t => t[1].reduce(
            {
                acc, x => match ((acc, x)) {
                    case (e1, (Multiply, e2)) => Multiply(e1, e2)
                    case (e1, (_, e2)) => Divide(e1, e2)
                }
            },
            t[0]
        )
    }.parseFunc(input)
}

func expressionFunc(input: List<Token>): Option<(Expression, List<Token>)> {
    let combine = Combinator(combineFunc)
    combine.and(plus.or(minus).and(combine).many(0)).map {
        t => t[1].reduce(
            {
                acc, x => match ((acc, x)) {
                    case (e1, (Plus, e2)) => Plus(e1, e2)
                    case (e1, (_, e2)) => Minus(e1, e2)
                }
            },
            t[0]
        )
    }.parseFunc(input)
}

public func eval(str: String): Result<Int64, String> {
    let runes = List<Rune>.fromArray(str.trimAscii().toRuneArray())
    Token.fromRunes(runes).andThen(Expression.fromTokens).map {e => e.eval()}
}
