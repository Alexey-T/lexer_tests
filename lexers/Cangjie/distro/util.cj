// cjlint-ignore -start !G.NAM.02 !G.EXP.03 !G.FUN.01
macro package cj_debounce_throttle.macros

import std.time.MonoTime
import std.convert.Parsable
import std.collection.ArrayList
internal import std.ast.*
public import cj_debounce_throttle.repeater.* // cjlint-ignore !G.PKG.01

const LEADING_STRING: String = 'leading'
const TRAILING_STRING: String = 'trailing'

/**
 * 保证必须是一个函数声明，然后执行action动作
 */
func mustBeFunc(
    input: Tokens,
    action: (FuncDecl) -> Tokens
): Tokens {
    let decl = parseDecl(input)
    if (let Some(funcDecl) <- (decl as FuncDecl)) {
        action(funcDecl)
    } else {
        diagReport(
            DiagReportLevel.ERROR,
            input,
            '不是函数类型',
            '只支持函数类型'
        )
        input
    }
}

/**
 * 解析属性
 */
func parseAttrToConfig(attrs: Tokens): Tokens {
    var interval = 0
    var leading = false
    var trailing = false

    var index: Int64 = 0
    while (true) {
        let (litExpr, nextIndex) = parseExprFragment(attrs, startFrom: index)
        match (litExpr) {
            case litConstExpr: LitConstExpr =>
                let literal = litConstExpr.literal
                if (literal.kind == TokenKind.INTEGER_LITERAL) {
                    interval = Int64.parse(literal.value)
                } else if (literal.kind == TokenKind.STRING_LITERAL) {
                    let value = literal.value
                    if (value == LEADING_STRING) {
                        leading = true
                    } else if (value == TRAILING_STRING) {
                        trailing = true
                    }
                }
            case refExpr: RefExpr =>
                let value = refExpr.identifier.value
                if (value == LEADING_STRING) {
                    leading = true
                } else if (value == TRAILING_STRING) {
                    trailing = true
                }
            case _ => diagReport(
                DiagReportLevel.ERROR,
                attrs[nextIndex..nextIndex],
                '这里必须是字面量',
                '只支持字面量'
            )
        }

        if (nextIndex >= attrs.size) {
            break
        }

        if (attrs[nextIndex].kind != TokenKind.COMMA) {
            diagReport(
                DiagReportLevel.ERROR,
                attrs[nextIndex..nextIndex + 1],
                "必须是逗号分割的多个表达式",
                "改为逗号"
            )
        }

        index = nextIndex + 1
    }

    // 必须有interval
    if (interval == 0) {
        diagReport(
            DiagReportLevel.ERROR,
            attrs,
            "必须有时间(毫秒)",
            "添加时间属性"
        )
    }

    if (!leading && !trailing) {
        trailing = true
    }

    quote(
        RepeaterConfig(interval: Duration.millisecond * $(interval), leading: $(leading), trailing: $(trailing)))
}

/**
 * 实现Debounce或者Throttle的宏
 */
func impDebounceOrThrottle(throttle: Bool, attr: Tokens, funcDecl: FuncDecl): Tokens {
    // 解析参数
    let config = parseAttrToConfig(attr)
    // 外层创建一个Debouncer
    let debouncerName = Token(TokenKind.IDENTIFIER, "debouncerFor" + funcDecl.identifier.value.toAsciiTitle())
    // 将函数体作为debouncer的block
    let ret = quote(private var $(debouncerName): ?Repeater = None
        )

    let funcBlockNodes = funcDecl.block.nodes
    let orgNodes = funcBlockNodes.clone()
    funcBlockNodes.clear()
    funcBlockNodes.append(
        parseExpr(
        quote(
            $(debouncerName).getOrDefault {
            let config = $(config)
            Repeater(throttle: $(throttle.toTokens()), config: config) {
                $(orgNodes)
            }}.call())
    ))
    ret.append(funcDecl.toTokens())
    return ret
}
// cjlint-ignore -end
