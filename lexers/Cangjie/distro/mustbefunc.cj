macro package cj_curry.macros

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
