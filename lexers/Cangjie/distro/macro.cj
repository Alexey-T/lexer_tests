macro package cj_curry.macros

// parseType: 0.55.3 及以上版本支持
internal import std.ast.{FuncDecl, parseDecl, parseExpr, Tokens, ToTokens,
    parseType, TokenKind, diagReport, DiagReportLevel, Token}

public macro Curry(input: Tokens): Tokens {
    mustBeFunc(input) {
        funcDecl =>
        let orgFuncParams = funcDecl.funcParams.clone()
        if (orgFuncParams.isEmpty()) {
            funcDecl.toTokens()
        } else if (orgFuncParams.iterator().any {
            p => p.not.kind == TokenKind.NOT
        }) {
            diagReport(
                DiagReportLevel.ERROR,
                funcDecl.toTokens(),
                '不支持带命名参数的函数',
                '不支持带命名参数的函数'
            )
            funcDecl.toTokens()
        } else {
            makeCurries(funcDecl)
        }
    }
}
