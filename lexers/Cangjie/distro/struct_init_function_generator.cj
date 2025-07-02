macro package Macro_JsonSerializable.macros

import std.ast.*
import std.collection.*

// 结构体命名构造函数生成器
class StructInitFunctionGenerator <: Generator {

    public func generate(context: Context): Unit {
        var tokens = context.attrs
        if (tokens.size == 0) {
            tokens = Token(TokenKind.PRIVATE, "private").toTokens()
        } else if (tokens.size > 1) {
            diagReport(DiagReportLevel.ERROR, context.attrs, "构造函数可见性设置错误", "不支持多个关键字")
            throw Exception("构造函数可见性设置错误")
        }
        let illegal = match (tokens[0].kind) {
            case TokenKind.PUBLIC => false
            case TokenKind.PRIVATE => false
            case TokenKind.PROTECTED => false
            case TokenKind.INTERNAL => false
            case _ => true
        }
        if (illegal) {
            diagReport(DiagReportLevel.ERROR, context.attrs, "构造函数可见性设置错误", "不支持的关键字: ${tokens[0].value}")
            return
        }
        let params = ArrayList<FuncParam>()
        let body = quote()
        for (reval in context.valDecls) {
            if (reval.assign.kind == TokenKind.ASSIGN) {
                continue
            }
            params.append(FuncParam(quote(
                $(reval.identifier)!: $(reval.declType)
            )))
            body.append(quote(
                this.$(reval.identifier) = $(reval.identifier)
            ))
        }
        let funcDecl = FuncDecl(quote(
            $(tokens) init() {
                $(body)
            }
        ))
        funcDecl.funcParams = params
        context.structDecl.body.decls.append(funcDecl)
    }
}