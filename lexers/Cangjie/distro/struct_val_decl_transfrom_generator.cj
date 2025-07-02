macro package Macro_JsonSerializable.macros

import std.ast.*

// 将属性中 ?T 转换为 Option<T>
class StructValDeclTransfromGenerator <: Generator {

    public func generate(context: Context): Unit {
        for (decl in context.valDecls) {
            match (decl.declType) {
                case t: PrefixType =>
                    var reval = t.baseType.toTokens()
                    for (op in t.prefixOps) {
                        if (op.kind == TokenKind.QUEST) {
                            reval = quote(Option<$(reval)>)    
                        } else {
                            diagReport(DiagReportLevel.ERROR, op.toTokens(), "不支持的标识", "仅支持问号")
                            throw Exception("不支持的标识")
                        }
                    }
                    decl.declType = RefType(reval.toTokens())
                case _ => continue
            }
        }
    }
}