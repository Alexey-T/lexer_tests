macro package Macro_JsonSerializable.macros

import std.ast.*

// 生成上下文
class Context {

    let attrs: Tokens
    let input: Tokens
    let structDecl: StructDecl
    let valDecls: Array<VarDecl>
    
    init(attrs: Tokens, input: Tokens) {
        this.attrs = attrs
        this.input = input
        this.structDecl = match (parseDecl(input)) {
            case d: StructDecl => d
            case _ =>
                diagReport(DiagReportLevel.ERROR, input, "宏仅适用于结构体", "请检查宏是否作用在结构体上")
                throw Exception("不支持的类型")
        }
        if (!findInterface("Serializable", this.structDecl)) {
            diagReport(DiagReportLevel.ERROR, input, "结构体需要声明 Serializable 接口", "请检查结构体是否声明要实现 Serializable 接口")
            throw Exception("未声明 Serializable 接口")
        }
        let visitor = StructDeclVisitor()
        this.structDecl.traverse(visitor)
        this.valDecls = visitor.varDecls.toArray()
    }
}