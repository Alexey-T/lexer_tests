macro package Macro_JsonSerializable.macros

import std.ast.*
import std.collection.*

// 结构体属性访问器
class StructDeclVisitor <: Visitor {

    let varDecls = ArrayList<VarDecl>()

    public override func visit(varDecl: VarDecl) {
        this.varDecls.append(varDecl)
        breakTraverse()
    }
}