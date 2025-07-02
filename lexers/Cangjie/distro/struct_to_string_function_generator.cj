macro package Macro_JsonSerializable.macros

import std.ast.*

// 结构体 toString、toJsonString 方法生成器
class StructToStringFunctionGenerator <: Generator {

    public func generate(context: Context): Unit {
        if (!findInterface("ToString", context.structDecl)) {
            return
        }
        let toStringFunc = FuncDecl(quote(
            public func toString(): String {
                return this.serialize().toJson().toString()
            }
        ))
        context.structDecl.body.decls.append(toStringFunc)
        let toJsonStringFunc = FuncDecl(quote(
            public func toJsonString(): String {
                return this.serialize().toJson().toJsonString()
            }
        ))
        context.structDecl.body.decls.append(toJsonStringFunc)
    }
}