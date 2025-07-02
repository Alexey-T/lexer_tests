macro package Macro_JsonSerializable.macros

import std.ast.*

// 结构体 fromJson、toJson 方法生成器
class StructToJsonFunctionGenerator <: Generator {

    public func generate(context: Context): Unit {
        if (!findInterface("ToJson", context.structDecl)) {
            return
        }
        let fromJsonFunc = FuncDecl(quote(
            public static func fromJson(jv: JsonValue): DataModel {
                return DataModel.fromJson(jv)
            }
        ))
        context.structDecl.body.decls.append(fromJsonFunc)
        let toJsonFunc = FuncDecl(quote(
            public func toJson(): JsonValue {
                return this.serialize().toJson()
            }
        ))
        context.structDecl.body.decls.append(toJsonFunc)
    }
}