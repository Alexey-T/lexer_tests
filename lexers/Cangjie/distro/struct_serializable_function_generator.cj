macro package Macro_JsonSerializable.macros

import std.ast.*
import std.collection.*

// 结构体 serialize、deserialize 方法生成器
class StructSerializableFunctionGenerator <: Generator {

    public func generate(context: Context): Unit {
        this.generateDeserialize(context)
        this.generateSerialize(context)
    }

    private func generateStructNameTokens(decl: StructDecl): Tokens {
        try {
            return quote($(decl.identifier)$(decl.genericParam))
        } catch (_: Exception) {
            return quote($(decl.identifier))
        }
    }

    private func generateDeserialize(context: Context): Unit {
        let arguments = ArrayList<Argument>()
        let assign = quote()
        for (reval in context.valDecls) {
            if (reval.assign.kind != TokenKind.ASSIGN) {
                let arg = Argument()
                arg.identifier = Token(TokenKind.IDENTIFIER, reval.identifier.value)
                arg.expr = CallExpr(quote(
                    $(reval.declType).deserialize(dms.get($(reval.identifier.value)))
                ))
                arg.colon = Token(TokenKind.COLON, ":")
                arguments.append(arg)
            } else if (reval.keyword.kind == TokenKind.VAR) {
                assign.append(quote(
                    reval.$(reval.identifier) = $(reval.declType).deserialize(dms.get($(reval.identifier.value)))
                ))
            }
        }
        let ce = CallExpr(quote($(generateStructNameTokens(context.structDecl))()))
        ce.arguments = arguments
        let body: Tokens
        if (assign.size == 0) {
            body = quote(
                return $(ce)
            )
        } else {
            body = quote(
                var reval = $(ce)
                $(assign)
                return reval
            )
        }
        let deserializeFunc = FuncDecl(quote(
            public static func deserialize(dm: DataModel): $(generateStructNameTokens(context.structDecl)) {
                let dms = match (dm) {
                    case d: DataModelStruct => d
                    case _ => throw Exception("This data is not DataModelStruct")
                }
                $(body)
            }
        ))
        context.structDecl.body.decls.append(deserializeFunc)
    }

    private func generateSerialize(context: Context): Unit {
        let body = quote()
        for (reval in context.valDecls) {
            body.append(quote(
                dms.add(field($(reval.identifier.value), this.$(reval.identifier)))
            ))
        }
        let serializeFunc = FuncDecl(quote(
            public func serialize(): DataModel {
                let dms = DataModelStruct()
                $(body)
                return dms
            }
        ))
        context.structDecl.body.decls.append(serializeFunc)
    }
}