macro package Macro_JsonSerializable.macros

import std.ast.*
import std.collection.*

// 宏入口
public macro JsonSerializable(attrs: Tokens, input: Tokens): Tokens {
    let context = Context(attrs, input)
    let generators: Array<Generator> = [
        StructValDeclTransfromGenerator(),
        StructInitFunctionGenerator(),
        StructSerializableFunctionGenerator(),
        StructToJsonFunctionGenerator(),
        StructToStringFunctionGenerator()
    ]
    generators |> forEach { g: Generator => g.generate(context) }
    return context.structDecl.toTokens()
}