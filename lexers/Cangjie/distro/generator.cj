macro package Macro_JsonSerializable.macros

import std.ast.*

// 生成器抽象类
abstract class Generator {
    public func generate(context: Context) : Unit
}