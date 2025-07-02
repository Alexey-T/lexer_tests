macro package Macro_JsonSerializable.macros

import std.ast.*

// 工具方法，查找结构体是否需要实现某个接口
func findInterface(name: String, decl: StructDecl): Bool {
    for (sp in decl.superTypes) {
        match (sp) {
            case rt: RefType => if (rt.identifier.value == name) {
                return true
            } else {
                continue
            }
            case _ => continue
        }
    }
    return false
}