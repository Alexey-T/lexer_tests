macro package cj_curry.macros

import std.collection.ArrayList

func makeCurries(funcDecl: FuncDecl): Tokens { // cjlint-ignore !G.FUN.01
    let ret = Tokens()
    // 添加原始实现
    ret.append(funcDecl.toTokens())
    // 开始构造多个柯里化的函数
    // 需要修改参数列表、返回值类型、函数体
    // 保留原函数参数
    let orgFuncParams = funcDecl.funcParams.clone()
    var orgFuncRetTokens: ?Tokens = None
    // 保留原始返回值类型
    try {
        let retType = funcDecl.declType
        orgFuncRetTokens = retType.toTokens()
    } catch (e: Exception) {
        println('函数没有明确标注返回值类型')
    }
    // 保留原始函数体
    let funcBlockNodes = funcDecl.block.nodes
    let oldBlockNodes = funcBlockNodes.clone()
    // 参数个数
    let paramSize = orgFuncParams.size

    for (i in 0..(paramSize - 1)) {
        let funcParams = funcDecl.funcParams
        funcParams.clear()
        // 重新赋值 参数类型
        funcParams.appendAll(orgFuncParams[0..=i])
        // 重新赋值返回类型
        if (let Some(retTypeTokens) <- orgFuncRetTokens) {
            var newRetType = retTypeTokens
            for (r in (paramSize - 1)..i : -1) {
                let pr = orgFuncParams[r]
                newRetType = quote(($(pr.paramType)) -> $(newRetType))
            }

            // println('参数类型: ${funcParams.toTokens()} 返回类型: ${newRetType.toString()}')
            funcDecl.declType = parseType(newRetType)
        }

        // 重新赋值函数体
        funcBlockNodes.clear()
        var funcBodyTokens = oldBlockNodes.toTokens()
        for (r in (paramSize - 1)..i : -1) {
            let pr = orgFuncParams[r]
            funcBodyTokens = quote({$(pr.identifier):$(pr.paramType) => $(funcBodyTokens)})
        }
        // println('函数体: ${funcBodyTokens}')
        funcBlockNodes.append(parseExpr(funcBodyTokens))
        ret.append(funcDecl.toTokens())
    }
    // println(ret.toString())
    ret
}
