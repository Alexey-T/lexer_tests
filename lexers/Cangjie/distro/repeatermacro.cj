macro package cj_debounce_throttle.macros
/**
 * Debounce实现
 * @author unravel
 * @description

    debounce简单实现，高频动作连续触发时，实际动作往后顺移

    使用方式如下
    300毫秒,开始前和结束后都回调
    300这个数字是必须的，leading和trailing是可选的。他们三个使用逗号分割
    leading和trailing都没有时，默认为trailing
    @Debounce[300,leading, trailing]
 */
public macro Debounce(attr: Tokens, input: Tokens): Tokens {
    mustBeFunc(input) {
        funcDecl: FuncDecl => impDebounceOrThrottle(false, attr, funcDecl)
    }
}

/**
 * Throttle实现
 * @author unravel
 * @description

    throttle简单实现，高频动作连续触发时，每隔interval调用一次实际动作

    使用方式和 Debounce 一样
 */
public macro Throttle(attr: Tokens, input: Tokens): Tokens {
    mustBeFunc(input) {
        funcDecl: FuncDecl => impDebounceOrThrottle(true, attr, funcDecl)
    }
}
