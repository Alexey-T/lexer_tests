// CFFI：调用 C 标准库 signal 函数处理系统信号
// LJJ 2024.12.10
import std.sync.sleep
import std.time.Duration

foreign func signal(value: Int32, handler: CFunc<(Int32) -> Unit>): Unit
const SIGINT: Int32 = 2

@C
func handler(value: Int32) {
    println('caught signal: ${value}')
}

unsafe main() {
    signal(SIGINT, handler)
    while (true) {
        println('sleep 1 second')
        sleep(Duration.second)
    }
}