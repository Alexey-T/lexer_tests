// 群聊程序-客户端
// 仓颉语言团队 刘俊杰 2024/10/21
import std.socket.*
import std.console.*

const IP = "127.0.0.1"
const PORT: UInt16 = 24621
const BUFFER_SIZE = 1024
var g_quit = false

main() {
    let socket = TcpSocket(IP, PORT)
    socket.connect()
    // 读取服务器发来的消息
    spawn {
        while (!g_quit) {
            let data = Array<Byte>(BUFFER_SIZE, item: 0)
            var count = 0
            // 如果对端正常关闭连接，不会抛出异常，read 返回 0
            try { count = socket.read(data) } catch (e: SocketException) {
                g_quit = true
            }
            if (g_quit || count == 0) { break }
            println(String.fromUtf8(data))
        }
    }
    // 向服务器发送消息
    while (!g_quit) {
        let input = Console.stdIn.readln().getOrDefault {"\n"}
        try { socket.write(input.toArray()) } catch (e: SocketException) {
            g_quit = true
        }
    }
    socket.close()
}