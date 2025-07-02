// 群聊程序-服务器
// 仓颉语言团队 刘俊杰 2024/10/21
import std.collection.*
import std.collection.concurrent.*
import std.console.*
import std.socket.*
import std.sync.*
import std.time.Duration

const PORT: UInt16 = 24621
const BUFFER_SIZE = 1024
var g_quit = false

// 从控制台读取退出命令
func controller() {
    spawn {
        while (!g_quit) {
            let key = Console.stdIn.read().getOrDefault {r' '}
            if (key == r'q' || key == r'Q') {
                g_quit = true
            }
        }
    }
}

extend TcpSocket {
    public func tryWrite(data: Array<Byte>) {
        try { this.write(data) } catch (e: SocketException) {
            if (!this.isClosed()) { this.close() }
            return false
        }
        return true
    }
}

main() {
    let messages = NonBlockingQueue<(TcpSocket, Array<Byte>)>()
    let members = LinkedList<TcpSocket>() // 群聊成员-客户端
    let server = TcpServerSocket(bindAt: PORT) // 服务端
    server.bind()
    controller()

    // 和新的客户端建立连接
    let mutex = ReentrantMutex()
    let monitor = Monitor()
    spawn {
        while (!g_quit) {
            let client = server.accept()
            synchronized (mutex) {
                members.append(client)
            }
            // 每个连接要处理的任务
            spawn {
                while (!g_quit) {
                    let data = Array<Byte>(BUFFER_SIZE, item: 0)
                    try { client.read(data) } catch (e: SocketException) {
                        return // 遇到异常终止此线程，由转发线程清理资源
                    }
                    println(String.fromUtf8(data))
                    messages.enqueue((client, data))
                    synchronized (monitor) { monitor.notify() }
                }
            }
        }
    }

    // 转发消息到所有客户端，实现群聊
    while (!g_quit) {
        // 此处 Monitor 仅用于避免 while 空转，可以注释掉以下三行代码对比进程 CPU 消耗情况
        synchronized (monitor) {
            monitor.wait(timeout: Duration.millisecond * 100)            
        }
        while (let Some((client, data)) <- messages.dequeue()) {
            mutex.lock()
            // 遍历群聊成员，转发消息并清理无效客户端
            members.removeIf { socket =>
                if (refEq(client, socket)) { // 不必转发给消息源
                    return false
                }
                !socket.tryWrite(data)
            }
            mutex.unlock()
        }
    }
    // 退出时通知所有客户端
    for (client in members) {
        if (client.tryWrite("Server Exit".toArray())) {
            client.close()
        }
    }
    server.close()
}