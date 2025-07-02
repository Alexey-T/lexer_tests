package cangjieWeb.mysql

import std.net.{TcpSocket}

public class Buffer {
    private var idx: Int64 = 0
    private var length: Int64 = 0
    private var buf: Array<Byte>
    private let socket: TcpSocket

    init(socket: TcpSocket) {
        this.socket = socket
        this.buf = Array<Byte>(4096, item: 0)
    }

    public func readNext(need: Int64): Array<Byte> {
        if (length < need) {
            fill(need)
        }
        
        let data = buf[idx .. idx + need]
        idx += need
        length -= need

        return data
    }

    public func fill(need: Int64) {
        if (length > 0 && idx > 0) {
            buf.copyTo(buf, idx, 0, length - idx)
        }
        if (need > buf.size) {
            grow(need)
        }
        idx = 0
        while (length < need) {
            var readSize = socket.read(buf[length..])
            length += readSize
        }
    }

    public func grow(need: Int64) {
        let newBuf = Array<Byte>(need, item: 0)
        buf.copyTo(newBuf, 0, 0, length)
        this.buf = newBuf
    }
}