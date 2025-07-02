package cangjieWeb.mysql
import std.database.sql.{Connection, Transaction, ConnectionState, Statement}
import std.collection.{Map, HashMap, ArrayList}
import std.net.{TcpSocket, IPSocketAddress, SocketException, SocketKeepAliveConfig}
import cangjieWeb.utils.ByteArrayUtils
import encoding.hex.{toHexString,fromHexString}
public class MysqlConnection <: Connection {

    private let config: Config
    private let tcpSocket: TcpSocket
    private let buffer: Buffer
    protected var sequence: Byte = 0
    private var cipher: Array<Byte> = Array<Byte>()
    private var charset: Byte = 0
    private var flags: UInt32 = 0
    private var plugin: String = ""

    private var maxPacketAllowed: Int64 = maxPacketSize
	private var maxWriteSize: Int64 = maxPacketSize - 1

    init(config: Config) {
        this.config = config

        // 构造tcpSocket
        var socket = TcpSocket(IPSocketAddress.parse(config.address))
        socket.keepAlive = SocketKeepAliveConfig()
        
        // 三次握手建立连接
        socket.connect()
        this.tcpSocket = socket
        this.buffer = Buffer(socket)
        // 处理handshake, mysql server 向 client 发送
        readInitPacket()

        // Send Client Authentication Packet
        writeAuthPacket()
        // handleAuthResult
        handleAuthResult()

        // get max allowed packet size
        // let result = getSystemVar("max_allowed_packet")
    }

    public func getMetaData(): Map<String, String> {
        HashMap([])
    }
    public func createTransaction():Transaction {
        return MySqlTransaction()
    }

    public prop state: ConnectionState {
        get() {
            ConnectionState.Connected
        }
    }

    public func close() {
        
    }

    public func isClosed(): Bool {
        return false
    }

    public func prepareStatement(sql: String): Statement {
        return MySqlStatement(this, sql)
    }

    private func readInitPacket() {
        let data = readPacket()
        
        // 协议版本: 1 字节
        // 服务器版本: 以 \0 结尾的字符串
        // 连接 ID: 4 字节
        var pos = 1 + ByteArrayUtils.indexByte(data[1..], 0) + 1 + 4 // 11

        // 密码加密部分: 8 字节
        cipher = cipher.concat(data[pos..pos + 8])
        // 填充字节: 1 字节（始终为 0x00）
        pos = pos + 8 + 1 

        // 能力标志（低 2 字节）: 2 字节
        flags = UInt32(ByteArrayUtils.littleEndianUint16(data[pos .. pos + 2]))
        pos = pos + 2

        // 字符集: 1 字节
        charset = data.get(pos).getOrThrow()
        
        // 状态标志: 2 字节
        // 能力标志（高 2 字节）: 2 字节
        // 认证插件数据长度: 1 字节
        // 保留字节: 10 字节（全部为 0x00）
        pos = pos + 1 + 2 + 2 + 1 + 10 // 38

        // 密码加密部分: 可变长度（通常为 12 字节）
        cipher = cipher.concat(data[pos..pos + 12])
        pos += 13

        let end = ByteArrayUtils.indexByte(data[pos..], 0)
        if (end != -1) {
            plugin = String.fromUtf8(data[pos .. pos + end])
        }else {
            plugin = String.fromUtf8(data[pos .. ])
        }
    }

    private func writeAuthPacket() {
        var clientFlags: UInt32 = 
		    clientProtocol41 |  
			clientSecureConn | 
			clientLongPassword | 
			clientTransactions | 
			clientLocalFiles |  
            clientPluginAuth |  
            clientMultiResults | 
            flags &
            clientLongFlag 
	    

        let authResp = ByteArrayUtils.scrambleCachingSha2(cipher, config.password.toArray())

        // 4 长度 + sequence
        // 4 clientFlags
        // 4 个空位
        // 1 charset
        // 23 空
        // username
        // 1 以0结尾
        // 1 密码长度
        // 密码
        // dbname

        let authRespLen = authResp.size
        let authRespLEI = ByteArrayUtils.appendLengthEncodedInteger(UInt64(authRespLen))
        if (authRespLEI.size > 1) {
            clientFlags |= clientPluginAuthLenEncClientData
        }
        var pktLen = 4 + 4 + 1 + 23 + config.username.toArray().size + 1 + authRespLEI.size + authResp.size + 21 
	    if (config.dbname.size > 0) {
		    clientFlags |= UInt32(clientConnectWithDB)
		    pktLen += config.dbname.toArray().size + 1
	    }

        let data = Array<Byte>(pktLen + 4, item: UInt8(0))

        data[0] = UInt8(pktLen & 0xff)
	    data[1] = UInt8(pktLen >> 8)
	    data[2] = UInt8(pktLen >> 16)
	    data[3] = sequence

        data[4] = UInt8(clientFlags & 0xff)
	    data[5] = UInt8(clientFlags >> 8 & 0xff)
	    data[6] = UInt8(clientFlags >> 16 & 0xff)
	    data[7] = UInt8(clientFlags >> 24 & 0xff)

        data[12] = charset

        var pos = 13 + 23

        if (config.username.size > 0) {
            config.username.toArray().copyTo(data, 0, pos, config.username.toArray().size)
            pos += config.username.toArray().size
        }
        pos += 1

        authRespLEI.copyTo(data, 0, pos, authRespLEI.size)
        pos += authRespLEI.size

        authResp.copyTo(data, 0, pos, authResp.size)
        pos += authResp.size

        if (config.dbname.size > 0) {
            config.dbname.toArray().copyTo(data, 0, pos, config.dbname.toArray().size)
            pos +=  config.dbname.toArray().size
            pos += 1
        }
        plugin.toArray().copyTo(data, 0, pos, plugin.toArray().size)
        pos += plugin.toArray().size
        writePacket(data)
    }

    private func handleAuthResult() {

        var result = readAuthResult()
        var authData = result[0]
        var newPlugin = result[1]
        if (newPlugin != "") {
            if (authData.size > 0) {
                // cipher = authData
                authData.copyTo(cipher, 0, 0, cipher.size)
            } else {
                authData = cipher
            }
            plugin = newPlugin
            let authResp = ByteArrayUtils.scrambleCachingSha2(authData, config.password.toArray())
            writeAuthSwitchPacket(authResp)

            result = readAuthResult()
            authData = result[0]
            newPlugin = result[1]
        }

        match(plugin) {
            case "caching_sha2_password" => 
                match(authData.size) {
                    case 1 =>
                        match(authData[0]) {
                            case 4 =>
                                // request public key from server
                                let pktLen = 1
        
                                let data = Array<Byte>(pktLen + 4, item: UInt8(0))

                                data[0] = UInt8(pktLen & 0xff)
                                data[1] = UInt8(pktLen >> 8)
                                data[2] = UInt8(pktLen >> 16)
                                data[3] = sequence

                                data[4] = cachingSha2PasswordRequestPublicKey
                                writePacket(data)

                                let publicKeyData = readPacket()

                                sendEncryptedPassword(publicKeyData[1..])

                                readPacket()
                            case _ =>
                                println("eror or correct but do later")
                        }
                    case _ =>
                        println("error size or correct size but do later ")
                }
            case _ => 
                println("other, do later")
            
        }
    }

    private func writeAuthSwitchPacket(authData: Array<Byte>) {
        let pktLen = authData.size
        
        let data = Array<Byte>(pktLen + 4, item: UInt8(0))

        data[0] = UInt8(pktLen & 0xff)
	    data[1] = UInt8(pktLen >> 8)
	    data[2] = UInt8(pktLen >> 16)
	    data[3] = sequence

        authData.copyTo(data, 0, 4, authData.size)
        writePacket(data)
    }

    private func readAuthResult() {
        // let data = fromHexString(data: String)

        let data = readPacket()
        
        match(data[0]) {
            case 1 =>
                return (data[1..], "")
            case 254 => 
                if (data.size == 1) {
                    return (Array<Byte>(), "mysql_old_password")
                }
                let pluginEndIndex = ByteArrayUtils.indexByte(data, 0)
                if (pluginEndIndex < 0) {
                    return (Array<Byte>(), "")
                }
                let plugin = String.fromUtf8(data[1..pluginEndIndex])
                let authData = data[pluginEndIndex + 1 ..]
                return (authData, plugin)
            case _ => 
                println("other, do later")
                return (Array<Byte>(), "")
        }
    }

    private func sendEncryptedPassword(publicKey: Array<Byte>) {
        let encrypted = ByteArrayUtils.scrambleRSA(config.password, publicKey, cipher)
        ("encrypted = " + encrypted.toString())
        writeAuthSwitchPacket(encrypted)
    }

    protected func writeCommandPacketStr(command: Byte, sql: String) {
        sequence = 0
        let pktLen = 1 + sql.toArray().size

        let data = Array<Byte>(pktLen + 4, item: UInt8(0))

        data[0] = UInt8(pktLen & 0xff)
	    data[1] = UInt8(pktLen >> 8)
	    data[2] = UInt8(pktLen >> 16)
	    data[3] = sequence

        data[4] = command
        sql.toArray().copyTo(data, 0, 5, sql.toArray().size)
        writePacket(data)

    }

    // private func getSystemVar(name: String): Array<Byte> {

    // }

    protected func readColumns(count: UInt16): ArrayList<MySqlField> {
        let returnResult = ArrayList<MySqlField>()
        
        for(i in 0..count) {
            let data = readPacket()
            var pos = 0
            let mysqlField = MySqlField()
            

            // catalog
            var result = ByteArrayUtils.readLengthEncodedString(data)
            var catalog = String.fromUtf8(result[0])
            pos += result[2]

            // database
            result = ByteArrayUtils.readLengthEncodedString(data[pos..])
            var database = String.fromUtf8(result[0])
            pos += result[2]
            // tableName
            result = ByteArrayUtils.readLengthEncodedString(data[pos..])
            var tableName = String.fromUtf8(result[0])
            pos += result[2]
            mysqlField.setTableName(tableName)
            
            // org_table
            result = ByteArrayUtils.readLengthEncodedString(data[pos..])
            var orgTableName = String.fromUtf8(result[0])
            pos += result[2]

            // name
            result = ByteArrayUtils.readLengthEncodedString(data[pos..])
            var name = String.fromUtf8(result[0])
            pos += result[2]
            mysqlField.setName(name)

            // org_name
            result = ByteArrayUtils.readLengthEncodedString(data[pos..])
            var orgName = String.fromUtf8(result[0])
            pos += result[2]
            // filter
            pos ++

            mysqlField.setCharSet(data[pos])
            pos += 2

            mysqlField.setLength(ByteArrayUtils.littleEndianUint32(data[pos..pos + 4]))
            pos += 4

            mysqlField.setFieldType(UInt16(data[pos]))
            pos ++

            mysqlField.setFlags(ByteArrayUtils.littleEndianUint16(data[pos .. pos + 2]))
            pos += 2

            mysqlField.setDecimals(data[pos])
            returnResult.append(mysqlField)
        }

        let data = readPacket()
        // match(data[0]) {
        //     case 254 => println("read end")
        //     case _ => println("error, do later")
        // }
        return returnResult
    }

    protected func readRows():ArrayList<Array<Byte>> {
        let result = ArrayList<Array<Byte>>()
        while(true) {
            let data = readPacket()
            match(data[0]) {
                case 254 =>
                    break
                case _ =>
                    result.append(data)
            }
        }
        return result
    }




    protected func readPacket(): Array<Byte> {
        let data = buffer.readNext(4)
        let pktLen = Int64(UInt32(data[0]) | UInt32(data[1])<<8 | UInt32(data[2])<<16)
        if (data[3] != sequence) {
            println("ERROR sequence NOT EQUAL")
        }
        sequence ++

        let resultData = buffer.readNext(pktLen)
        if (pktLen < maxPacketSize) {
            return resultData
        }
        let resultData2 = readPacket()
        return resultData.concat(resultData2)
    }

    protected func readUntilEOF(): Array<Byte> {
        let result = ArrayList<Byte>()
        while (true) {
            let tempData = readPacket()
            match(tempData[0]) {
                case 254 => break
                case 255 => println("error happen, do later")
                case _ => result.appendAll(tempData)
            }
        }
        return result.toArray()
    }

    protected func writePacket(data: Array<Byte>) {
        // TODO 拆包发送
        // if (data.size - 4 <= maxWriteSize) {
        try {
            this.tcpSocket.write(data)
            sequence ++
        } catch (e: SocketException) {
            println("send fail " + e.message)
        } finally {
            // println("send success")
        }
        // } 
        // splitPacket(data)
    }
}