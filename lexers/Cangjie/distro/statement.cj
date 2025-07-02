package cangjieWeb.mysql
import std.database.sql.{Statement, ColumnInfo, QueryResult, SqlDbType, UpdateResult, SqlNullableBigInt, SqlBigInt, SqlVarchar, SqlInteger}
import std.collection.{ArrayList}
import cangjieWeb.utils.ByteArrayUtils
public class MySqlStatement <: Statement {

    private let connection: MysqlConnection
    private let sql: String
    private var id : UInt32 = 0
    private var columnCount: UInt16 = 0
    private var paramCount: UInt16 = 0
    private let _parameters: Array<ColumnInfo> = Array<ColumnInfo>()
    private let _columns: Array<ColumnInfo> = Array<ColumnInfo>()

    private var rowCount: Int64 = 0
    private var lastInsertId: Int64 = 0



    init(connection: MysqlConnection, sql: String) {
        this.connection = connection
        this.sql = sql

        this.connection.writeCommandPacketStr(comStmtPrepare, sql)
        readPrepareResultPacket()
    }

    public prop parameterColumnInfos: Array<ColumnInfo> {
        get() {
            return ArrayList<ColumnInfo>().toArray()
        }
    }


    public func query(params: Array<SqlDbType>): QueryResult {
        // 发送
        writeExecutePacket(params)
        // 接收结果
        let columnNumber = readRequstSetHeaderPacket()
        // println("columnCount = ${columnNumber}")
        var columns: ArrayList<MySqlField> = ArrayList<MySqlField>()
        var rows: ArrayList<Array<Byte>> = ArrayList<Array<Byte>>()
        if (columnNumber > 0) {
            // read columns
            columns = this.connection.readColumns(UInt16(columnNumber))
            // read rows
            rows = this.connection.readRows()
        }
        return MysqlQueryResult(columns, rows)
    }
    public func setOption(key: String, value: String): Unit {

    }
    public func update(params: Array<SqlDbType>): UpdateResult {
        // 发送
        writeExecutePacket(params)

        // 接收结果
        let updateResult = readRequstSetHeaderPacket()        
        return MySqlUpdateResult(rowCount, lastInsertId)
    }

    public func close() {
        
    }

    public func isClosed(): Bool {
        return false
    }

    private func readRequstSetHeaderPacket(): Int64 {
        let data = connection.readPacket()
        match(data[0]) {
            case 0 => 
                handleOkPacket(data[1..])
                return 0
            case 255 =>
                println("err, do later")
                return -1
            case _ => 
                1 + 1
                // println("unknow, do later")
                // return -1
        }

        // column count
        let (number, isNull, n) = ByteArrayUtils.readLengthEncodedInteger(data)
        return Int64(number)
    }

    private func handleOkPacket(data: Array<Byte>) {
        // Affected rows [Length Coded Binary]
	    let (affectedRows, affectedRowsIsNull, n) = ByteArrayUtils.readLengthEncodedInteger(data[0..])

	    // Insert id [Length Coded Binary]
	    let (insertId, insertIdIsNull, m )= ByteArrayUtils.readLengthEncodedInteger(data[n..])
        rowCount = Int64(affectedRows)
        lastInsertId = Int64(insertId)
    }


    private func readPrepareResultPacket() {
        let data = this.connection.readPacket()
        if (data[0] != iOK) {
            println("error here, do later")
        }

        id = ByteArrayUtils.littleEndianUint32(data[1..5])
        columnCount = ByteArrayUtils.littleEndianUint16(data[5..7])
        paramCount = ByteArrayUtils.littleEndianUint16(data[7..9])
        if (paramCount > 0) {
           let params:ArrayList<MySqlField> = this.connection.readColumns(paramCount)
        } 
        if (columnCount > 0) {
            let columns = this.connection.readColumns(columnCount)
        }
    }

    private func writeExecutePacket(params: Array<SqlDbType>) {
        connection.sequence = 0
        var pktLen = 1 + 4 + 1 + 4 + ((Int64(paramCount) + 7) / 8) + 1 + (Int64(paramCount) * 2)
        let paramValues = ArrayList<Array<Byte>>() 
        let paramTypes = Array<Byte>(Int64(paramCount) * 2, item: 0)
        var bitMask = UInt64(0)

        for(i in 0 .. params.size) {
            let param = params.get(i).getOrThrow()
            // 先只处理 SqlBigInt 和 SqlNullableBigInt
            match(param.name) {
                case "SqlNullableBigInt" => 
                    let temp = param as SqlNullableBigInt
                    let value = temp.getOrThrow()
                    match(value.value) {
                        case Some(number) => 
                            paramTypes[i<<1] = fieldTypeLongLong
                            let appenValue = ByteArrayUtils.uint64ToBytes(UInt64(number))
                            paramValues.append(appenValue)
                            pktLen += 8
                        case None => 
                            bitMask += 1 << UInt64(i)
                            paramTypes[i<<1] = fieldTypeNULL
                    }
                    
                case "SqlBigInt" => 
                    let temp = param as SqlBigInt
                    let number = temp.getOrThrow().value
                    paramTypes[i<<1] = fieldTypeLongLong
                    let appenValue = ByteArrayUtils.uint64ToBytes(UInt64(number))
                    paramValues.append(appenValue)
                    pktLen += 8

                case "SqlInteger" =>
                    let temp = param as SqlInteger
                    let number = temp.getOrThrow().value
                    paramTypes[i<<1] = fieldTypeLong
                    let appenValue = ByteArrayUtils.uint32ToBytes(UInt32(number))
                    paramValues.append(appenValue)
                    pktLen += 4

                case "SqlVarchar" =>
                    let temp = param as SqlVarchar
                    let s = temp.getOrThrow().value
                    paramTypes[i<<1] = fieldTypeVarString
                    let result = ArrayList<Byte>()
                    var lengthBytes = ByteArrayUtils.appendLengthEncodedInteger(UInt64(s.toArray().size))
                    result.appendAll(lengthBytes)
                    result.appendAll(s.toArray())
                    paramValues.append(result.toArray())
                    pktLen = pktLen + result.toArray().size

                case _ => println("unsupport data type")
            }
        }

        let data = Array<Byte>(pktLen + 4, item: UInt8(0))

        data[0] = UInt8(pktLen & 0xff)
	    data[1] = UInt8(pktLen >> 8)
	    data[2] = UInt8(pktLen >> 16)
	    data[3] = connection.sequence

        data[4] = comStmtExecute

        data[5] = UInt8(id)
        data[6] = UInt8(id>>8)
        data[7] = UInt8(id>>16)
        data[8] = UInt8(id>>24)

        data[10] = 0x01

        if (paramCount > 0) {
            var pos = 14 + ((Int64(paramCount) + 7) / 8)
            for(i in 14 .. pos) {
                data[i] = UInt8(bitMask >> UInt64((i - 14)* 8))
            }

            data[pos] = 0x01
            pos ++

            paramTypes.copyTo(data, 0, pos, paramTypes.size)
            pos += paramTypes.size
            for(i in 0 .. paramCount) {
                let paramValue = paramValues[Int64(i)]
                paramValue.toArray().copyTo(data, 0, pos, paramValue.toArray().size)
                pos += paramValue.toArray().size
            }
        }
        connection.writePacket(data)
    }

}