package cangjieWeb.mysql
import std.database.sql.{QueryResult, ColumnInfo, SqlDbType, SqlInteger, SqlVarchar}
import std.collection.{ArrayList}
import cangjieWeb.utils.ByteArrayUtils
public class MysqlQueryResult <: QueryResult {

    private let columns: ArrayList<MySqlField>
    private let rows: ArrayList<Array<Byte>>
    private var tempRowsIndex = 0
    private var nullBitMapStartPos = 1
    private var nullBitMapEndPos = 0
    private var tempRowPos = 0

    init(columns: ArrayList<MySqlField>, rows: ArrayList<Array<Byte>>) {
        this.columns = columns
        this.rows = rows
        this.nullBitMapEndPos = (this.columns.size + 7) / 8 + nullBitMapStartPos
    }


   public prop columnInfos: Array<ColumnInfo> {
        get() {
            return ArrayList<ColumnInfo>().toArray()
        }
    }
    public func next(values: Array<SqlDbType>): Bool {
        tempRowPos = nullBitMapStartPos + 1
        if (tempRowsIndex < rows.size) {
            for(i in 0 .. values.size) {
                fillData(values[i])
            }
            tempRowsIndex ++
            return true
        }
        return false
    }

    public func close() {
        
    }

    public func isClosed(): Bool {
        return false
    }

    private func fillData(value: SqlDbType) {
            match (value) {
                case v: SqlInteger	=> 
                    let number = ByteArrayUtils.littleEndianUint32(rows[tempRowsIndex][tempRowPos..])
                    v.value = Int32(number)
                    tempRowPos += 4
                case v: SqlVarchar => 
                    let (s, isNull, n) = ByteArrayUtils.readLengthEncodedString(rows[tempRowsIndex][tempRowPos..])
                    v.value = String.fromUtf8(s)
                    tempRowPos += n
                case _ =>
                    println("unsupport Type")
            }
    }
}