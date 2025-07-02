package cangjieWeb.mysql
import std.database.sql.{UpdateResult}
public class MySqlUpdateResult <: UpdateResult {

    private var _rowCount: Int64 
    private var _lastInsertId: Int64 

    public init(rowCount: Int64, lastInsertId: Int64) {
        this._rowCount = rowCount
        this._lastInsertId = lastInsertId
    }


    public prop lastInsertId: Int64 {
        get() {
            return _lastInsertId
        }
    }
    public prop rowCount: Int64 {
        get() {
            return _rowCount
        }
    }

    public func close() {
        
    }

    public func isClosed(): Bool {
        return false
    } 
}