package cangjieWeb.mysql
import std.database.sql.{Datasource, Connection}
public class MySqlDatasource <: Datasource {

    private let config: Config

    init(config: Config) {
        this.config = config
    }

    public func connect(): Connection {
        return MysqlConnection(config)
    }
    public func setOption(key: String, value: String) {

    }

    public func close() {
        
    }

    public func isClosed(): Bool {
        return false
    }
}