package cangjieWeb.mysql
import std.database.sql.{Driver, DriverManager, Datasource}
public class MySqlDriver <: Driver {
    static init() {
        println("mysql register MySqlDriver")
        DriverManager.register("mysql", MySqlDriver())
    }

    public prop name: String {
        get() {
            return "mysql"
        }
    }
    public prop version: String {
        get() {
            return "0.0.1"
        }
    }
    public prop preferredPooling: Bool {
        get() {
            return true
        }
    }

    // 要实现的第一个方法
    // connectionString 格式  username:password@protocol(address)/dbname
    // opts 目前仅支持 charset
    public func open(connectionString: String, opts: Array<(String, String)>): Datasource {
        // let connectionString = "username:password@protocol(192.168.4.44:3307)/dbname"
        let config = parseConnectStringAndParams(connectionString, opts)
        return MySqlDatasource(config)
    }

    private func parseConnectStringAndParams(connectionString: String, opts: Array<(String, String)>): Config {
        let usernameEnd = connectionString.indexOf(":", 0).getOrDefault({ => -1})
        let passwordEnd = connectionString.indexOf("@").getOrDefault({ => -1})
        let protocolEnd = connectionString.indexOf("(").getOrDefault({ => -1})
        let addressEnd = connectionString.indexOf(")").getOrDefault({ => -1})
        let dbnameStart = addressEnd + 2
        let username = connectionString[0..usernameEnd]
        let password = connectionString[usernameEnd + 1 .. passwordEnd]
        let protocol = connectionString[passwordEnd + 1 .. protocolEnd]
        let address  = connectionString[protocolEnd + 1 .. addressEnd]
        let dbname = connectionString[dbnameStart .. ]

        println("username = ${username}, password = ${password}, protocol = ${protocol}, address = ${address}, " 
        + "dbname = ${dbname}")

        let config = Config()
        config.address = address
        config.username = username
        config.password = password
        config.protocol = protocol
        config.dbname = dbname

        for((key, value) in opts) {
            config.params.put(key, value)
        }

        return config
    }
}