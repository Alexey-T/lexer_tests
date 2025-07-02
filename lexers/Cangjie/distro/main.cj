package cangjieWeb
import serialization.serialization.*
import encoding.json.*
import std.database.sql.{DriverManager, SqlDbType, SqlBigInt, SqlInteger, SqlVarchar}
import cangjieWeb.mysql.MySqlDatasource
import std.collection.{ArrayList}
main() {
    let connectionString = "root:12345678@tcp(10.200.1.164:3306)/admin-api"
    let opts = [
        ("cachePrepStmts", "true")
    ]
    let drv = DriverManager.getDriver("mysql") ?? return
    let ds = drv.open(connectionString, opts)
    let connection = ds.connect()

    let deleteTable = """
        drop table if exists `sys_dept`;
    """
    let deleteTableStatement = connection.prepareStatement(deleteTable)
    let deleteTableParams = ArrayList<SqlDbType>()
    let deleteTableResult = deleteTableStatement.update(deleteTableParams.toArray())
    println("delete table sys_dept if exists success")
        

    let createTable = """
        CREATE TABLE `sys_dept` (
            `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
            `parent_id` int NOT NULL COMMENT '父id',
            `dept_type` int NOT NULL COMMENT '部门类型（1->公司, 2->中心，3->部门）',
            `dept_name` varchar(30) CHARACTER SET utf8mb3 COLLATE utf8mb3_bin NOT NULL COMMENT '部门名称',
            `dept_status` int NOT NULL DEFAULT '1' COMMENT '部门状态（1->正常 2->停用）',
            PRIMARY KEY (`id`) USING BTREE,
            UNIQUE KEY `dept_name` (`dept_name`) USING BTREE
        ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_bin ROW_FORMAT=DYNAMIC COMMENT='部门表';"""

    let createTableStatement = connection.prepareStatement(createTable)
    let createTableParams= ArrayList<SqlDbType>()
    let createTableResult = createTableStatement.update(createTableParams.toArray())
    println("create table sys_dept success")

    // INSERT INTO `admin-api`.sys_dept(id, parent_id, dept_type, dept_name, dept_status) VALUES(1, 0, 1, 'develop1', 1);
    let insertTableStatement = connection.prepareStatement("INSERT INTO sys_dept(parent_id, dept_type, dept_name, dept_status) VALUES(?,?,?,?);")
    var companyUpdateResult = insertTableStatement.update(SqlInteger(0), SqlInteger(1), SqlVarchar("cangjie company"), SqlInteger(1))
    var heNanUpdateResult = insertTableStatement.update(SqlInteger(Int32(companyUpdateResult.lastInsertId)), SqlInteger(2), SqlVarchar("henan center"), SqlInteger(1))
    var zheJiangUpdateResult = insertTableStatement.update(SqlInteger(Int32(companyUpdateResult.lastInsertId)), SqlInteger(2), SqlVarchar("zhejiang center"), SqlInteger(1))
    insertTableStatement.update(SqlInteger(Int32(heNanUpdateResult.lastInsertId)), SqlInteger(3), SqlVarchar("henan development1"), SqlInteger(1))
    insertTableStatement.update(SqlInteger(Int32(heNanUpdateResult.lastInsertId)), SqlInteger(3), SqlVarchar("henan development2"), SqlInteger(1))
    insertTableStatement.update(SqlInteger(Int32(zheJiangUpdateResult.lastInsertId)), SqlInteger(3), SqlVarchar("zhejiang development1"), SqlInteger(1))
    insertTableStatement.update(SqlInteger(Int32(zheJiangUpdateResult.lastInsertId)), SqlInteger(3), SqlVarchar("zhejiang development2"), SqlInteger(1))
    println("insert into table sys_dept, 7 rows")

    let id = SqlInteger(0)
    let parentId = SqlInteger(0)
    let deptType = SqlInteger(0)
    let deptName = SqlVarchar("")
    let deptStatus = SqlInteger(0)

    let statement = connection.prepareStatement("select * from `sys_dept`;")
    let param = ArrayList<SqlDbType>()
    let queryResult = statement.query(param.toArray())
    var tempIndex = 1
    println("query sys_dept all data")
    while(queryResult.next(id, parentId, deptType, deptName, deptStatus)) {
        println(" ${tempIndex} rows data, id = ${id.value}, parentId = ${parentId.value}," + 
            "deptType = ${deptType.value}, deptName = ${deptName.value}, deptStatus = ${deptStatus.value}"
        )
        tempIndex ++ 
    }

    tempIndex = 1
    println("query sys_dept where dept_type = 2")
        let statement2 = connection.prepareStatement("select * from sys_dept where dept_type = ?;")
        param.append(SqlBigInt(2))
    let queryResult2 = statement2.query(param.toArray())
    while(queryResult2.next(id, parentId, deptType, deptName, deptStatus)) {
        println(" ${tempIndex} rows data, id = ${id.value}, parentId = ${parentId.value}," + 
            "deptType = ${deptType.value},  deptName = ${deptName.value}, deptStatus = ${deptStatus.value}"
        )
        tempIndex ++ 
    }
}