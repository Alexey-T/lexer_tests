package com.mg.groovy.samples.db.infra

import groovy.sql.Sql 


abstract class DataAccessObject {
    
    Sql db
    
    abstract List getFields()
    
    def dataSet()    { db.dataSet(tablename) }
    
    def getIdField() { /*tablename.toLowerCase() +*/ 'id' }
    
    def getWhereId() { "WHERE $idField = ?"}
    
    String getTablename() { 
        def name = this.getClass().name.toLowerCase()
        return name[name.lastIndexOf('.')+1..-4] + 's'
    }
    
    def create(List args) {   
        Map argMap = [:] 
        args.eachWithIndex { arg, i -> argMap[fieldNames[i]] = arg } 
        dataSet().add argMap 
    }
    
    Map getSchema() { 
        Map result = [:] 
        fieldNames.each {result[it] = fields[fields.indexOf(it)+1]} 
        return result 
    }
    
    List getFieldNames() { 
        List result = [] 
        0.step(fields.size(),2) { result << fields[it] } 
        return result 
    }
    
    def update(field, newValue, id) { 
        def stmt = "UPDATE $tablename SET $field = ? $whereId" 
        db.executeUpdate stmt, [newValue, id] 
    }
    
    def delete(id) { 
        def stmt = "DELETE FROM $tablename $whereId" 
        db.executeUpdate stmt, [id] 
    }
    
    def all(sortField) {             
        def selects = fieldNames + idField 
        def result = [] 
        def stmt = "SELECT " + selects.join(', ') + " FROM ${tablename} ORDER BY $sortField"
            
        db.eachRow(stmt.toString()){ rs -> 
        	Map businessObject = [:] 
        	selects.each { businessObject[it] = rs[it] } 
        	result << businessObject 
        } 
        return result 
    } 
} 
