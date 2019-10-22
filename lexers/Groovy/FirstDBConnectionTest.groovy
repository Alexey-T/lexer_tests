package com.mg.groovy.samples.db

import org.junit.Test


/**
 * FirstDBConnectionTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class FirstDBConnectionTest extends AbstractDB {
	
	@Test
	def void test_Connection() {
		println "test_Connection - " * 5
		
		println "Connection..."
		def connection = connection()
		
		println "DataSource..."
		def ds = ds()
		
		println "Java...."
		def jcon = getConnection() 
		
		println "GOT!"
	}
	
	
	@Test
	def void test_InsertPreparedStatement() {
		println "\n" + ("test_InsertPreparedStatement - " * 5)
		setUp()
		
		def db = ds()
		def sql_insert = "insert into musics(artist, album, name, date_launched) values(?, ?, ?, ?)"
		
		db.execute sql_insert, ['Better Than Ezra', 'Deluxe', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'Friction Baby', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'How Does Your Garden Grow?', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'Deluxe', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'Closure', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'Before The Robots', '', '2008-12-01']
		db.execute sql_insert, ['Better Than Ezra', 'Artifakt', '', '2008-12-01']
		
	}
	
	@Test
	def void test_SimpleSelect() {
		println "\n" + ("test_SimpleSelect - " *5)
		
		def db = ds()
		db.eachRow('SELECT * FROM musics'){ music -> println "${music.artist} - ${music.album} - ${music.name} - ${music.date_launched}" }
		
	}
	
	@Test
	def void test_QuerySelect() {
		println "\n" + ("test_QuerySelect - " *5)
		
		ds().query('SELECT artist, album FROM musics'){ resultSet -> 
		    if(resultSet.next()){ 
		        print   resultSet.getString(1) 
		        print   ' (-) ' 
		        println resultSet.getString('album') 
		    } 
		} 		
		
		
	}	
	
	@Test
	def void test_AllOnce() {
	    println "\n" + ("test_AllOnce - " *5)

	    List musics = ds().rows('SELECT artist, album FROM musics') 
	    println "There are ${musics.size()} Musics:" 
	    println musics.collect{"${it.album} (${it[0]})"}.join(", ") 

	}
	
	
	
	@Test
	def void test_Update() {
	    println "\n" + ("test_Update - " * 5)
	    
	    def wrong = 'AC/DC'
	    def right = 'AC/BC'
	    ds().eachRow("SELECT * FROM musics WHERE artist = ${wrong}"){ music -> println "${music.artist} - ${music.album} - ${music.name} - ${music.date_launched}" }	    
	    
	    updateArtist(wrong, right)
	    
	    ds().eachRow("SELECT * FROM musics WHERE artist = ${right}"){ music -> println "${music.artist} - ${music.album} - ${music.name} - ${music.date_launched}" }	    
	    
	    
	}

    def updateArtist(wrong, right) {   
        ds().execute """ 
            UPDATE musics 
               SET artist = $right WHERE artist = $wrong; 
        """ 
    } 
	
	
	
}
