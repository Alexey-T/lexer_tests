package com.mg.groovy.samples.db.infra

import org.junit.Test

/**
 * MusicTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class MusicTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	void test_DAO() {
	    println "\n" + ("test_DAO - " * 5)

	    def helper     = new DbHelper()                  
	    def musicDAO = new MusicDAO(db: helper.db)	    
	    
	    println "Linsting all records..."
	    def musics = musicDAO.all('id')
	    musics.each { println it }
	    
	    print "\nCreating a new record..."
	    def newData = ['R.E.M', '', 'Live', '2007-01-01']
	    println newData
	    musicDAO.create(newData)
	    
        println "\nLinsting all records..."
        musics = musicDAO.all('id')
        musics.each { println it }
	    
	}

	
}
