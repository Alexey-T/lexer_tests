package com.mg.groovy.samples.db

import org.junit.Test

/**
 * DataSetsTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class DataSetsTest extends AbstractDB {
	
	@Test
	def void test_DataSet(){
		println "\n" + ("tesT_DataSet - " * 5)
		setUp()
		def musicsSet = ds().dataSet('musics')
		
		musicsSet.each{ println it }
	}
	
	
	@Test
	def void test_DataSet_FindAll(){
		println "\n" + ("tesT_DataSet_FindAll - " * 5)
		setUp()
		
		def ds = connection()
		def acdc = ds.dataSet('musics').findAll {it.id == 0 }
		
		//		acdc.each { println it }
		
		println "DataSet.findAll { condition ... }"
		//		def acdc = musicsSet.findAll {data -> data.date_launched > '2007-01-01' }
		
		//				println acdc.dump()
		//				println acdc?.sql
		//		println acdc.size()
		
	}
	
	
	
	
	
}
