package com.mg.groovy.samples.know

import org.junit.Before
import org.junit.Test;
import org.junit.Ignore

/**
 * SimpleKnowSample1.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class SimpleKnowSample1Test {
	
	
	@Test
	void test_Numbers() {
		def i = 1
		i = i.plus(1)
		assert i == 2
	}
	
	
	@Test
	void test_BookBean() {
		def book = new BookBean();
		book.price = 10.9
		assert book.price == 10.9
	}
	
	@Test
	void test_List_With_Map() {
		
		def id, artist, title, url, message
		id = "C1"
		artist = "Guns and Roses"
		title =  "Sweet Child o Mine"
		url = "http://marciogarcia.com"
		message = "Yeah baby!"
		
		def map = [(id):["id":"${id}/1", "artist":artist, "title":title, "url":url, "message":message]];
		assert map.size() == 1
		assert map.containsKey(id)
		assert map.containsKey("C1")
		println "${id} .. ${map[id]}"
		println "Full (1): ${map}"		
		
		
		id = "C2"
		artist = "Coldplay"
		title =  "Viva la vida"
		url = "http:coldplay.com"
		message = "Winner?!"    
		
		map[(id)] = ["id":"${id}/1", "artist":artist, "title":title, "url":url, "message":message]
		assert map.size() == 2
		assert map.containsKey(id)
		assert map.containsKey("C1")
		println "${id} .. ${map[id]}"
		println "Full (2): ${map}"
		
	}
	
	
}
