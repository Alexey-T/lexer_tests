package com.mg.groovy.samples.xml

import org.junit.Test


/**
 * RSSReaderTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class RSSReaderTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	void test_RSS() {
		println "\n" + ("test_RSS - ") * 5
		
		def url = "http://blog.marciogarcia.com/rss"
		
		println 'The top three news items today:' 
		def items = new XmlParser().parse(url).channel[0].item 
		for (item in items[0..2]) { 
			println "${item.title.text()} - ${item.link.text()}" 
			println item.description.text().substring(1, 100)
			println '-----------------------------------------------------------------' 
		} 
		
	}
	
}
