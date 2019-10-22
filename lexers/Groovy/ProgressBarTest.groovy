package com.mg.groovy.samples.snippets

import org.junit.Test

/**
 * ProgressBarTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ProgressBarTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	void test_progressBar() {
		println "\n" + ("test_progessBar - " * 5)
		
		
		
		processFiles { filled, info -> 
			print '\b' * 61 
			print '#'*filled + ':'*(10-filled) +' '+ info.padRight(50) 
		} 	    
		
	}
	
	def processFiles(notify) { 
		def names = new File('.').list() 
		names.eachWithIndex { name, i -> 
			notify(i * 10 / names.size(), name) 
			sleep 50   
		} 
	} 
	
}
