package com.mg.groovy.samples.gdk

import org.junit.Test


/**
 * InteractiveTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class InteractiveTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	def void test_Interactive1() {
	    println "test_Interactive1 - " * 5
	    
	    def newline = "NAME...\n" 
	    
	    println "toString: ${newline.toString()} \n"
//	      assert newline.toString() == "\n"

	    println "dump(): ${newline.dump()} \n"
//		assert newline.dump() == 
//		'''<java.lang.String@a value=[ 
//		] offset=0 count=1 hash=10>'''

		println "inspect(): ${newline.inspect()}"

		assert newline.inspect() == /"NAME...\n"/ 
	    
		
		//SLEEP..............................
		println "starting..."
		sleep 1000
		println "... doing something ..."
		sleep 1000
		println "...finished!"
		
	    
	}
	
}
