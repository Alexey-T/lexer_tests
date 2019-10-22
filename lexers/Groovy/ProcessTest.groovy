package com.mg.groovy.samples.gdk

import org.junit.Test


/**
 * ProcessTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ProcessTest{
	
    @Test
    def void test_Process01() {
		println "test_Process01 - " * 5
		
		def lscmd = "ls"
		def process = lscmd.execute()

		println "Content:"
		process.text.split("\n").each() {
		    println " - ${it}"
		}
		
		
    }
}
