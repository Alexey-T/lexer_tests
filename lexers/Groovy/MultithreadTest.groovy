package com.mg.groovy.samples.gdk

import org.junit.Test


/**
 * MultithreadTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class MultithreadTest{
	
    @Test
    def void test_Thread01() {
		println "test_Thread01 - " * 5

		//1
		println "\nbefore the thread...."
		def t = new Thread() {
		    println " (t) starting thread..."
		    println " (t) ending thread."
		} 
		t.start() 
		println "after the thread"
		
		
		//2
		sleep(1000)
		
		println "\nmethod 2 ... Thread.start  (start)"
		Thread.start { println " (t) thread .running" }
		println "method 2 (end)"
		
		//3
		sleep(1000)
		println "\nmethod 2 ... Times.runAfter (start)"
		new Timer().runAfter(1000){ 
		    println " (t) Times.runAfter .running"
		} 
		println "method 2 ... Times.runAfter (end)"
		
    }
	
}
