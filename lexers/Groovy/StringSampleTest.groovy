package com.mg.groovy.samples.know

import org.junit.Test;


/**
 * StringSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class StringSampleTest{
	
	@Test
	public void test_String() {
		println "-" * 20 + "   TEST STRING   " + "-" * 20
		
		def message = "Hello"
		assert message == "Hello"
		assert message instanceof java.lang.String
		
		
		message <<= " Message"
		assert message.toString() == "Hello Message"
		assert message instanceof java.lang.StringBuffer
		
		
		message[1..4] = 'i'
		assert message.toString() == "Hi Message"
		assert message instanceof java.lang.StringBuffer
		
		println ">"*30 + "<"*30
		message.find(){ text -> println "${text}" }
		println ">"*30 + "<"*30
		
		
		message <<= " M"
		println message + " / " + message.class
		
		
		message << "a"
		println message + " / " + message.class
		
		
		def nm = "Marcio"
		println nm + " / " + nm.class
		
		nm <<= " Mangar"
		println nm + " / " + nm.class
		
		
		def na = """aaaaa
	            bbbbb""" 
		
		println na		
		
	}
	
}
