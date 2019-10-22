
package com.mg.groovy.samples.gdk

import org.junit.BeforeClass
import org.junit.Test


/**
 * FiltersTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class FiltersTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	static void setUpBeforeClass() throws Exception{
	    def file = new File('./out.file')
	    file.delete()
	    
	    def lines = ['line one', 'line two', 'line three', 'line one again']
	    lines.each() { file.append(it + "\n")}
	    
	    println "File content: ${file.readLines()}"
	}
	
	
	@Test
	def void test_Filter() {
	    println "test_Filter - " * 5
	    
	    def reader = new StringReader('abc')                   
	    def writer = new StringWriter()
	    
	    reader.transformChar(writer) {
			
			println it.next() 
		}
	    
//	    assert 'bcd' == writer.toString()
//	    
//	    reader = new File('data/example.txt').newReader()  
//	    writer = new StringWriter() 
//	    reader.transformLine(writer) { it - 'line' }   
//	    assert " one\r\n two\r\n three\r\n" == writer.toString()
//	    
//	    input  = new File('data/example.txt') 
//	    writer = new StringWriter() 
//	    input.filterLine(writer) { it =~ /one/ }   
//	    assert "line one\r\n" == writer.toString()
//	    
//	    writer = new StringWriter() 
//	    writer << input.filterLine { it.size() > 8 }   
//	    assert "line three\r\n"  == writer.toString() 
	    
	}

	
	@Test 
	def void test_SerializedObject() {
	    println "test_SerializedObject - " * 5
	    
	    def file = new File('objects.dta') 
	    def out  = file.newOutputStream() 
	    def oos  = new ObjectOutputStream(out)
	    def objects = [1, "Hello Groovy!", new Date()]
	    println "To Store on file: ${objects}"
	    
	    println "Saving..."
	    objects.each {            
	        oos.writeObject(it)   
	    }                         
	    oos.close()
	    println "..saved!"
	    
	    def retrieved = []                        
	    file.eachObject { retrieved << it }
	    
	    println "From File: ${retrieved}"
	    assert retrieved == objects
	    
	}
	
	
}
