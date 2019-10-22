package com.mg.groovy.samples.gdk

import org.junit.Test

/**
 * IOTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class IOTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	def void test_ReadFile() {
		println "test_ReadFile - " * 5
		
		def file = new File("/Users/mangar/projects/blip_download/workspace/GroovyLearningProject/src/com/mg/groovy/samples/gdk/test_file.txt")
		file.eachLine() { println it}
		//	    file.each{println it}
		
		//	    assert file.any {it =~ /File/} 
		//	    assert 3 == file.findAll{it =~ /File/}.size() 
		//	    assert 5 ==  file.grep{it}.size() 	    
	
 
		file = new File('.') 
		println file.name 
		println file.absolutePath 
		println file.canonicalPath 
		println file.directory 		
		
		println file.canonicalPath + "/src/" + this.getClass().getPackage().getName().replace(".", "/") + ".groovy"
		
		println this.getClass().getCanonicalName()
		
	}
	
	@Test
	def void test_FileRead() {
	    println "test_FileRead - " * 5
	    
	    def example = new File('/Users/mangar/projects/blip_download/workspace/GroovyLearningProject/src/com/mg/groovy/samples/gdk/test_file.txt')
	    println "readLines(): \n ${example.readLines()}"
	    
	    def lines = ["line 1", "line two", "line III", "line 4", "line five", "line VI"] 
	    assert lines == example.readLines()
	    
	    example.eachLine { 
	        assert it.startsWith('line') 
	    }
	    
	    
	    
	    def hex = [] 
	    example.eachByte { hex << it }
	    println "example after .eachByte{hex << it} : " + example
	    
	    assert hex.size() == example.length() 
	    example.splitEachLine(/\s/){
			println "splitEachLine: " + it[0] + it[1]
	        assert 'line' == it[0] 
	    } 
	    
	    
	    example.withReader { reader ->
	    	println "withReader: ${reader.readLine()}"
//	        assert 'line 1' == reader.readLine() 
	    }
	    
//	    example.withInputStream { is -> 
//	        assert 'line one' == is.readLine() 
//	    } 
	    
	}
	

	@Test
	def void test_Out() {
	    println "testOut - " * 5
	    
	    def outFile = new File('./out.txt')
	    outFile.delete()
	    
	    def lines = ['line one','line two','line three']
	    
	    def content = ""
	    lines.each() {
			content += it + "\n"
			outFile.append(it + "\n")  
		}
	    
	    println "file.write: overwrite the file..."
	    //	    outFile.write(lines[0..1].join("\n"))

		println "file.append: append content into the file..."
		//	    outFile.append("\n"+lines[2])           
	    
	    println "readLines: " + outFile.readLines()
	    println "content: " + content
	    
	    assert lines == outFile.readLines()

	    
	    //2 - write...........
	    outFile.withWriter { writer ->                      
	        writer.writeLine(lines[0])                      
	    }
	    println "2..: " + outFile.readLines()
	    
	    
	    //like append..........
	    outFile.withWriterAppend('ISO8859-1') { writer ->   
	        writer << lines[1] << "\n"                      
	    } 
	    println "3..: " + outFile.readLines()	        
	        
	    outFile << lines[2]
	    println "4..: " + outFile.readLines()
	    
	    assert lines == outFile.readLines()	    
	    
	    
	    //MyTests.....
	    outFile.delete()
	    outFile = new File('./out.txt')
	        
	    println "File Clean"
	    
	    println "outFile << (append) 'Text'"
	    outFile << lines[0] + "\n" << lines[1] + "\n" + lines[2] + "\n"
	    
	    println "File NOT Clean: ${outFile.readLines()}"
	    
	    
	    
	    
	}
	
	
	
}
