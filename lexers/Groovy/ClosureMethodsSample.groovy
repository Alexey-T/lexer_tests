package com.mg.groovy.samples.closures

import org.junit.Test

/**
 * ClosureMethodsSample.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ClosureMethodsSample{
   
    
    
    @Test
    def void test_Currying() {
		println "test_Currying - " *5
	
//		The real power of currying comes when the closureÕs parameters are them- 
//		selves closures.		
		
		def adder = {x, y -> return x+y} 
		def addOne = adder.curry(1)
		assert addOne(5) == 6		
		println addOne(5)
		 		
//		with default...
//        def adder2 = {x, y=2 -> return x+y} 
//        def addOne2 = adder2.curry(3)
//        assert addOne == 3       
//        println adder2.call(1)
		
    }
    

    def caller (Closure closure){ 
        closure.getParameterTypes().size() 
    }     
    
    @Test
    def void test_Parameters() {
		println "test_Parameters - " * 5

		println caller{ one -> }
		assert caller { one -> }      == 1
		
		println caller{ one, two -> }
		assert caller { one, two -> } == 2
		
		println caller{ one, tt, ttt, q -> }
	
    }
    
	
    

    
    @Test
    def void test_CurryClosure() {
		println "test_CurryClosure - " *5
		
		def configurator = { format, filter, line ->   
		    filter(line) ?  format(line) : null
		}
		
		def appender = { config, append, line ->   
		    def out = config(line)                 
		    if (out) append(out)                   
		} 
		
		
		//create  method called: dateFormatter with one parameter (line) that returns a formated string:  "date: line"
		def dateFormatter   = { line -> "${new Date()}: $line" }   
		
		//return true if a string debug contains in the "line"
		def debugFilter     = { line -> line.contains('debug') }
		
		//println the  "line"
		def consoleAppender = { line -> println line }
		
		//define the first and second parameters....
		def myConf = configurator.curry(dateFormatter, debugFilter)   
		
		//defina de first and second parameters....
		def myLog  = appender.curry(myConf, consoleAppender)
		
		myLog('[debug] here is some message') 
		myLog('this will not be printed')
		
		
		//all together....
		def myLog2 = appender.curry(configurator.curry(dateFormatter, debugFilter), consoleAppender)
		myLog2('debug show this in console....')
		
    }
    
}
