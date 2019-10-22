package com.mg.groovy.samples.closures

import org.junit.Test


/**
 * ClosureOneSample.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ClosureOneSample{

    def closure_definition = 
    "A closure is a piece of code wrapped up as an object. It acts like a method \n" +  
    "in that it can take parameters and it can return a value. ItÕs a normal object in that\n" +  
    "you can pass a reference to it around just as you can a reference to any other \n" + 
    "object.\n\n" + 
    "Sample:\n" + 
    "addressBook.each { new Letter(it).send() }\n"
    
    
    @Test
    def void test_Sample() {
		println "Closure Definition \n${'-'*30} \n${closure_definition}"
	
	
		println "Hey! ... ${one()}"
    }
	
    /**
     * 
     */
    def one() {
		"one"
    }
    
    @Test
    def void test_Iterators() {
		println "test_Iterators - " *5
	
		def list = [0..9]

		println "--> Java 4"
		for (int i = 0; i < list.size(); i++) {
		    println list.get(i)
		}
		
		println "--> Java 5 (generics)" 
		for (Integer i : list) {
		    println i
		}
		
		println "--> Groovy"
		list.each() { println (it) }

    }
    
    @Test
    def void test_HandlingResource() {
		println "test_HandlingResource - " * 5	
		new File('/Users/mangar/projects/blip_download/workspace/GroovyLearningProject/src/com/mg/groovy/samples/closures/myfile.txt').eachLine { println it } 
		
    }
    
    @Test
    def void test_Simple() {
		println "test_Simple - " *5
		
		//explicit defined the variable...
		def log = '' 
	    (1..10).each{ counter -> log += counter } 
	    assert log == '12345678910'
	    println "1 - Explicit: ${log}"
	    
	    //implicit defined variable...
	    log = '' 
	    (1..10).each{ log += it } 
	    assert log == '12345678910'
	    println "1 - Implicit (it): ${log}"
	    
	    log = '' 
	     (1..10).each({ counter -> log += counter }) 
	    assert log == '12345678910'
	    println "2 - Explicit: ${log}"
	            
	    //implicit defined variable...
	    log = '' 
	    (1..10).each({ log += it }) 
	    assert log == '12345678910'
	    println "2 - Implicit (it): ${log}"	    
	    
	    log = '' 
        (1..10).each() { counter -> log += counter } 
	    assert log == '12345678910'
		println "3 - Explicit: ${log}"
		                   
		//implicit defined variable...
		log = '' 
		(1..10).each() { log += it } 
		assert log == '12345678910'
		println "3 - Implicit (it): ${log}"     
	           
	           
    }
    
    @Test
    def void test_MethodClosureSample() {
		println "test_MethodClosureSample - " *5
		
		def words = ['long string', 'medium', 'short', 'tiny']		
		
		MethodClosureSample first = new MethodClosureSample (6)
		Closure firstClosure = first.&validate
		
		assert 'medium' == words.find (firstClosure)		
		
		println "1- " + words.find (firstClosure)
		
		
		MethodClosureSample second = new MethodClosureSample (5)
		assert 'short' == words.find (second.&validate)
		
		println "2- " + words.find { word -> second.&validate(word) }
		println "3- " + words.find(second.&validate)
		
		
		MethodClosureSample third = new MethodClosureSample(50)
		println "4- " + words.find (third.&validate)
		
		
    }

    @Test
    def void test_Multimethod() {
		println "test_Multimethod - " *5

		MethodClosureSample instance = new MethodClosureSample() 
		Closure multi = instance.&mysteryMethod
		
		assert 10 == multi ('string arg')
		println "multi(string): ${multi ('string arg')}"
		
		assert 3 == multi (['list', 'of', 'values'])
		println "multi(array): ${multi (['list', 'of', 'values'])}"
		
		assert 14 == multi (6, 8)
		println "multi(int, int): ${multi (6,8)}"
		
    }

    @Test
    def void test_Declare() {
		def map = ['a':1, 'b':2]                              
		map.each{ key, value -> map[key] = value * 2 }   
		assert map == ['a':2, 'b':4]                      
		                                                  
		def doubler = {key, value -> map[key] = value * 2 }   
		map.each(doubler)                                 
		assert map == ['a':4, 'b':8]
    }
    
    @Test
    def void test_Calling() {
		def adder = { x, y -> return x+y }
		
		//implicit...
		assert adder(4, 3) == 7
		
		//calling the call method...
		assert adder.call(2, 6) == 8	
    }
    
    
    
    def benchmark(repeat, Closure worker){   
	    def start = System.currentTimeMillis()
	    
	    repeat.times{worker(it)}                           
	    
	    def stop = System.currentTimeMillis()   
	    
	    return stop - start                 
    } 
    
    @Test
    def void test_Benchamark_GOOD() {
		println "test_Benchmark - " * 5
	
	    def slow = benchmark(10000) { (int) it / 2 }   
	    def fast = benchmark(10000) { it.intdiv(2) }
	    println "Slow: ${slow} / Fast: ${fast} : ${fast < slow}"
	    assert fast * 15 < slow
	
	    
	    //	  other ways.....
	    def slow2 = benchmark(10000, { (int) it / 2 })   
	    
	    def closure = {(int)it/2}
	    def slow3 = benchmark(10000, closure)
	    
    }

    @Test
    def void test_DefaultValueAsParameter() {
		println "test_DefaultValueAsParameter - " *5
		
		def closure = {x, y=5 -> return x+y}
	
		println closure(1)
		println closure(1,1)
		
		println closure.call(2)
		println closure.call(2,1)
		
		
    }
	
    
}
