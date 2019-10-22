package com.mg.groovy.samples.closures

import org.junit.Test


/**
 * Scoping.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class Scoping{
	
	@Test
	def void test_Scopping1() {
	    println "test_Scopping1 - " * 5
	    
	    def x = 0         
	    println "X: ${x}"
	    10.times {     
	        x++       
	    }             
	    assert x == 10
	    
	    println "X: ${x}"
	}
	
	
	@Test
	def void test_Mother() {
	    println "test_Mother - " * 5
	    
	    Mother julia = new Mother()
	    def age = 30
	    
	    println "MName: $julia.name"
	    julia.name = "Julia"	    
	    println "MName: $julia.name"
	            
	    julia['name'] = "Julia []"        
	    println "MName: $julia.name"	    
	    
	    println "Nao eh campo: $julia.abc"
	    
	    
	    def closure = julia.birth(age)  
	    println "Closure: ${closure}"
	    
	    def param = this
	    def context = closure.call(param)
	    println "Age: ${(age)}"	    
	    println "Context: ${context}"
	    
	    context = closure.call(param)
	    println "Context: ${context}"
	    
	    println "context[0].class.name: " + context[0].class.name   
	    assert context[1..4] == [1,2,(age),(age)]   
	    assert context[5] instanceof Scoping   
	    assert context[6] instanceof Mother   
	    
	    def firstClosure  = julia.birth(4)                   
	    def secondClosure = julia.birth(4)
	    
	    assert false == firstClosure.is(secondClosure)
	    
	    
	    def city = julia.city(1)
	    println city.call()
	    println city.call()
	    println city.call()
	    println city.call()
	    
	    city = julia.city(11)
	    println city.call()
	    println city.call()
	    println city.call()
	    println city.call()
	    
	    
	}


    def foo(n) { 
        return {n += it} 
    }       
	
	
//	@Test
//	def void test_Accumulator() {
//	    println "test_Accumulator - " * 5
//	    
//	    def accumulator =  foo(1)
//	    
//	    def acc = accumulator(2)
//	    println acc
//	    assert acc == 3
//	    
//	    acc = accumulator(1)
//	    println acc
//	    assert acc == 4
//	}
	
	
}
