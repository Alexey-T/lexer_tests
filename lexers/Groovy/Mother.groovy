package com.mg.groovy.samples.closures

/**
 * Mother.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class Mother{

	int field = 1 
	int foo(){ 
	    return 2 
	} 
	
	//it could be 'def' instead of 'Closure'
	Closure birth (param) {  
	    def local = param 
//	    def closure = { [this, field, foo(), local, param, it, owner]
//	    	println " > Owner: ${owner.dump()}"
//	    	println " > Caller: ${caller}"
//	    	println " > Param: ${param}"
//	    	return [this, field, foo(), local, param, caller, owner]
//	    } 
	    
          def closure = { caller -> [this, field, foo(), local, param, caller, owner]
          } 
	    
	    return closure
//		return "Mangar"




	} 
	
	
	//age is an accumulator....
	def city (age) {
	    def closure = {
//		    age += 1
		    return "Sao Paulo - ${age++}"
	    }
	}

	
	String name = "Mother`s Name"
	Object get(String name) {
	    println "get: $name"
	    return "get: $name"
	}
	void set(String name, Object value) {
	    println "set: $name / $value"
	}
	
	
	static main(args) {
	    println "Groovy main!"
	}
	
}
