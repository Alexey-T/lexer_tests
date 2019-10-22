package com.mg.groovy.samples.closures

/**
 * MethodClosureSample.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
class MethodClosureSample{
	
	int limit 
	
	MethodClosureSample (int limit) {   
		this.limit = limit              
	}                                     
	
	boolean validate (String value) { 
		//	    println "   value: ${value}"
		return value.length() <= limit    
	}   
	
	
	
    MethodClosureSample () {   
              
    } 

    //multimethod...    
	int mysteryMethod (String value) { 
		return value.length() 
	} 
	
	int mysteryMethod (List list) { 
		return list.size() 
	} 
	
	int mysteryMethod (int x, int y) { 
		return x+y 
	} 
	
	
	
}
