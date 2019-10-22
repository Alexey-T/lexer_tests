/**
 * Copyright (c) 2008 MarcioGarcia [http://marciogarcia.com]
 *  
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *  
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *  
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.mg.groovy.samples.controlstructures

import org.junit.Test


/**
 * ControlStructuresOneTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ControlStructuresOneTest{
	
     public var01
     private var02
     def x = 1
     
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	def void test_ControlStructure_6() {
		
		def x = 1              
		if (x == 2) {   
			assert false 
		}                      
		/******************* 
		 if (x =  2) {   //ERROR!
		 println x 
		 }                      
		 ********************/ 
		println "X@Start: $x"
		if ((x = 3)) {   
			println x 
		}        
		println "X@End: $x"
		assert x == 3    
		
		def store = []      
		while (x = x - 1) {   
			store << x 
		} 
		assert store == [2, 1] 
		while (x =  1) {   
			println "while (x = 1) ..: $x"   
			break 
		} 
		
	}
	
	def static site = "http://127.0.0.1:8080/mangarblog"	
	
	@Test
	def void test_Switch() {
		
		
		//usage of isCase...
		//if (10.isCase(valueToCheck).....
		
		switch (10) { 
			case 0          : assert false ; break 
			case 0..9       : assert false ; break 
			case [8,9,11]   : assert false ; break 
			case Float      : assert false ; break   
			case {it%3 == 0}: assert false ; break   
			case ~/../      : assert true  ; break   
			default         : assert false ; break 
		} 
		
		//	    Class a.isCase(b) implemented as 
		//	    Object a.equals(b) 
		//	    Class a.isInstance(b) 
		//	    Collection a.contains(b) 
		//	    Range a.contains(b) 
		//	    Pattern a.matcher(b.toString()).matches() 
		//	    String (a==null && b==null) || a.equals(b) 
		//	    Closure a.call(b) 
		
		
		
		println site
		println site.toURL().getHost()
		println site.toURL().getPath()
		
		
		
	}

	@Test
	def void test_Loop() {
	    println "test_Loop - " * 5
	    
	    
	    def list = [1,2,3]
	    println "List: $list"
	    while (list) { 
	        list.remove(0) 
	    } 
	    assert list == []
	    println "List: $list"
	    
	    while (list.size() < 3) list << list.size()+1 
	    assert list == [1,2,3] 	    
	    println "List: $list"
	}
	
	
	
	
	@Test
	def void test_ForLoop() {
	    println "test_ForLoop - " * 5
	    def store = ''                          
		for (String i in 'a'..'c') store += i   
		assert store == 'abc'                   
		store = ''               
		for (i in [1, 2, 3]) {   
		    store += i           
		}                        
		assert store == '123'    
		def myString = 'Equivalent to Java'   
		store = ''                            
		for (i in 0 ..< myString.size()) {    
		    store += myString[i]              
		}                                     
		assert store == myString              
		            
		store = ''                 
		for (i in myString) {      
		    store += i             
		}                          
		assert store == myString    
	    
	}
}
