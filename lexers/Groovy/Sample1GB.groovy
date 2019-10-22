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
package com.mg.groovy.samples.groovybeans

import org.junit.Test


/**
 * Sample1GB.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class Sample1GB{
	
	@Test
	def void test_Sample1() {
		println "test_Sample1 - " * 5
		
		def bean = new MrBean(firstname: 'Rowan')   
		bean.lastname = 'Atkinson'
		
		println "FN: ${bean.@firstname} / LN: $bean.lastname --> Full Name: $bean.name"
		
		assert 'Rowan Atkinson' == bean.name 
	}
	
	
	@Test
	def void test_DotAt() {
		println "\n" + ("test_DotAt - " * 5)
		
		def bean = new MrBean(firstname: 'Rowan', lastname: 'Atkinson')   
		println "Original Acessor: ${bean.@firstname}"
		println "Overwrited Acessor: ${bean.firstname}"
		
		println "FN: ${bean.firstname} / LN: $bean.lastname --> Full Name: $bean.name"	        
	}
	
	
	@Test
	def void test_aaa() {
		println "\n" + ("test_" * 5)
		
		def obj = new SomeClass()
		
		def store = [] 
		
		println obj.properties
		
		obj.properties.each { property -> 
			store += property.key 
			store += property.value 
		}
		
		assert store.contains('someField')        == false  //public... 
		assert store.contains('somePrivateField') == false  //private...
		
		assert store.contains('someProperty')  //def
		assert store.contains('class')  //default class..
		assert store.contains('metaClass') //default metaClass...
		
		assert obj.properties.size() == 3 
		
		println store.dump()
		
	}

	@Test
	def void test_Expando() {
	    println "\n" + ("test_Expando - " * 5)
	    
	    def boxer = new Expando()
	    
	    assert null == boxer.takeThis
	    
	    boxer.takeThis = 'ouch!'
	    assert 'ouch!' == boxer.takeThis
	    println boxer.takeThis
	    	 
	    //problem with 'this'
	    boxer.fightBack = { times -> return takeThis * times }
//	    boxer.fightBack = { times -> return this.takeThis * times }	    
	    println boxer.fightBack(5)
	    
	    assert 'ouch!ouch!ouch!' == boxer.fightBack(3) 
	    
	}
	
	
}

class MrBean { 
	String firstname, lastname   
	
	String getName(){   
		return "$firstname $lastname" 
	} 
	
	//     String getFirstname() {
	//	 	return "FirstName( $firstname )"
	//     }
	
	def getFirstname() {
		return "FIXED FIRST NAME"
	}
}



