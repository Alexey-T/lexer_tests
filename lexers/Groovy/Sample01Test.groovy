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
package com.mg.groovy.samples.methodspars

import org.junit.Test

import com.mg.groovy.samples.methodspars.business.Vendor as Vend
import com.mg.groovy.samples.methodspars.business.Address as Addr

/**
 * Sample01Test.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class Sample01Test{
	
     Sample01Test() {
	 
     }
     def name
     def age    
     
     def surname
     def city
     
     Sample01Test(name, age, Object[] more) {
	 	
     }
     
     
     
     
     def method01(a=1, b=2, Object[] more) {
	 	"Test return... $a / $b / $more"
     }

	@Test
	void test_Sample1() {
	    println "test_Sample1 - " * 5
	    
	    def desc = "Where there’s a string, you can generally also use a GString. So how about \n" + 
	    		   "obj.\"\${var}\"()? Yes, this is also possible, and the GString will be \n" + 
	    		   "resolved to determine the name of the method that is called on the object! GEEKS\n" 

	    println desc 
	    
		def sample = new Sample01Test()
		
		def methodName = "method01"
		println "method name: $methodName"
		
		def content = sample."$methodName"()
		println "Result of call: $content"
		
		
		content = sample.method01(10, 20)
		println "Result of call (2): $content"

	    content = sample.method01(10, 20, 3, 4, 5, 6, 7, 8, 9)
	    println "Result of call (2): $content"
		
		
	}

	
	@Test
	void test_MapIfInterr() {
	    println "test_MapIfInterr - " * 5
	    
	    def map = [a:[b:[c:1]]]
	    println "map.a.b.c: $map.a.b.c"
	    assert map.a.b.c == 1 
	    
	    if (map && map.a && map.a.x){
			println  "never into here!"
	        assert map.a.x.c == null 
	    }	    
	    
	    println "-" * 15
	    println map?.a?.b?.x?.z
		    
	}
	

	@Test
	void test_Constructor() {
	    println "test_Constructor - " * 5
	    
	    Sample01Test a = new Sample01Test();
	    
//	    def b = ['marcio', '29'] as Sample01Test
//	    println b.name
//	    assert b.name == 'marcio'
//	    assert b.age == '29'
	    
//	    Sample01Test c = ['marcio', '29']
//	    println c.dump()
	    
//	    Sample01Test d = ['marcio', '29', 3, 4, 5]
	    
	    //named parameters....
	    def na = new Sample01Test(name:'Marcio', age:'29', surname:'Garcia', city:'Sydney')
	    assert na.name == 'Marcio'
	    assert na.age == '29'
		assert na.surname == 'Garcia'
		assert na.city == 'Sydney'
		    
	    println "Data: $na.name $na.surname / $na.age / $na.city"
	    
	    
	    //implicit...
//	    Sample01Test ni
//	    ni = ['Marcio', '29']
//        println "Data: $ni.name $ni.surname / $ni.age / $ni.city"
	    
	}
	
	
	@Test
	void test_Vendor() {
	    println "test_Vendor - " * 5
	    
	    def vendor = new Vend(name:'Name', product:'Bala Chita')
	    println vendor.dump()
	    
	    
	}
	
	
}

 

 
 