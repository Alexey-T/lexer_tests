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
package com.mg.groovy.samples.spreadmixin

import org.junit.Test


/**
 * SpreadTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class SpreadTest{
	
	def getList() { 
		return [1,2,3] 
	} 
	def sum(a,b,c){ 
		return a + b + c 
	}      
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	def void test_Spread() {
		
		//1
		assert 6 == sum(*list) 
		
		//2
		def range = (1..3) 
		assert [0,1,2,3] == [0,*range] 
		
		//3
		def map = [a:1,b:2] 
		assert [a:1, b:2, c:3] == [c:3, *:map] 
		
		
	}
	
	@Test
	def void test_MixingUse() {
	    use (StringCalculationCategory) { 
		    assert 1    == '1' + '0' 
		    assert 2    == '1' + '1' 
		    assert 'x1' == 'x' + '1' 
		}
	}
	
	
}


class StringCalculationCategory {    
	static def plus(String self, String operand) { 
		try {    
			return self.toInteger() + operand.toInteger() 
		} 
		catch (NumberFormatException fallback){ 
			return (self << operand).toString() 
		} 
	}         
} 