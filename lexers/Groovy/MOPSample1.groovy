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
package com.mg.groovy.samples.mop

import org.junit.Test


/**
 * MOPSample1.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class MOPSample1{
	
	@Test
	def void test_Sample01() {
	    
	    def desc = "If you want a usual Java class to be recognized as a Groovy class, you only\n" + 
	    "have to implement the GroovyObject interface. For convenience, you\n" +
	    "can also subclass the abstract class GroovyObjectSupport, which pro-\n" + 
	    "vides default implementations.\n\n"
	 
	    println desc
	    
	    
	}
	
}
