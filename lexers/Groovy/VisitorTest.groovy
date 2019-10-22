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
package com.mg.groovy.samples.closures

import org.junit.Test


/**
 * VisitorTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class VisitorTest{
	
	@Test
	def void test_VisitorPattern() {
	    println "test_VisitorPattern - " * 5
		def picture = new Drawing(shapes: 
				[new Square(width:1), new Circle(radius:1)] )
	    
	    def total = 0 
	    picture.accept { total += it.area() }
	    
	    println "The shapes in this drawing cover an area of $total units." 
	    println 'The individual contributions are: ' 
	    picture.accept { println it.class.name + ":" + it.area() } 
	    
	    
	}
	
}
