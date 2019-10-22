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
package com.mg.groovy.samples.style

import org.junit.Test


/**
 * Equalizer.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class EqualizerMultimethod{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	void test_Equalizer2() {
	    println "test_Equalizer2 - " * 5
	    
	    Object same  = new EqualizerObject() 
	    Object other = new Object()
	    
	    println new EqualizerObject().equals( same  )
	    assert   new EqualizerObject().equals( same  )
	    
	    println new EqualizerObject().equals( other )
	    assert ! new EqualizerObject().equals( other )
	    
	}
	
}
  
 
 class EqualizerObject { 
     boolean equals(EqualizerObject e){ 
         return true 
     } 
 } 
 

 
 
