package com.mg.groovy.samples.know

import org.junit.Test


/**
 * NumberSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class NumberSampleTest{
	
    @Test
    public void test_NumberFunctions() {

		def store = '' 
	    10.times{     
	        store += 'x' 
	    } 
	    assert store == 'xxxxxxxxxx'
	    
	    //upto + closure...	    
	    store = '' 
	    1.upto(5) { number ->   
	        store += number 
	    } 
	    assert store == '12345'
	    
	    //downto + closure
	    store = '' 
	    2.downto(-2) { number ->   
	        store += number + ' ' 
	    } 
	    assert store == '2 1 0 -1 -2 ' 
	    
	    
	    //step + closure....
	    store = ''
	    
	    0.step(11, 1) { n ->
	    	store += n + ' - '
	    }
	    println store
	    
	    store = ''
	    0.step(0.5, 0.1 ){ number ->   
	        store += number + ' ' 
	    } 
	    assert store == '0 0.1 0.2 0.3 0.4 '	
	
    }
    
    
}
