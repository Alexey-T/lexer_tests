package com.mg.groovy.samples.know

import org.junit.Test


/**
 * RangesTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class RangesTest{
	
    @Test
    public void test_Range() {
	
		assert (0..10).contains(0)             
		assert (0..10).contains(5)             
		assert (0..10).contains(10)            
		                                       
		assert (0..10).contains(-1) == false   
		assert (0..10).contains(11) == false   
		assert (0..<10).contains(9)             
		assert (0..<10).contains(10) == false   
		                                        
		def a = 0..10    
		
		//instance of Range...
		assert a instanceof Range   
		assert a.contains(5)        
		                      
		//inplicit construction.....
		a = new IntRange(0,10)   
		assert a.contains(5)     
		                         

//		assert (0.0..1.0).contains(0.5)      

		//date range..... 
		def today     = new Date()              
		def yesterday = today-1                 
		assert (yesterday..today).size() == 2   
		                     
		assert ('a'..'c').contains('b')   
		                                        
		def log = ''             
		for (element in 5..9){   
		    log += element       
		}       
		assert log == '56789'
		
		
		log = ''                 
		for (element in 9..5){   
		    log += element       
		}                        
		assert log == '98765'    
	
    }
	

    
    @Test
    public void test_RangeObjects() {
	
		def result = ''                
	    (5..9).each{ element ->    
	        result += element      
	    }                          
	    assert result == '56789'   
	    assert (0..10).isCase(5)
	    
	    
	    def age = 36  
	    def insuranceRate
	    switch(age){                                        
	        case 16..20 : insuranceRate = 0.05 ; break      
	        case 21..50 : insuranceRate = 0.06 ; break      
	        case 51..65 : insuranceRate = 0.07 ; break      
	        default: throw new IllegalArgumentException()   
	    }                                                   
	    assert insuranceRate == 0.06
	    
	    
	    def ages = [20,36,42,56]                  
	    def midage = 21..50                       
	    assert ages.grep(midage) == [36,42]

	
    }
    
    @Test
    public void test_RangeDate() {
		def mon = new Weekday('Mon') 
		def fri = new Weekday('Fri')
		
		println mon
		println fri
		
//		def worklog = '' 
//		for (day in mon..fri) {
//		    println day
//		    worklog += day.toString() + ' ' 
//		} 
//		assert worklog == 'Mon Tue Wed Thu Fri ' 
    }
    
}
