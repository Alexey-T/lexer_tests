package com.mg.groovy.samples.know

import org.junit.Test


/**
 * DateSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class DateSampleTest{
	
	
	@Test
	public void test_date()  {
		println "-".multiply(20) + " TEST_DATE " + "-".multiply(20)
		
		def d = new Date()
		assert d instanceof java.util.Date
		
		println "Date 0: ${d}"
		d.setHours(1)
		println "Setted for 1 hour (1:XX) ... ${d}"
		
		
		println "Next Day: ${d.next()}"
		
	}    
	
	
	
}
