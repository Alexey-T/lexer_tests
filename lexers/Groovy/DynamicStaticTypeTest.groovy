package com.mg.groovy.samples.know

import org.junit.Test;


/**
 * DynamicStaticTypeTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class DynamicStaticTypeTest{
	
	@Test
	public void test_static_dynamic() {
		def a = 1
		assert a.class == Integer.class
		
		int i = 1
		assert i.class == Integer.class
		
		a = "Marcio"
		assert a.class == String.class 
		
		//		NOT WORKS!!!!
		//		i = "Mangar"
		//		assert i.class == String.class
		
	}

}
