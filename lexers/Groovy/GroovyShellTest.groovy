package com.mg.groovy.samples.know

import org.junit.Test


/**
 * GroovyShellTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class GroovyShellTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	void test_GroovyShell() {
		
		def shell = new GroovyShell() 
		def result = shell.evaluate("12 + 23") 
		assert result == 35 	    
		
	}
	
	@Test
	void test_GroovyShell_Java() {
		GroovyShell shell = new GroovyShell(); 
		Object result = shell.evaluate("12+23"); 
		assert new Integer(35).equals(result);	    
	}
	
	
	
	@Test
	void test_ParametersShell() {
	    
	    def binding = new Binding(x: 6, y: 4)   
	    def shell = new GroovyShell(binding) 
	    shell.evaluate(''' 
	        xSquare = x * x       
	        yCube   = y * y * y   
	    ''') 
	    assert binding.getVariable("xSquare") == 36    
	    assert binding.yCube == 64 
	    
	}

	
	@Test
	void test_GenerateClass() {
	    
	    def shell = new GroovyShell() 
	    def clazz = shell.evaluate('''
	        class MyClass {                
	            def method() { "value" }   
	        }                              
	        return MyClass
	    ''') 
	    assert clazz.name == "MyClass" 
	    def instance = clazz.newInstance()   
	    assert instance.method() == "value"
	    
	    
	}
	
}
