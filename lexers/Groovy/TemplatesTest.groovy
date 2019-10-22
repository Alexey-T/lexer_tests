package com.mg.groovy.samples.gdk

import org.junit.Test


/**
 * TemplatesTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class TemplatesTest{
	
	@Test
	def void test_SimpleTemplateEngine() {
		println "test_SimpleTemplateEngine - " * 5
		
		def expecting = '''Dear Mrs. Davis, 
another month has passed and it's time for these 2 tasks: 
 - visit the Groovy in Action (GinA) page
 - chat with GinA readers
your collaboration is very much appreciated'''
		
		println "Expected:\n ${expecting}"
		
		def mailReminder = '''Dear ${salutation?salutation+' ':''}$lastname,    
another month has passed and it's time for these <%=tasks.size()%> tasks:                           
<% tasks.each { %> - $it                            
<% } %>your collaboration is very much appreciated        
'''
		
		def engine   = new groovy.text.SimpleTemplateEngine()       
		def template = engine.createTemplate(mailReminder)   
		
		def binding  = [                                            
				salutation: 'Mrs.',                                      
				lastname  : 'Davis',                                     
				tasks     : ['visit the Groovy in Action (GinA) page',   
				'chat with GinA readers']                   
				]
		
		println "\nGenerated:\n ${template.make(binding).toString()}"
		//		    assert template.make(binding).toString() == expecting    
		
		//          - SimpleTemplateEngine produces the template in terms of a script as dis- 
		//		    cussed previously. At make time, that script writes line-by-line to the output 
		//		    destination. The script is cached. 
		//		    - GStringTemplateEngine holds the template in terms of a writable closure, 
		//		    possibly providing better performance and scalability for large templates 
		//		    and for stateless streaming scenarios. See section 12.2.2. 
		//		    - XmlTemplateEngine is optimized when the template’s raw text and the 
		//		    resulting text are both valid XML. It operates on nodes in the DOM and 
		//		    can thus provide a pretty-printed result. Unlike other engines, it produces 
		//		    system-dependent line feeds. 
		
		
	}
	
}
