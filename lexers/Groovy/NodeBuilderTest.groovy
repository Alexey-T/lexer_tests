package com.mg.groovy.samples.builder

import org.junit.Test


/**
 * NodeBuilderTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class NodeBuilderTest{
	
	/**
	 * @throws java.lang.Exception
	 */
	@Test
	def void test_Builder() {
	    
	    //THE MAGIC new NodeBuilder()
	    def builder = new NodeBuilder()
	    
	    def ulcDate = new Date(107,0,1) 
	    def invoices = builder.invoices{   
	        invoice(date: ulcDate){                 
	            item(count:5){ 
	                product(name:'ULC', dollar:1499) 
	            }

	       item(count:1){ 
	            product(name:'Visual Editor', dollar:499) 
	        } 
	    }
	        
	    invoice(date: new Date(106,1,2)){ 
	        item(count:4) { 
	            product(name:'Visual Editor', dollar:499) 
	        } 
	    } 
	    
	} 
	    
//	soldAt = invoices.grep {                            
//	       it.item.product.any{ it.'@name' == 'ULC' }   
//	   }.'@date'
	   
//	println invoices.findAll() { println it.item.product.any { it.'@name'} }

	println invoices.findAll() { 
	    println "-> Invoice: $it"
	    it.item.each { 
			println "  -> Item: " + it.@count + " / " + it.product.@name
		} 
	}


	}

	
	@Test
	def void test_XMLNodeBuilder() {
	    println "\n\n" + ("XMLNodeBuilder - "*5)
	    
	    def writer = new StringWriter() 
	    
	    //	  THE MAGIC new groovy.xml.MarkupBuilder()
	    def builder = new groovy.xml.MarkupBuilder(writer)   
	    def invoices = builder.invoices {     
	        for(day in 1..3) {                               
	            invoice(date: new Date(106,0,day)){          
	                item(count:day){ 
	                    product(name:'ULC', dollar:1499) 
	                } 
	            } 
	        } 
	    }
	    
	    def result = writer.toString().replaceAll("\r","")
	    println result
	}

	
	@Test
	def void test_HTMLNodeBuilder() {
	    println "\n\n" + ("HTMLNodeBuilder - " * 5)
	    
        def writer = new StringWriter()
	    
	    //	   THE MAGIC new groovy.xml.MarkupBuilder()
	    def html = new groovy.xml.MarkupBuilder(writer)
	    html.html {
			head {
			    title ("Constructed by Markup")
			}
			body {
			    center {
					h1 "My First HTML generated"
			    }
			    hr {}
			}
	    }
	    
        def result = writer.toString().replaceAll("\r","")	   
	    println result
	    
	}
	
	
	@Test
	def void test_AntBuilder() {
	    println "\n\n" + ("test_AntBuilder - " * 5)
	    
//	    <project name="AntIf" default="main" > 
//	    <target name="check.java.version"> 
//	        <condition property="java.version.ok"> 
//	            <contains string="${java.version}" substring="1.4"/> 
//	        </condition>        
//	        <fail unless="java.version.ok"> 
//	            This build script requires JDK 1.4.x. 
//	        </fail> 
//	    </target> 
//	    <target name="main" 
//	        depends="check.java.version" 
//	        if="java.version.ok"> 
//	    
//	        <!-- further action --> 
//	    
//	    </target> 
//	</project> 
	    
//	    def ant = new AntBuilder() 
//	    if ( ! System.properties.'java.version'.contains('1.4')) { 
//	        ant.fail 'This build script requires JDK 1.4.x' 
//	    } 
	    
	}
	
	
	
}

 

//class Invoice {                         
//    List    items                       
//    Date    date                        
//}                                       
//class LineItem {                        
//    Product product                     
//    int     count                       
//    int total() {                       
//        return product?.dollar * count   
//    }                                   
//}                                       
//class Product {                         
//    String  name                        
//    def     dollar                      
//}
