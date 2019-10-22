package com.mg.groovy.samples.xml

import org.junit.Test


/**
 * XMLGrooveDOM.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class XMLGrooveDOM{
	
	@Test
	void test_GrooveDOM1() {
		println "\n" + ("test_GrooveDOM - ") * 5
		
		def plan = new XmlParser().parse(new File('/Users/mangar/projects/blip_download/workspace/GroovyLearningProject/src/com/mg/groovy/samples/xml/data.xml')) 
		
		assert 'plan' == plan.name() 
		assert 'week' == plan.week[0].name() 
		assert 'task' == plan.week[0].task[0].name() 
		assert 'read XML chapter' == plan.week[0].task[0].'@title' 
		
		assert 6 == plan.week.task.'@done'*.toInteger().sum()
		
		
		assert 'plan->week->week->task->task->task->task->task' == plan.breadthFirst()*.name().join('->') 

	}
	
}
