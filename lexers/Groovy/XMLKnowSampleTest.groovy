package com.mg.groovy.samples.know

import org.junit.Test

/**
 * XMLKnowSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class XMLKnowSampleTest{
	
	@Test
	void test_readXML() {
		def blip = new XmlSlurper().parse(new File('/Users/mangar/projects/blip_download/workspace/GroovyLearningProject/src/com/mg/groovy/samples/know/music_list.xml')) 
		for (music in blip.musics.music) { 
			println "${music.@id} - ${music.artist}" 
		}
	}
	
}