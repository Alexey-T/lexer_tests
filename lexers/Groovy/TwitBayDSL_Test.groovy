package com.mg.twitbay.dsl

import org.junit.BeforeClass
import org.junit.Test
/**
 * TwitBayDSL_Test.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class TwitBayDSL_Test{
	
	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	static void setUpBeforeClass() throws Exception{
	}
	
	
	@Test
	void test_Peroform1() {
		println "test_Perform_1"
		
		def twit = new TwitBayDSL()
		
		def a = twit.getProduct {
			productId "290304033677"
			eBayAppId "your_ebay_app_id"
			twUser "your_twitter_username"
			twPassword "your_twitter_password"
		}		
		
	}
	
}
