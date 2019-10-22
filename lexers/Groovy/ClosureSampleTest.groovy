package com.mg.groovy.samples.know

import org.junit.Test


/**
 * ClosureSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ClosureSampleTest{
	
	@Test
	void test_Closure1() {
		
		def totalClinks = 0 
		def partyPeople = 100
		
		1.upto(partyPeople) { guestNumber -> 
			def clinksWithGuest = guestNumber-1 
			totalClinks += clinksWithGuest 
		} 
		def calc = (partyPeople*(partyPeople-1))/2
		println "${totalClinks} == ${calc}"
		assert totalClinks == (partyPeople*(partyPeople-1))/2 
		
	}
	
	
	
	@Test
	void test_listClosure() {
		def list = [1, 2, 3, 4, 5, 6, 7, 8, 9]
		list.each() { item -> print "${item} - " }
	}
	
	
	
	
}
