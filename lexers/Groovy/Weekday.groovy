package com.mg.groovy.samples.know

/**
 * Weekday.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class Weekday{
	
	static final DAYS = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
	
	private int index = 0
	
	Weekday(String day){   
		index = DAYS.indexOf(day) 
	}    
	Weekday next(){ 
		return new Weekday(DAYS[(index+1) % DAYS.size()]) 
	}    
	Weekday previous(){ 
		return new Weekday(DAYS[index-1])   
	} 
	int compareTo(Object other){ 
		return this.index <=> other.index 
	} 
	String toString(){ 
		return DAYS[index] 
	} 
	
}
