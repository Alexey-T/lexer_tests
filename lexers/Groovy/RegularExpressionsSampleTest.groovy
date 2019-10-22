package com.mg.groovy.samples.know

import org.junit.Test


/**
 * RegularExpressionsSampleTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class RegularExpressionsSampleTest{
	
    //find: =~
    //match: ==~
    //pattern: ~String	
    
    //() group
    //(x|y) x or y
    //x+ at least once
    //x* zero or more times
    //x? zero or one 
    //x{m} exactly
    //x{m,n} at least m at most n
    //[1-9] range 1..9
    //[a-c] range a, b, c
    //^start
    //\w word
    //\W except word
    //\d decimal
    //\D except decimal    
    
	@Test
	public void test_RegularExpressions() {
		
		assert "abc" == "${/abc/}"
		
		def reference = "hello"
		assert "hello" == /$reference/
		
		def text = "01"
		def res = (text ==~ /([0-3])?([1-9]+)/)
		println "Text: ${text} / Res.: ${res}"
		
		
		text = "bla bla bla bla bla bla bla bla bla"
		println text
		def re = /(bla)+/
		text = text.replaceAll(re, "zzz")
		println text
		
		def words = text.split()
		println "Words: ${words}"
		
		text = text.replace(/ /, ';')
		
		words = text.split()
		println "Words, wrong split: ${words} size: ${words.size()}"		
		
		words = text.split(/;/)
		println "Words, right split: ${words}  size: ${words.size()}"
		
		
	}
	
	
	@Test
	public void test_Boundary() {
		//find =~
		
		def myFairStringy = 'The rain in Spain stays mainly in the plain!' 
		println myFairStringy
		
		//		 words that end with 'ain': \b\w*ain\b 
		def BOUNDS = /\b/ 
		def	rhyme = /$BOUNDS\w*ain$BOUNDS/ 
		def	found = '' 
		
		
		//eachMatch
		myFairStringy.eachMatch(rhyme) { match ->   
		    found += match[0] + ' ' 
		} 
		assert found == 'rain Spain plain '
		
		
		//each
		found = '' 
		(myFairStringy =~ rhyme).each { match ->   
		    found += match + ' ' 
		} 
		assert found == 'rain Spain plain '
		
		
		//replaceAll
		def cloze = myFairStringy.replaceAll(rhyme){ it-'ain'+'___' }   
		assert cloze == 'The r___ in Sp___ stays mainly in the pl___!'
				
		println cloze
		
	    def cloze2 = myFairStringy.replace(/ain/, '___')   
//	    assert cloze == 'The r___ in Sp___ stays mainly in the pl___!'
	                
	    println cloze2		
		
	}
	
	
	@Test
	public void test_Matcher() {
	    
	    //==~
	    
	    //1
	    def matcher = 'a b c' =~ /\S/
	    println matcher
	    
	    assert matcher[0]    == 'a' 
	    assert matcher[1..2] == 'bc' 
	    assert matcher.count == 3 	    
	    
	    
	    //2
	    matcher = 'a:1 b:2 c:3' =~ /(\S+):(\S+)/ 
	    assert matcher.hasGroup() 
	    assert matcher[0] == ['a:1', 'a', '1'] 
	    println matcher
	    println matcher[0] + " /" + matcher[1].class
	    
	    //3
	    ('xyzxyz' =~ /(.)(.)(.)/).each() {all, a1, a2, a3 ->
			println " -> ${all} / ${a1} / ${a2} / ${a3}"
	    }
	    
	}
	
	
	@Test
	public void test_Pattern() {
	    
	    def twister = 'she sells sea shells at the sea shore of seychelles' 
//		 some more complicated regex: 
//		 word that starts and ends with same letter 
		def regex = /\b(\w)\w*\1\b/ 
		def start = System.currentTimeMillis() 
		100000.times{ 
		    twister =~ regex   
		} 
		def first = System.currentTimeMillis() - start 
		start = System.currentTimeMillis() 
		def pattern = ~regex                   
		100000.times{ 
		    pattern.matcher(twister)   
		} 
		def second = System.currentTimeMillis() - start 
		println first + " > " + second * 1.20
	}
	
	
	@Test
	public void test_Classification() {
	    
	    assert (~/..../).isCase('bear') 
	    switch('bear'){ 
	        case ~/..../ : assert true; break 
	        default      : assert false 
	    } 
	    def beasts = ['bear','wolf','tiger','regex'] 
	    assert beasts.grep(~/..../) == ['bear','wolf'] 
	    
	}
	
	
}
