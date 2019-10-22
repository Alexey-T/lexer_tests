package com.mg.groovy.samples.know

import org.junit.Test


/**
 * MapTest.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class MapTest{
	

    @Test
    public void test_MapSample() {
	
		def myMap = [a:1, b:2, c:3]
		
		//checking the map....
		assert myMap instanceof HashMap 
		assert myMap.size() == 3 
		assert myMap['a']   == 1 
		
		//empty map....
		def emptyMap = [:] 
		assert emptyMap.size() == 0
		
		//TreeMap....
		def explicitMap = new TreeMap() 
		explicitMap.putAll(myMap) 
		assert explicitMap['a'] == 1 	
	
		assert [a:1] == ['a':1]
	
    }
    
    @Test
    public void test_VariableKey() {
		println "test_VariableKey - " * 5
		def x = 'a' 
	    assert ['x':1] == [x:1] 
	    assert ['a':1] == [(x):1] 	
		
		println x
		println (x)
    }
    
    @Test
    public void test_GetValues() {
		println "test_GetValues - " * 5
		
		def myMap = [a:1, b:2, c:3]
		
		//retriving values...
		assert myMap['a']       == 1   
		assert myMap.a          == 1   
		assert myMap.get('a')   == 1   
		assert myMap.get('a',0) == 1
		
		//missing values...
		assert myMap['d']       == null   
		assert myMap.d          == null   
		assert myMap.get('d')   == null
		
		//supply default value...
		assert myMap.get('d',0) == 0   
		assert myMap.d          == 0
	
		//set values...
		myMap['d'] = 1        
		assert myMap.d == 1   
		myMap.d = 2           
		assert myMap.d == 2	
		
	
    }

    @Test
    public void test_Query() {
		println "test_Query - " * 5
		def myMap = [a:1, b:2, c:3] 
		def other = [b:2, c:3, a:1]
		
		assert myMap == other   
		assert myMap.isEmpty()  == false                       
		assert myMap.size()     == 3                           
		assert myMap.containsKey('a')                          
		assert myMap.containsValue(1)
		
		assert myMap.keySet()        == toSet(['a','b','c'])
		
		assert toSet(myMap.values()) == toSet([1,2,3])
		
		assert myMap.entrySet() instanceof Collection          
		
		//closure...
		assert myMap.any   {entry -> entry.value > 2  }   
		assert myMap.every {entry -> entry.key   < 'd'}
		
	
    }

    //utility...
    def toSet(list){                  
        new java.util.HashSet(list)   
    }   
    
    
    @Test
    public void test_Iteration() {
		println "test_Iteration - " * 5
		
		def myMap = [a:1, b:2, c:3] 
		def store = ''
		
		//each, entry...
		myMap.each {entry ->       
		    store += entry.key     
		    store += entry.value   
		}                          
		assert store.contains('a1')       
		assert store.contains('b2') 
		assert store.contains('c3')
		println "Store: ${store}"
		
		
	    //each, key and value		
		store = '' 
		myMap.each {key, value ->   
		    store += key            
		    store += value          
		}                           
		assert store.contains('a1') 
		assert store.contains('b2') 
		assert store.contains('c3')
		println "Store(2): ${store}"
		
		
		//for keySet()
		store = '' 
		for (key in myMap.keySet()) {   
		    store += key                
		}                               
		assert store.contains('a') 
		assert store.contains('b') 
		assert store.contains('c')
		println "Store (keySet): ${store}"
		
		
		//for values()
		store = '' 
		for (value in myMap.values()) {   
		    store += value                
		}                                 
		assert store.contains('1') 
		assert store.contains('2') 
		assert store.contains('3')
		println "Store (values): ${store}"
	
    }

    @Test
    public void test_ChangeContent() {
		println "test_CahngeContent - " * 5
		
		//clear...
		def myMap = [a:1, b:2, c:3] 
		myMap.clear() 
		assert myMap.isEmpty()
		
		//remove by key....
		myMap = [a:1, b:2, c:3]
		println myMap
		myMap.remove('a') 
		assert myMap.size() == 2
		println myMap
		
		//subgroup....
		myMap = [a:1, b:2, c:3]
		println myMap
		def abMap = myMap.subMap(['a','b'])   
		assert abMap.size() == 2
		println abMap
		
		//findAll < 3 .... all elements (true) in the closure...
		abMap = myMap.findAll   { entry -> entry.value < 3} 
		assert abMap.size() == 2
		assert abMap.a      == 1
		println "findAll <3: ${abMap}"
		
		//find < 2 .... only the first element (true) in the closure...
		def found = myMap.find  { entry -> entry.value < 3} 
		assert found.key   == 'a' 
		assert found.value == 1
		println "find < 2 ${found}"
		
		//collect....
		def doubled = myMap.collect { entry -> entry.value *= 2} 
		assert doubled instanceof List 
		assert doubled.every    {item -> item %2 == 0} 
		println "Doubled: ${doubled}"
	
    }

    @Test
    public void test_CountWord() {
	
		println "test_CountWord - " * 5
	
		def textCorpus = 
	    """ 
	    Look for the bare necessities 
	    The simple bare necessities 
	    Forget about your worries and your strife 
	    I mean the bare necessities 
	    Old Mother Nature's recipes 
	    That bring the bare necessities of life 
	    """
	    
	    //tokenize....
	    def words = textCorpus.tokenize()
	    println "Word: ${words}"
	    
	    def wordFrequency = [:]
		//each, token... calculate the frequency of each word...
	    words.each { word -> 
	        wordFrequency[word] = wordFrequency.get(word,0) + 1   
	    } 
		println "WordFrequency: ${wordFrequency}"
	    
		//order in keys....
	    def wordList = wordFrequency.keySet().toList()
	    println "WordList: ${wordList}"
	    wordList.sort { wordFrequency[it] }
	    println "WordList: ${wordList}"	    
	    
	    def statistic = "\n" 
	    wordList[-1..-6].each { word -> 
	        statistic += word.padLeft(12)    + ': ' 
	        statistic += wordFrequency[word] + "\n" 
	    } 
	    println "Statistic: ${statistic}"
	
    }
}
