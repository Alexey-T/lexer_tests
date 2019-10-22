package com.mg.groovy.samples.know

import org.junit.Test


/**
 * ListSample.groovy
 *
 * @author Marcio Garcia - marcio@mangar.com.br
 */
public class ListSample{
	
    
    @Test
    public void test_Creating() {
	
		//define a fill list (1, 2, 3 elements)
		def myList = [1,2,3] 
		assert myList.size() == 3 
		assert myList[0] == 1 
		assert myList instanceof ArrayList
	
		//define an empty list...
		def emptyList = [] 
		assert emptyList.size() == 0 
		
		//define a list from a range....  Range.toList()
		def longList = (0..1000).toList() 
		assert longList[555] == 555
		assert longList.size() == 1001
		
		//explicit define...
		def explicitList = new ArrayList()
		//add an existing array to the new....
		explicitList.addAll(myList)
		assert explicitList.size() == 3
		
		//change the value from the first element (zero)
		println explicitList
		explicitList[0] = 10 
		assert explicitList[0] == 10
	    println explicitList
	        
	    
		explicitList = new LinkedList(myList)   
		assert explicitList.size() == 3
		
		println explicitList
		explicitList[0] = 10 
		assert explicitList[0] == 10 
	    println explicitList
	
    }
    
    @Test
    public void test_AddRemove(){
	
		def myList = ['a','b','c','d','e','f']   

		//part of the list...
		assert myList[0..2]  == ['a','b','c']
		
		//specific positions...
		assert myList[0,2,4] == ['a','c','e']   

		//replace range....
		myList[0..2] = ['x','y','z']   
		assert myList == ['x','y','z','d','e','f']  
		                    
		//clean positions...
		myList[3..5] = []                          
		assert myList == ['x','y','z']              
		                    
		//add range into a position....
		myList[1..1] = ['y','1','2']   
		assert myList == ['x','y','1','2','z']
	
    }
    
    @Test
    public void test_ListSample() {
	
		def myList = []
		
		//first element...
		myList += 'a'   		
		assert myList == ['a']
		
		//adding...
		myList += ['b','c']   
		assert myList == ['a','b','c']
		
		//reset...
		myList = [] 
		//add a, add b
		myList <<  'a' << 'b'   
		assert myList == ['a','b']
		
		//remove 'b'
		assert myList - ['b'] == ['a']
		
		//multiply the array...
		assert myList * 2 == ['a','b','a','b']
	
    }
	
    @Test
    public void test_Remove() {
	
		def list = ['a','b','c']
		
		//by position...
        list.remove(2)    
        assert list == ['a','b']
		
		//by value....
        list.remove('b')    
    
        list = ['a','b','b','c']
		
		//by range...
        list.removeAll(['b','c'])                
        assert list == ['a']   	
	
	
    }
    
    @Test
    public void test_Methods() {

		//flatten...
		assert [1,[2,3]].flatten() == [1,2,3]       
	        
		//intersect...
		assert [1,2,3].intersect([4,3,1])== [3,1]
		
		//disjoint....
		assert [1,2,3].disjoint([4,5,6])            
		                                              
		def list = [1,2,3]                              
		def popped = list.pop()   
		assert popped == 3                          
		assert list == [1,2]                        
		                   
		//reverse....
		assert [1,2].reverse() == [2,1]             
		             
		//sort....
		assert [3,1,2].sort() == [1,2,3]            
		                                                                 
		list = [ [1,0], [0,1,2] ]      
		list = list.sort { a,b -> a[0] <=> b[0] }   
		assert list == [ [0,1,2], [1,0] ]
		
		list = list.sort { item -> item.size() }   
		assert list == [ [1,0], [0,1,2] ] 
		                                              
    }
       
    @Test
    public void test_FindAllCollect() {
        //collect returns all element inside the closure...                                 
        def doubled = [1,2,3].collect{ item ->   
            item*2                                    
        }                                             
        assert doubled == [2,4,6]                      
                                                      
        //... return only the elements true inside the closure.
        def odd = [1,2,3].findAll{ item ->   
            item % 2 == 1 
        } 
        assert odd == [1,3]	
    }
    
    @Test
    public void test_RemoveDuplicate() {
	
		println "test_RemoveDuplicate - "*5
		def x = [1,1,1] 
		println x
		
		//way 1
		assert [1] == new HashSet(x).toList()
		println new HashSet(x).toList()
		
		//way 2 (best)
		assert [1] == x.unique()
		println x.unique()
    }  
    
    @Test
    public void test_RemoveNull() {
	
		println "test_RemoveNull - "*5
	
		def x = [1,null,1] 
		println x
		
		assert [1,1] == x.findAll{it != null}
		println x.findAll{it != null}
		
		assert [1,1] == x.grep{it}
		println x.grep{it}
    }

    @Test
    public void test_Query() {
		println "test_Query - "*5
		
		def list = [1,2,3] 
		assert list.count(2) == 1               
		assert list.max() == 3                  
		assert list.min() == 1                  
		              
		//only who closure returns true...
		def even = list.find { item ->          
		    item % 2 == 0                       
		}                                       
		assert even == 2                        
		                 
		//everyone who closure returns true...
		assert list.every { item -> item < 5} 
		
		//anyone who clojure returns true
		assert list.any   { item -> item < 2} 
    }
    
    @Test
    public void test_Iteration() {
		println "test_Iteration - " * 5
		
		def store = '' 
		def list = [1, 2, 3]
		println list
		
		//all elements...
	    list.each { item ->         
	        store += item           
	    }                           
	    assert store == '123'  
	    println store
	                                
	    store = ''
	    //all elements inversed.....
	    list.reverseEach{ item ->   
	        store += item           
	    }                           
	    assert store == '321'
	    println store

    }

    @Test
    public void test_Acumulation() {
		println "test_Acumulation - " * 5
		
		def list = [1,2,3]
		
		assert list.join('-') == '1-2-3'
		println list.join('-')
            
	    def result = list.inject(0){ clinks, guests ->   
	        clinks += guests                         
	    }                                            
	    assert result == 0 + 1+2+3
	    
	    assert list.sum() == 6                       
	                                                 
	    def factorial = list.inject(1){ fac, item ->     
	        fac *= item                              
	    }                                            
	    assert factorial == 1 * 1*2*3	
    }
    
    
    
}
