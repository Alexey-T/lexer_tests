// prime number generator
for (i <- 2 to 1000)
  if((2 to i).find( j=> (i % j == 0 && i != j) ) == None)
    println(i)

// function currying example

def matcher(haystack: List[Char])(needle: String) = {                                                                     
      haystack contains needle.charAt(0)                                                                                       
     }  

def isInitialUpperCase = matcher(List('A','B','C','D','E','F','G','H','I','L','M','N','O','P','Q','R','S','T','U','V','Z')) _
isInitialUpperCase("Fe")

val upperCased = {                
      if (isInitialUpperCase("fe")) "fe"
      else "Fe"                        
     }

List("Federico","federico","Andrea").filter(isInitialUpperCase)

// immutability

val x = 2
var mutableX = 3
x = 3; // no!
mutableX = 4; // yes

val m = Map("1" -> "one")
m("1") = "ONE"; // it does work, but it's not assigning, it's returning a new Map

import scala.collection.mutable.Map
val m = Map("1" -> "one") // this time does not use default immutable Map, but mutable Map
m("1") = "ONE"; // updates inline

// pattern matching examples

x match {
 Case 2 => println(“two”)
 Case a: Int => println(“an integer”)
 Case _ => throw new Exception(“error”)
}

abstract class Fruit
case object Apple extends Fruit      
case object Pear extends Fruit       

def act(X: Fruit) = X match {        
  case Pear => "i like pears"         
  case Apple => "i do not like apples"
  case _ => "fruit not recognized"    
 }                                    

act(Pear) // java.lang.String = i like pears
act(Apple) // java.lang.String = i do not like apples

// trait example

trait Dad {
	def getRole = " my role is Dad "
}

trait Lazyness {
	def getTemperament = " i am a lazy person "
}

class Human(name: String) {
	def getName = "my name is " + this.name
}
	
class Man(name: String) extends Human(name) with Dad

val man = new Man("Jon") with Lazyness

println(List(man.getName, man.getRole, man.getTemperament).reduceLeft(_ + ", " + _))

// trait example 2

abstract class Human2(name: String) {
	def getName = "my name is " + this.name
}

trait Dad2 extends Human2 {
	abstract override def getName = "i am a dad, and " + super.getName
}

class Man2(name: String) extends Human2(name) with Dad2

val man2 = new Man2("John smith")

println(man2.getName)

// various - bbc news reader

import java.net._
val url = new URL("http://feeds.bbci.co.uk/news/rss.xml")
val xmlPayload = XML.load(url)
(xmlPayload \\ "item").foreach( item => println( (item \ "title").text) )


// actor example
val a = actor { 
 loop {
  receive {
   case x: Int => println(x)
   case _ => ()             
   }
  }
 }

a ! 2
a ! "no print"