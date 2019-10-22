package com.mg.groovy.samples.closures

public class Visitor{
	
}

class Drawing { 
    List shapes 
    def accept(Closure yield) { shapes.each{it.accept(yield)} } 
} 
class Shape { 
    def accept(Closure yield) { yield(this) } 
} 
class Square extends Shape { 
    def width 
    def area() { width**2 } 
} 
class Circle extends Shape { 
    def radius 
    def area() { Math.PI * radius**2 } 
} 

