\ Shapes -- OO in Forth

require ooforth.fs

\ class Shape =======================================================

\ Declare the base class for shapes.

class Shape
    ivar _x                  \ X position of shape
    ivar _y                  \ Y position of shape
    method MoveTo ( x y )    \ Move to new x,y position
    method RMoveTo ( dx dy ) \ Move relative
    method Draw ( )          \ Draw the shape
endclass

: Shape::MoveTo ( x y obj -- )
    swap over _y !   _x !
; implements MoveTo

: Shape::RMoveTo ( dx dy obj -- )
    swap over _y +!   _x +!
; implements RMoveTo


\ class Rectangle ====================================================

\ Rectangle inherits from Shape, using the inherits clause.  It adds
\ _width and _height instance variables and new methods for setting
\ these values.  A definition of Draw is provided, making Rectangle a
\ concrete class.

class Rectangle
    inherits Shape
    ivar _width
    ivar _height
    method SetWidth ( w obj )
    method SetHeight ( h obj )
endclass

: Rectange::Draw ( obj )
    ." Drawing a Rectangle at (" dup _x @ 0 .r
    ." ," dup _y @ 0 .r
    ." ), width " dup _width @ 0 .r
    ." , height " _height @ 0 .r
    cr
; implements Draw

: Rectangle::SetWidth ( w obj )
    _width !
; implements SetWidth

: Rectangle::SetHeight ( h obj )
    _height !
; implements SetHeight


\ class Circle =======================================================

\ Circle, similar to Rectangle.

class Circle
    inherits Shape
    ivar _radius
    method SetRadius ( r obj )
endclass

: Circle::Draw ( obj )
    ." Drawing a Circle at (" dup _x @ 0 .r
    ." ," dup _y @ 0 .r
    ." ), radius " _radius @ 0 .r
    cr
; implements Draw

: Circle::SetRadiu ( r obj )
    _radius !
; implements SetRadius



\ Main program =======================================================

\ create two shape objects and initialize their fields

Rectangle sh0    10 20 sh0 MoveTo   5 sh0 SetWidth   6 sh0 SetHeight
Circle    sh1    15 25 sh1 MoveTo   8 sh1 SetRadius

\ store the shapes in a simple array

create shapes  2 cells allot
sh0 shapes !   sh1 shapes cell+ !

\ create one more stand alone rectangle

Rectangle r     0 0 r MoveTo   15 r SetWidth   15 r SetHeight

\ DoSomethingWithShape is a function that expects a shape object on
\ the stack.

: DoSomethingWithShape ( shape )
    dup draw
    dup 100 100 rot RMoveTo
    draw
;

\ TryShape is the main program.

: TryShape

    \ initialize the shapes

    10 20 sh0 MoveTo   5 sh0 SetWidth   6 sh0 SetHeight
    15 25 sh1 MoveTo   8 sh1 SetRadius
    0 0     r MoveTo   15 r SetWidth   15 r SetHeight

    \ write the output

    cr
    2 0 do shapes i cells + @ DoSomethingWithShape loop

    30 r SetWidth
    r Draw
;
