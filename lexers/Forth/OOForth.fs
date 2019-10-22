\ Example:
\    class Dog
\        ivar _age
\        method Speak
\    endclass
\
\    Dog fido
\
\    Defining a Method:
\        : Dog::Speak    ." Woof" ;   implements Speak
\
\ ====================================================================
\ Warn that a Pure Virtual Function has been called.

: pvf ( -- )
    1 abort" pure virtual called" ;
' pvf constant pvfc

\ Class Definition Structure

variable curclass       \ Points to latest class definition structure
: _nvar             ;   \ number of variables (including base classes)
: _nmeth    cell+   ;   \ number of methods (including base classes)
: _vptr   2 cells + ;   \ vptr
: _parent 3 cells + ;   \ address of class definition for base class

\ Start a class declaration

: class ( "name" -- )
    create here curclass !
    0 , 0 , 0 , 0 ,
does>
    curclass !
    create
    curclass @ _vptr @ ,
    curclass @ _nvar @ cells allot ;

\ Declares the base class. Used within the class declaration before
\ any instance variable or method declarations.

: inherits ( "name" -- )
    ' >body
    dup _nvar @    curclass @ _nvar !
    dup _nmeth @   curclass @ _nmeth !
    curclass @ _parent ! ;

\ Terminate the class declaration and create the VTable for the class.

: endclass ( -- )
    here   curclass @ _vptr  !

    \ initialize vtbl with pvf
    curclass @ _nmeth @
    0 ?do pvfc , loop

    \ copy parents vtable to here, if parent exists
    curclass @ _parent @
    if
	curclass @ _parent @ dup _vptr @ swap _nmeth @
	curclass @ _vptr @ swap
	cells cmove
    then
;

\ Declare an instance variable.  Used within a class declaration.

: ivar ( "name" -- )
    create
    curclass @ _nvar @ cells ,
    1   curclass @ _nvar   +!
    does> @ + cell+ ;

\ Declare a method.  Used within a class declaration.

: method ( "name" -- )
    create curclass @ _nmeth @ cells ,
    1   curclass @ _nmeth   +!
    does> @ over @ + @   execute ;

\ Declare that the immediately preceeding word is a class method
\ implementing the named method.

: implements ( "name" -- )
   ' >body @    curclass @ _vptr @ +   lastxt swap ! ;
