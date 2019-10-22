USING:  kernel math io locals sequences accessors memoize typed prettyprint ;
IN: syn-support

! Comments are started with the symbol "!", but only if it is the first char
! of the line, or when its previous char is a space.
! No comments in this line: "
SYNTAX: D: parse-decimal suffix! ;
dddd ! dddddd

! Examples of definitions with keywords:
! ":" "::" "MEMO:" "MEMO::" "TYPED:" "TYPED::"

: colon-word ( str -- )
    print ;

:: double_colon-word ( str -- )
    str print ;

MEMO: memo_colon-example ( str -- n )
    length ;

MEMO:: memo_double_colon-example ( str -- n )
    str length ;

TYPED: typed_colon-example ( a: integer b: integer -- sum: integer )
    + ;

TYPED:: typed_double_colon-example ( a: integer b: integer -- sum )
    a b + ;

! Examples of global variables:
SYMBOL: g_1
SYMBOLS: g_2 g_3 g_4 ;

! At this point, tree should be:
!
! ¦- g_1
! ¦- g_2
! ¦- g_3
! ¦- g_4
! ¦- : colon-word ( str -- )
! ¦- :: double_colon-word ( str -- )
! ¦- MEMO: memo_colon-example ( str -- n )
! ¦- MEMO:: memo_double_colon-example ( str -- n )
! ¦- typed_colon-example ( a: integer b: integer -- sum: integer )
! ¦- typed_double_colon-example ( a: integer b: integer -- sum )

! Note: "TYPED" can be omitted inside tree, since can be deducted by typed parameters



! Hierarchy system


! Class definition: Base is the class, b1 and b2 are member VARIABLE
TUPLE: Base b1 b2 ;

! Derived class inherits from Base and has d1 and d2 variables
TUPLE: Derived < Base d1 d2 ;

! Define the default constructor <Base> for class Base
C: <Base> Base

! Signature for class member method declarations
GENERIC: f1 ( n obj -- )
GENERIC: meth1 ( obj -- )

! f1 is a member method of Base
M: Base f1
    b1>> + dup odd? [ my-err ] [ . ] if ;

! Declare singleton classes s1 sm1 sm2 sm3
SINGLETON: s1
SINGLETONS: sm1 sm2 sm3 ;

! Define method meth1 for singleton class s1
! Note "M::" instead of "M:", for tree parsing is the same
M:: s1 meth1 ( obj -- )
    obj drop "Hello" print ;

! main, just for testing, ignore this
3 0 1 <Base> f1
s1 meth1

dd
! Tree for hierarchy system:
!
! ¦- Base
! ¦---- b1
! ¦---- b2
! ¦---- <Base>
! ¦---- f1 ( n obj -- )
! ¦- Derived
! ¦---- d1
! ¦---- d2
! ¦- s1
! ¦---- meth1 ( obj -- )
! ¦- sm1
! ¦- sm2
! ¦- sm3

! Note: to simplify tree parsing may be enough to color class var just to
! distinguish them from methods, instead of declaration for member methods.

! Complete tree of this file should be:
!
! ¦- g_1
! ¦- g_2
! ¦- g_3
! ¦- g_4
! ¦- : colon-word ( str -- )
! ¦- :: double_colon-word ( str -- )
! ¦- MEMO: memo_colon-example ( str -- n )
! ¦- MEMO:: memo_double_colon-example ( str -- n )
! ¦- typed_colon-example ( a: integer b: integer -- sum: integer )
! ¦- typed_double_colon-example ( a: integer b: integer -- sum )
! ¦- Base
! ¦---- b1
! ¦---- b2
! ¦---- <Base>
! ¦---- f1 ( n obj -- )
! ¦- Derived
! ¦---- d1
! ¦---- d2
! ¦- s1
! ¦---- meth1 ( obj -- )
! ¦- sm1
! ¦- sm2
! ¦- sm3

! Note: any ordering of class/global/var/words :-)
