
var a: array[0..1, char]
let i = 5
try:
  a[i] = 'N'
except IndexDefect:
  echo "invalid index"
0B0_10001110100_0000101001000111101011101111111011000101001101001001'f64
var i = 10'u8 + 5'u8
1_2.3_4
1.2 1e-3_4

=     +     -     *     /     <     >
@     $     ~     &     %     |
!     ?     ^     .     :     \
`   (    )     {    }     [    ]    ,  ;   [.    .]  {.   .}  (.  .)  [:

proc `^/`(x, y: float): float =
  # a right-associative division operator
  result = x / y
echo 12 ^/ 4 ^/ 8 # 24.0 (4 / 8 = 0.5, then 12 / 0.5 = 24.0)
echo 12  / 4  / 8 # 0.375 (12 / 4 = 3.0, then 3 / 8 = 0.375)

var a: array[0..1, char]
let i = 5
try:
  a[i] = 'N'
except IndexDefect:
  echo "invalid index"         

i = 0     # This is a single comment over multiple lines.
# The scanner merges these two pieces.
# The comment continues here.
2 
#[Comment here.
Multiple lines
are not a problem.]#

2
#[  #[ Multiline comment in already
   commented out code. ]#
proc p[T](x: T) = discard
]#
proc foo =
  ##[Long documentation comment
  here.
  ]##
            

type Type = object
  `int`: int

let `object` = Type(`int`: 9)
assert `object` is Type
assert `object`.`int` == 9

var `var` = 42
let `let` = 8
assert `var` + `let` == 50

const `assert` = true
assert `assert`
  
""""long string within quotes""""
var f = openFile(r"C:\texts\text.txt") # a raw string, so ``\t`` is no tab
