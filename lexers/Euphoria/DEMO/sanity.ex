		------------------------------------------
		-- AUTOMATIC SELF-CHECKING SANITY TEST  --
		-- for Euphoria                         --
		-- A quick test of most of the features --
		------------------------------------------
-- Usage:
--        ex sanity
--       exw sanity.ex
--       exu sanity.ex

with type_check

include get.e
include graphics.e  -- comment after include is ok
include sort.e
include machine.e
include file.e
include wildcard.e as wild
include image.e
include misc.e
include dll.e
include msgbox.e

integer SLASH
if platform() = LINUX then
    SLASH = '/'
else
    SLASH = '\\'
end if

constant msg = 1 -- place to send messages
atom t
t = time()
object y

procedure the_end()
    puts(msg, "Press Enter to continue\n")
    if atom(gets(0)) then
    end if
    if graphics_mode(-1) then
    end if
    abort(0)
end procedure

procedure make_sound()
-- test sound() built-in

    for i = 400 to 4000 by 400 do
	sound(i)
	for j = 1 to 100000 do
	end for
	sound(0)
    end for
end procedure

without warning
procedure abort()
-- force abort with trace back
    puts(msg, "\ndivide by 0 to get trace back...Press Enter\n")
    if sequence(gets(0)) then
    end if
    ? 1/0
end procedure
with warning

constant epsilon = 1e-10 -- allow for small floating point inaccuracy

procedure show(object x, object y)
-- show the mismatched values
    puts(msg, "\n   ---MISMATCH--- \n   x is ")
    ? x
    puts(msg, "   y is ")
    ? y
    puts(msg, " y - x is ")
    ? y - x
    abort()
end procedure

procedure same(object x, object y)
-- object x must be identical to object y else abort program
    atom ratio

    if atom(x) and atom(y) then
	if x = y then
	    return
	else
	    if y = 0 then
		show(x, y)
	    else
		ratio = x / y
		if ratio < 1 - epsilon or ratio > 1 + epsilon then
		    show(x, y)
		end if
	    end if
	end if
    elsif length(x) = length(y) then
	for i = 1 to length(x) do
	    same(x[i], y[i])
	end for
    else
	show(x, y)
    end if
end procedure

integer abs_id

function built_in()
-- built-in tests
    sequence d

    d = date()
    if d[1] < 93 or d[2] > 12 or d[3] < 1 or d[4] > 23 or d[5] > 59 or
	d[6] >59 or d[7] > 7  or d[8] > 366 then
	abort()
    end if
    d = power({-5, -4.5, -1,  0, 1,  2,  3.5, 4, 6},
	      { 3,    2, -1,0.5, 0, 29, -2.5, 5, 8})
    if d[1] != -125 or d[2] != 20.25 or d[3] != -1 or d[4] != 0 then
	abort()
    end if 
    if d[5] != 1 or d[6] != 536870912 or d[7] <.043 or d[7] > .044 then
	abort()
    end if
    if d[8] != 1024 or d[9] != 1679616 or power(2,3) != 8 then
	abort()
    end if
    same(power(16, 0.5), 4)
    d = remainder({5, 9, 15, -27}, {3, 4, 5, 6})
    if d[1] != 2 or d[2] != 1 or d[3] != 0 or d[4] != -3 then
	abort()
    end if
    d = remainder({11.5, -8.8, 3.5, 5.0}, {2, 3.5, -1.5, -100.0})
    if d[1] != 1.5 or d[2] < -1.81 or d[2] > -1.79 or d[3] != 0.5 or d[4] != 5 then
	abort()
    end if
    same(4, sqrt(16))
    same(3, length("ABC"))
    same({1, 1, 1, 1}, repeat(1, 4))
    if rand(10) > 10 or rand(20) < 1 or not find(rand(5.5), {1,2,3,4,5}) then
	abort()
    end if
    if equal(0,1) or 
       not equal(-9,-9) or 
       equal(5,{{{}}}) or 
       not equal(5,5.0) then
	abort()
    end if
    set_rand(5555)
    d = rand(repeat(10,20))
    set_rand(5555)
    if not equal(d, rand(repeat(10,20))) then
	abort()
    end if
    if time() < 0 then
	abort()
    end if
    if call_func(abs_id, {sin(3.1415)}) > 0.02 then
	abort()
    end if
    if cos(0) < .98 then
	abort()
    end if
    if call_func(abs_id, {tan(3.14/4) - 1}) > .02 then
	abort()
    end if
    if (call_func(abs_id, {tan(arctan(1)) - 1}) > 000.02) then
	abort()
    end if
    if log(2.7) < 0.8 or log(2.7) > 1.2 then
	abort()
    end if
    if floor(-3.3) != -4 then
	abort()
    end if
    if floor(-999/3.000000001) != -333 then
	abort()
    end if
    if floor(9.99/1) != 9 then
	abort()
    end if
    for i = -9 to 2 do
	if i = 1 then
	    return i
	end if
    end for
end function

function abs(atom x)
-- absolute value
    if x < 0 then
	return -x
    else
	return x
    end if
end function

abs_id = routine_id("abs")
if abs_id = -1 then
    abort()
end if

procedure sub()
    y = 200
end procedure

procedure overflow()
-- test overflows from integer into floating point
    object two29, two30, maxint, prev_i
    integer two30i, mtwo30i
    sequence s

    two30 = 1
    for i = 1 to 30 do
	two30 *= 2
    end for
    s = {two30, two30+1, two30+2}
    s += s
    if compare(s, {two30*2, two30*2+2, two30*2+4}) then
	abort()
    end if
    mtwo30i = -1
    for i = 1 to 29 do
	mtwo30i *= 2
    end for
    two30i = 1
    for i = 1 to 29 do
	two30i *= 2
    end for
    if 2 * two30i != -2 * mtwo30i then
	abort()
    end if
    if two30i*2 != two30 then
	abort()
    end if
    two29 = floor(two30 / 2)
    if two29 + two29 != two30 then
       abort()
    end if

    maxint = floor(two30 - 1)
    if maxint + 1 != two30 then
	abort()
    end if

    if 2 + maxint != two30 + 1 then
	abort()
    end if

    if (-maxint - 1) * -1 != two30 then
	abort()
    end if

    prev_i = -maxint + 1
    for i = -maxint to -maxint -5 by -1 do
	if i != prev_i - 1 then
	    abort()
	end if
	prev_i = i
    end for

    prev_i = maxint - 5
    for i = maxint - 3 to maxint + 3 by 2 do
	if i != prev_i + 2 then
	    abort()
	end if
	prev_i = i
    end for

    if floor(two30) != two30 then
	abort()
    end if

    if floor(two30 + two30 - 1) != two30 * 2 - 1 then
	abort()
    end if
end procedure

type natural(integer x)
    return x >= 0
end type

procedure atomic_ops()
-- test operations on atoms
    object a, x, z
    integer n, m
    natural p

    if arcsin(.5) <.523 or arcsin(.5) >.524 then
	abort()
    end if
    p = 0
    p = 0.000
    p = 4.0/2.0
    if p != 2.0 then
	abort()
    end if    
    n = 1
    m = 1
    if n and m then
    else
	abort()  
    end if
    if atom(n) or 1/0 then
	-- short-circuit skips the divide
    end if
    if sequence(n) and n[1] then
	-- short-circuit skips the subscript
    end if
    if 1 xor 1 then
	abort()
    end if
    if not ((0 xor 1) and (1 xor 0))then
	abort()
    end if
    if 0 xor 0 then
	abort()
    end if
    x = 100
    sub() -- y = 200
    z = 300

    if x + y != z then
	abort()
    end if

    if x != 100 then
	abort()
    end if

    if 3 * 3 != 9 or
       3 * 900000000 != 2700000000 or
       15000 * 32000 != 480000000 or
       32000 * 15000 != 480000000 or
       1000 * 13000 != 13000000 or
       13000 * 1000 != 13000000 then
	abort()
    end if
    while x != 100 do
	abort()
    end while

    if not (z - y = 100) then
	abort()
    end if

    if #FFFFFFFF != 4294967295 then
	abort()
    end if
   
    p = 20
    while not (p < 10) do
	p -= 2       
    end while
    if p != 8 then
	abort()
    end if

    if x * 1000.5 != 100050 or x * y != 20000 or x / y != 0.5 then
	abort()
    end if

    if y < x then
	abort()
    end if

    if y <= x then
	abort()
    end if

    if x > y then
	abort()
    end if

    if x >= y then
	abort()
    end if

    if -x != -100 then
	abort()
    end if

    if x = x and y > z then
	abort()
    end if

    x = 0

    y = {"ten", "one", "two", "three", "four", "five", "six", "seven", "eight",
	 "nine", "ten", "ten"}

    while x <= 11 do
	if x = 1 then a = "one"
	elsif x = 2 then a = "two"
	elsif x = 3 then a = "three"
	elsif x = 4 then a = "four"
	elsif x = 5 then a = "five"
	elsif x = 6 then a = "six"
	elsif x = 7 then a = "seven"
			 if 1 + 1 = 2 then
			     same(a, "seven")
			 elsif 1 + 1 = 3 then
			     abort()
			 else
			     abort()
			 end if
	elsif x = 8 then a = "eight"
	elsif x = 9 then a = "nine"
	else a = "ten"
	end if
	same(a, y[1+x])
	x = x + 1
    end while

    y = 0
    for xx = 100 to 0 by -2 do
	y = y + xx
    end for
    same(y, 50 * 51)

    for xx = 1 to 10 do
	if xx = 6 then
	    x = 6
	    exit
	end if
	y = 1
	while y < 25 do
	    y = y + 1
	    if y = 18 then
		exit
	    end if
	end while
	same(y, 18)
    end for
    y = repeat(-99, 7)
    for xx = +3 to -3 by -1 do
	y[xx+4] = xx
    end for
    same(y, {-3, -2, -1, 0, +1, +2, +3})
 
    
    y = {1,2,3}
    for xx = 1.5 to +3.0 by .5 do
      y[xx] = xx
    end for
    same(y, {1.5, 2.5, 3.0})

    y = {}
    for xx = -9.0 to -9.5 by -.25 do
      y = y & xx
    end for
    same(y, {-9.0, -9.25, -9.5})
    y = {}
    for i = 800000000 to 900000000 by 800000000 do
	y = append(y, i)        
    end for
    if compare(y, {800000000}) then
	abort()
    end if
    y = 5
    n = 3
    a = 2
    for i = 1 to y by a do
	n = n - 1
	y = 155
	a = 1
    end for
    same(n, 0)
end procedure

procedure floating_pt()
-- test floating-point operations
    sequence x
    atom final

    x = {1.5, -3.5, 1e10, -1e20, 0.0, 0.0001}
    y = repeat(x, 10)
    if x[1]/x[2] > -0.42 or x[1]/x[2] < -0.43 then
	abort()
    end if
    if find(1e10, x) != 3 then
	abort()
    end if
    for a = -1.0 to sqrt(999) by 2.5 do
	if a > 20.0 then
	    final = a
	    exit
	end if
    end for
    if final < 20.0 or final > 23 then
	abort()
    end if
end procedure

function one()
    return 1
end function

function two()
    return 2.000
end function

function sequence_ops()
-- test operations on sequences
    object i, w, x, y, z
    sequence s
    integer j

    x = {{1,2,3,4,5,6,7,8,9,10,11,12},{13,14,15,16,17,18,19,20,21,22,23,24}}
    y = {1,2,3,4,{5,6,7,8,{9,10,11,12}}}

    x[1][y[5][$][1]] = 1
    
    if x[1][9] != 1 then
	abort()
    end if
    
    x = {0, 1, 2, 3, 4, 5}
    y = {1, 0, 1, 1, 0, 0}
   
    if not equal(x xor y, y xor x) then
	abort()
    end if
    if compare({{1,1,0,0,1,1}}, {x} xor {y}) != 0 then
	abort()
    end if
    if compare({{1,1}, {2,3}} xor 
	       {{0,0}, {1,1}}, 
	       {{1,1}, {0,0}}) != 0 then
	abort()
    end if
    x = "Hello "
    y = "World"
 
    if find(0, x = x) then
	abort()
    end if

    if x[two()*two() - two()] != 'e' then
	abort()
    end if

    if x[one()+one()] != x[two()] then
	abort()
    end if
    j = x[1]
    if j != 'H' then
	abort()
    end if
    s = {3.0}
    s[1] = 1.0000
    j = s[1]
    if j != 1 then
	abort()
    end if
    i = 1
    if not atom(i) or not integer(i) then 
	abort()
    end if
    if length(y) != 5 then 
	abort()
    end if
    while i <= 5 do
	x = append(x, y[i])
	i = i + 1
    end while
 
    i = 1
    while i <= 3 do
	x = append(x, '.')
	x = append(x, '\'')
	i = i + 1
    end while
    same(x, "Hello World.'.'.'")
    x = {}
    x = append(x, {20,30,5})
    same(x, {{20,30,5}})
    x = repeat(5, 19)
    x = append(x, 20)
    x[7] = 9
    y = {9, 9, {9}}
    y = prepend(y, 8)
    y = prepend(y, {9, 9})
    same(y, {{9, 9}, 8, 9, 9, {9}})
    y = x
    z = y * x + x + 1000
    w = z > 1030 or x = 9
    
    same(z, {1030, 1030, 1030, 1030, 1030, 1030, 1090, 1030, 1030, 1030,
	     1030, 1030, 1030, 1030, 1030, 1030, 1030, 1030, 1030, 1420})
    same(w, {0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
	     0, 0, 0, 0, 0, 0, 0, 0, 0, 1})
    x = {"",""}
    x[1] &= "Euph"
    x[2] &= "oria"
    same(x, {"Euph", "oria"})
    x = {100, 200, {1, 2, {0, 0, 0}}, 300}
    x[3][3][3] = 26
    x[3][3][3] += 0+0
    x[3][3][3] -= 2
    x[3][3][2..3] += 1
    x[3][3][2..3] -= {0,0}
    x[3][3][3] *= 8
    x[3][3][3] /= 8
    x = x * x
    same(x, {10000, 40000, {1, 4, {0, 1, 625}}, 90000})
    y = x / {1, 2, 3, 4}
    same(y, {10000, 20000, {1/3, 4/3, {0, 1/3, 625/3}}, 22500})
    -- & tests
    same(2 & {5, 6,7}, {2, 5, 6, 7})
    same({} & 3, {3})
    same("ABC" & "DEF" & "GHIJ" & {}, "ABCDEFGHIJ")
    same('A' & 'B' & 'C', "ABC")
    -- slice tests
    x = "ABCDEFGHIJKLMNOP"
    same(x[1..4], "ABCD")
    y = x[2..5]
    same(y, "BCDE")
    same(x[4..3], {})
    same(x[4..4], "D")
    x[3..5] = "000"
    same(x, "AB000FGHIJKLMNOP")
    x[6..9] = '8'
    same(x, "AB0008888JKLMNOP")
    same(floor({1, 2, -3, 4, -5} / 3), {0, 0, -1, 1, -2})
    same(and_bits({#0FFFFFFF, #7000, 5}, 
		  {#F0F0F0F0, #4010, 3}),
		  {#00F0F0F0, #4000, 1})
    same(or_bits({#0FFFFFFF, #7000, 5}, 
		 {#00F0F0F0, #4010, 3}),
		 {#0FFFFFFF, #7010, 7})
    same(xor_bits({#0FFFFFFF, #7000, 5}, 
		  {#00F0F0F0, #4010, 3}),
		  {#0F0F0F0F, #3010, 6})
    same(and_bits(not_bits({#0FFFFFFF, #7000, #05}), #0000FFFF), 
		  {#00000000, #00008FFF, #0000FFFA})
    return y
end function


procedure sequence_ops2()
-- more tests of sequence operations
object x, y

    x = "ABCDEFGHIJKLMNOPD"
    if find('D', x) != 4 then
	abort()
    end if
    if find("ABC", {"", 0.0, 0, "ABC"}) != 4 then
	abort()
    end if
    if find(0.0, {"", 0.0, 0, "ABC"}) != 2 then
	abort()
    end if
    if find_from('D', x, 10.0) != length(x) then
	abort()
    end if
    if find_from('D', x, length(x)+1) != 0 then
	abort()
    end if
    if match("EFGH", x) != 5 then
	abort()
    end if
    if match({"AB", "CD"}, {0, 1, 3, {}, {"AB", "C"}, "AB", "CD", "EF"}) != 6 then
	abort()
    end if
    if match_from("EU", "EUEUXXXEUXXX", 7) != 8 then
	abort()
    end if
    
    if not equal(x,x) then
	abort()
    end if
    if compare({}, {}) != 0 then
	abort()
    end if
    y = repeat(repeat(repeat(99, 5), 5), 5)
    if y[3][3][3] != 99 then
	abort()
    end if
    if compare(y[4][4][3..5], repeat(99, 3)) != 0 then
	abort()
    end if
    y[3][2][1..4] = 88
    if compare(y[3][2], {88, 88, 88, 88, 99}) != 0 then
	abort()
    end if
end procedure

procedure circularity()
-- test for circular references in internal garbage collector
    object x, y, z
    
    z = {{1,2},{3,4}}
    z[1][2] = z

    same(z, {{1,{{1,2},{3,4}}}, {3,4}})
    
    x = {{"abc", {0, 0, 0}}, "def", 1, 2}
    x[3] = x
    x[1..2] = x[2..3]

    x = append(x, x)
    x = prepend(x, x)
    if compare(x, x) != 0 then
	abort()
    end if
    
    y = "ABCDE"
    y[2] = repeat(y, 3)
    if compare(y, y) != 0 then
	abort()
    end if
end procedure

procedure patterns()
-- test wildcard routines   
    if wildcard_file("ABC*DEF.*", "XBCDEF.E") then
	abort()
    end if
    if platform() != LINUX and not wildcard_file("A?B?C?D", "a1b2C3D") then
	abort()
    end if
    if wildcard_match("AAA", "AAa") then
	abort()
    end if
    if not wildcard_match("??Z*Z*", "ABZ123Z123") then
	abort()
    end if
end procedure

procedure conversions()
-- test conversion of values to/from string representation   
    sequence v
    
    v = sprintf("values are: %5d, %3d, %4.2f", {1234, -89, 6.22})
    if compare(v, "values are:  1234, -89, 6.22") != 0 then
	abort()
    end if
    v = value("{1,2,3}")
    if compare(v, {GET_SUCCESS, {1,2,3}}) != 0 then
	abort()
    end if
    for x = 1 to 100 by 3 do
	v = value(sprintf("%d", x)) 
	if compare(v, {GET_SUCCESS, x}) != 0 then
	    abort()
	end if
	v = value(sprintf("#%x ", x))
	if compare(v, {GET_SUCCESS, x}) != 0 then
	    abort()
	end if
    end for
end procedure

procedure output()
-- test file output routines
    integer file_no
    file_no = open("sanityio.tst", "w")
    if file_no < 0 then
	abort() 
    end if
    puts(file_no, "-- io test\n")
    print(file_no, {1,2,3})
    puts(file_no, '\n')
    print(file_no, -99)
    puts(file_no, " {11, {33, {#33}}, 4, 5 } {\t\t}\n")
    puts(file_no, "{} .999 -.999 1.55e00 {11,   22 , {33, 33}, 4, 5  }\n") 
    printf(file_no, "%e", 10000)
    printf(file_no, " %d", -123)
    printf(file_no, " %5.1f", 5+1/2)
    printf(file_no, "%50s\n", {"+99 1001 {1,2,3} 1E-4 {1.002e23,-59e-5,"})
    printf(file_no, "%9e}\t\t-1e-20\t   -.00001e5\n", 59e30)
    puts(file_no, "\"Rob\" \"ert\" \"Craig\"  ")
    puts(file_no, "\"\" \"\\n\" \"\\t\\r\"\t")
    puts(file_no, "\"\\'\\\"\" 'A' '\\n' '\\\"' '\\'' '\\r'\n")
    printf(file_no, "{#%x, ", 291)
    puts(file_no, "\"ABC\"} {'A', 'B', '\\n'}")  
    close(file_no)
end procedure

procedure input()
-- test file input routines
    integer file_no
    object line
    integer char

    file_no = open("sanityio.tst", "r")
    if file_no < 0 then
	abort()
    end if
    if seek(file_no, 5) then
	abort()
    end if
    if seek(file_no, -1) then
	abort()
    end if
    if seek(file_no, 0) then
	abort()
    end if
    if where(file_no) != 0 then
	abort()
    end if
    line = gets(file_no)
    if compare(line, "-- io test\n") != 0 then
	abort()
    end if
    char = getc(file_no)
    if char != '{' then
	abort()
    end if
    close(file_no)
end procedure

without type_check
integer color
color = 1
sequence v

procedure testgr()
-- test basic VGA graphics operations
    sequence x
    
    if v[VC_XPIXELS] < 100 or v[VC_YPIXELS] < 100 then
	abort()
    end if
    draw_line(BLUE, {{20, 100}, {600, 100}})

    for i = 1 to 200 by 5 do
	pixel(WHITE, {3*i, i})
	if get_pixel({3*i, i}) != 7 then
	    abort()
	end if
    end for

    polygon(color, 0, {{20,350}, {40, 250}, {80, 400}})
    ellipse(color+5, 1, {350, 350}, {440,440})
    color = color + 1
    x = {}
    for i = 0 to 63 do
	x = x & repeat(i, 2)
    end for
    for p = 220 to 320 by 4 do
	display_image({p,p}, repeat(x+color, 2))
    end for
end procedure
with type_check

constant TRUE = 1/1, FALSE = 0, junk=-TRUE
if junk != -1 then
    abort()
end if

procedure testget()
-- test input of Euphoria objects
    object gd
    object x, i
    object results

    gd = open("sanityio.tst", "r")
    if gd < 0 or gd > 10 then
	abort()
    end if
    if not sequence(gets(gd)) then
	abort()
    end if
    results = {
	 {0, {1,2,3}},
	 {0, -99},
	 {0, {11, {33, {#33}}, 4, 5}},
	 {0, {}},
	 {0, {}},
	 {0, 0.999},
	 {0, -0.999},
	 {0, 1.55},
	 {0, {11, 22, {33, 33}, 4, 5}},
	 {0, 10000},
	 {0, -123},
	 {0, 5.5},
	 {0, 99},
	 {0, 1001},
	 {0, {1, 2, 3}},
	 {0, 0.0001},
	 {0, {1.002e+23, -0.00059, 5.9e+31}},
	 {0, -1e-20},
	 {0, -1},
	 {0, "Rob"},
	 {0, "ert"},
	 {0, "Craig"},
	 {0, ""},
	 {0, "\n"},
	 {0, "\t\r"},
	 {0, "\'\""},
	 {0, 'A'},
	 {0, '\n'},
	 {0, '\"'},
	 {0, '\''},
	 {0, '\r'},
	 {0, {#123, "ABC"}},
	 {0, {'A', 'B', '\n'}},
	 {-#1, 0}
    }
    i = 1
    while TRUE do
	x = get(gd)
	if x[1] = -1 then
	    exit
	end if
	same(x, results[i])
	i = i + 1
    end while
    if compare(results[i], {-1, 0}) != 0 then
	puts(2, "wrong number of get values\n")
    end if
    close(gd)
end procedure

function fib(integer n)
-- fibonacci
    if n < 2 then
	return n
    else
	return fib(n-1) + fib(n-2)
    end if
end function

integer rp

procedure recursive_proc()
-- a recursively-called procedure
    if rp > 0 then
	rp = rp - 1
	recursive_proc()
    end if
end procedure

procedure machine_level()
-- quick test of machine-level routines
    atom addr, dest, src

    if 9.99 != float64_to_atom(atom_to_float64(9.99)) then
	abort()
    end if
    if -27 != float32_to_atom(atom_to_float32(-27)) then
	abort()
    end if
    
    addr = allocate(100)
    poke4(addr, #12345678)
    if peek4u(addr) != #12345678 then
	abort()
    end if
    if peek4s(addr) != #12345678 then
	abort()
    end if
    poke(addr, {77, -1, 5.1, -1.1})
    if compare(peek({addr, 4}), {77, 255, 5, 255}) then
	abort()
    end if
    poke(addr, #C3) -- RET instruction
    if peek(addr) != #C3 then
	abort()
    end if
    call(addr)
    free(addr)
    for x = 0 to +2000000 by 99999 do
	if bytes_to_int(int_to_bytes(x)) != x then
	    abort()
	end if
    end for
    if bits_to_int({1,0,1,0}) != 5 then
	abort()
    end if
    if not equal(int_to_bits(17,8), {1,0,0,0,1,0,0,0}) then
	abort()
    end if
    -- mem_copy() and mem_set()
    dest = allocate(20)
    src = allocate(20)
    poke(src, {1,2,3,4,5,6,7,8,9})
    mem_copy(dest, src, 9)
    if compare(peek({dest, 9}), peek({src,9})) != 0 then
	abort()
    end if
    mem_set(dest+3, 100, 4)
    if compare(peek({dest, 9}), {1,2,3,100,100,100,100,8,9}) != 0 then
	abort()
    end if
    free(dest)
    free(src)
end procedure

global type sorted(sequence x)
-- return TRUE if x is in ascending order
    for i = 1 to length(x)-1 do
	if compare(x[i], x[i+1]) > 0 then
	    return FALSE
	end if
    end for
    return TRUE
end type

procedure win32_tests()
-- tests that require WIN32 platform (exw sanity.ex)
    atom lib
    integer Sleep, Beep
    
    if instance() < 0 then
	abort()
    end if
    
    lib = open_dll("kernel32")
    if lib = 0 then
	abort()
    end if
    Beep = define_c_func(lib, "Beep", {C_INT, C_INT}, C_INT)
    if Beep = -1 then
	abort()
    end if
    Sleep = define_c_proc(lib, "Sleep", {C_INT})
    if Sleep = -1 then
	abort()
    end if
    for i = 1 to 3 do
	if c_func(Beep, {0, 0}) = 0 then
	    abort()
	end if
	t = time()
	c_proc(Sleep, {1000}) -- 1000 milliseconds
	if time() - t < .5 then
	    abort()
	end if
    end for
end procedure

integer last_sum

function checksum(sequence filename)
-- Return the simple sum of all the bytes in a file, 
-- or 0 if file not found
    integer fn, c
    
    fn = open(filename, "rb")
    last_sum = 0
    if fn != -1 then
	while TRUE do
	    c = getc(fn)
	    if c = -1 then
		exit
	    end if
	    last_sum += c
	end while
	close(fn)
    end if
    return last_sum
end function

constant SUM_EX = 19552919,
	 SUM_EXW = 8092818,
	 SUM_EXU = 9917472

object eudir

procedure corrupt(sequence filename)
-- say that a file is corrupt   
    sequence full_name

    full_name = lower(eudir & filename)
    puts(msg, "\n\n")
    if last_sum = 0 then
	puts(msg, full_name & " does not exist. \n")
	puts(msg, "The install is not correct. ")
    else
	printf(msg, "%s seems to be incorrect. \n", {full_name})
	if    last_sum = SUM_EX then
	    puts(msg, "It's the Complete Edition ex.exe file. ")
	elsif last_sum = SUM_EXW then
	    puts(msg, "It's the Complete Edition exw.exe file. ")
	elsif last_sum = SUM_EXU then
	    puts(msg, "It's the Complete Edition exu file. ")
	else                
	    printf(msg, "Its check-sum (%d) is wrong. \n", last_sum)
	    puts(msg, 
	    "Either it's an old version, or it has been corrupted. \n")
	    puts(msg, 
	    "You can download the latest version of Euphoria from: \n")
	    puts(msg, "    http://www.RapidEuphoria.com/ ")
	end if
    end if
    puts(msg, "\n\n\n")
    the_end()
end procedure

procedure reboot_msg()
    if platform() = LINUX then
	puts(msg, "Did you forget to edit your profile and log in again?\n")
    else    
	puts(msg, "Did you forget to reboot (restart) your machine?\n")
    end if
    puts(msg, "See INSTALL.DOC\n")
    the_end()
end procedure

procedure check_install(integer doit)
-- see if Euphoria was installed correctly
    object path
    sequence temp_eudir
    integer ex_sum, slash
    
    if not doit then
	-- puts(1, "skipping checksum - press Enter\n")
	-- if getc(0) then
	-- end if
	return
    end if
    
    path = getenv("PATH")
    if atom(path) then
	puts(msg, "Note: Your PATH variable has not been set.\n")
	reboot_msg()
    end if
    
    eudir = getenv("EUDIR")
    if atom(eudir) then
	puts(msg, "Note: Your EUDIR variable has not been set.\n")
	reboot_msg()
    end if
    
    if length(eudir) and eudir[$] = '\\' then
	eudir = eudir[1..$-1]
    end if
	
    temp_eudir = upper(eudir)
    slash = find(SLASH, temp_eudir)
    if slash then
	-- safer to ignore C:\ on WinDOS
	temp_eudir = temp_eudir[slash+1..$]
    end if
    
    if not match(temp_eudir & SLASH & "BIN", wild:upper(path)) then
	puts(msg, "Note: " & eudir & SLASH & "BIN is not on your PATH.\n")
	reboot_msg()
    end if

    -- check for corrupted or incorrect files:
    if platform() = LINUX then
	return 
--      eudir &= "/bin/"
--      ex_sum = checksum(eudir & "exu")
--      if ex_sum = SUM_EXU then
--          if checksum(eudir & "pdexu") != SUM_PDEXU then
--              corrupt("pdexu")
--          end if
--      elsif ex_sum = SUM_PDEXU then
--      else
--           corrupt("exu") -- could be FreeBSD
--      end if
    else    
	-- DOS & Windows
	eudir &= "\\BIN\\"
	ex_sum = checksum(eudir & "ex.exe")
	if ex_sum = SUM_EX then
	    if checksum(eudir & "exw.exe") != SUM_EXW then
		corrupt("exw.exe")
	    end if
	else
	    corrupt("ex.exe")
	end if   
    end if
end procedure

without profile
without warning
global procedure sanity()
-- main program
    sequence cmd_line, save_colors, s1, s2
    integer vga, ok

    check_install(FALSE) --TRUE)

    if platform() = WIN32 then
	vga = FALSE
	ok = message_box("Run the test?", "Euphoria WIN32 Sanity Test", 
	{MB_OKCANCEL, MB_SYSTEMMODAL})
	if ok = IDCANCEL then
	    return
	end if
    else
	-- some text mode tests
	clear_screen()
	position(1,1)
	puts(1, "ABC")
	position(2,1)
	puts(1, "def")
	
	s1 = save_text_image({1,1}, {2,3})
	display_text_image({10,20}, s1)
	s2 = save_text_image({10, 20}, {11, 22})
	if not equal(s1, s2) then
	    abort()
	end if

	-- graphics mode and other tests
	vga = not graphics_mode(18) 
	v = video_config()
	clear_screen()
	position(12, 20)
	if compare({12, 20}, get_position()) != 0 then
	    abort()
	end if
	puts(msg, "Euphoria SANITY TEST ... ")
    end if

    if platform() = WIN32 then
	win32_tests()
    end if

    for j = 0 to 20 by 2 do  
	cmd_line = command_line()
	    
	if length(cmd_line) < 1 or length(cmd_line) > 10 then
	    abort()
	end if
	if length(current_dir()) < 2 then
	    abort()
	end if
	if length(dir(".")) < 2 then
	    abort()
	end if
	if vga and platform() != LINUX then 
	    testgr()
	end if
	make_sound()
	same(built_in(), 1)
	atomic_ops()
	overflow()
	floating_pt()
	if compare(sequence_ops(), "BCDE") != 0 then
	    puts(msg, "sequence_ops failed\n")
	end if
	sequence_ops2()
	circularity()
	output()
	input()
	testget()
	conversions()
	patterns()
	machine_level()
	rp = 100
	recursive_proc()
	if rp != 0 then
	    puts(msg, "recursive proc failed\n")
	end if
	if fib(20) != 6765 then
	    puts(msg, "fib failed\n")
	end if
 
	if not sorted(sort(-500 + rand(repeat(1000, 1000)))) then
	    puts(msg, "standard sort failed\n")
	end if
	if not sorted(sort({"robert", "junko", "dave", "ken", "lurdes"})) then
	    puts(msg, "standard general sort failed\n")
	end if

    end for
    
    if platform() = LINUX then
	system("rm sanityio.tst", 2)
    else
	system("del sanityio.tst", 2)
    end if  
    save_colors = {}
    if platform() = DOS32 then
      for i = 0 to v[VC_NCOLORS]-1 do
	  save_colors = append(save_colors, palette(i, {0,0,0}))
      end for
      for i = 1 to 200 do
	  sound(i*15)
	  all_palette(rand(repeat({63,63,63}, v[VC_NCOLORS])))
      end for
      sound(0)
      all_palette(save_colors)
    end if
    if platform() = WIN32 then
	ok = message_box("PASSED (100%)", "Euphoria WIN32 Sanity Test", MB_OK)
    else
	puts(msg, "\nPASSED (100%)\n")
	the_end()    
    end if
end procedure

integer z

-- another for-loop test
z = 0
for j = 1 to 10 do
    z = z + j
end for
if z != 55 then
    abort()
end if

sanity()


