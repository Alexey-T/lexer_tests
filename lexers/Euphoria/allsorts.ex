		----------------------------------------
		-- Sorting Algorithms in Euphoria     --
		----------------------------------------
-- Notes:

-- The slower sorts will be omitted after a while. 

-- Bucket sort looks really good, but remember that the other sorts are 
-- generic, and can handle integers, floating-point numbers, strings, etc.
-- Bucket sort can only sort integers, and the integers need to be 
-- in a reasonably small, well-defined range, as is the case in this
-- benchmark. It also uses more memory than some of the other sorts.

without type_check

include machine.e
include misc.e

tick_rate(100) -- get better clock resolution on DOS32
set_rand(9999)

constant MAX = 50000
constant TRUE = 1, FALSE = 0

constant CHECK_RESULTS = FALSE -- TRUE for debugging new sorting algorithms

integer hybrid_limit

type natural(integer x)
    return x >= 0
end type

type file_number(integer x)
    return x >= -1
end type

function simple_sort(sequence x)
-- put x into ascending order
-- using a very simple sort
object temp

    for i = 1 to length(x) - 1 do
	for j = i + 1 to length(x) do
	    if compare(x[j],x[i]) < 0 then
		temp = x[j]
		x[j] = x[i]
		x[i] = temp
	    end if
	end for
    end for
    return x
end function

function bubble_sort(sequence x)
-- put x into ascending order
-- using bubble sort
object temp
natural flip, limit

    flip = length(x)
    while flip > 0 do
	limit = flip
	flip = 0
	for i = 1 to limit - 1 do
	    if compare(x[i+1], x[i]) < 0 then
		temp = x[i+1]
		x[i+1] = x[i]
		x[i] = temp
		flip = i
	    end if
	end for
    end while
    return x
end function

function insertion_sort(sequence x)
-- put x into ascending order
-- using insertion sort
    object temp
    natural final

    for i = 2 to length(x) do
	temp = x[i]
	final = 1
	for j = i-1 to 1 by -1 do
	    if compare(temp, x[j]) < 0 then
		x[j+1] = x[j]
	    else
		final = j + 1
		exit
	    end if
	end for
	x[final] = temp
    end for
    return x
end function

function shell_sort(sequence x)
-- Shell sort based on insertion sort

    integer gap, j, first, last
    object tempi, tempj

    last = length(x)
    gap = floor(last / 10) + 1
    while TRUE do
	first = gap + 1
	for i = first to last do
	    tempi = x[i]
	    j = i - gap
	    while TRUE do
		tempj = x[j]
		if compare(tempi, tempj) >= 0 then
		    j += gap
		    exit
		end if
		x[j+gap] = tempj
		if j <= gap then
		    exit
		end if
		j -= gap
	    end while
	    x[j] = tempi
	end for
	if gap = 1 then
	    return x
	else
	    gap = floor(gap / 3.5) + 1
	end if
    end while
end function


global function quick_sort(sequence x)
-- put x into ascending order
-- using recursive quick sort
    natural n, last, mid
    object temp, midval

    n = length(x)
    if n < 2 then
	return x    -- already sorted (trivial case)
    end if

    mid = floor((n + 1) / 2)
    midval = x[mid]
    x[mid] = x[1]

    last = 1
    for i = 2 to n do
	if compare(x[i], midval) < 0 then
	    last += 1
	    temp = x[last]  x[last] = x[i]  x[i] = temp
	end if
    end for

    return quick_sort(x[2..last]) & {midval} & quick_sort(x[last+1..n])
end function


global function hybrid_sort(sequence x)
-- put x into ascending order
-- using recursive quick sort
-- but call insertion sort for short sequences
    natural n, last, mid
    object midval, temp

    n = length(x)
    if n < hybrid_limit then
	return insertion_sort(x)
    end if

    mid = floor((n + 1) / 2)
    midval = x[mid]
    x[mid] = x[1]

    last = 1
    for i = 2 to n do
	if compare(x[i], midval) < 0 then
	    last += 1
	    temp = x[last]  x[last] = x[i]  x[i] = temp
	end if
    end for

    return hybrid_sort(x[2..last]) & {midval} & hybrid_sort(x[last+1..n])
end function

sequence x

procedure g_insertion_sort()
-- put global variable x into ascending order
-- using insertion sort of general objects
object temp
natural final

    for i = 2 to length(x) do
	temp = x[i]
	final = 1
	for j = i-1 to 1 by -1 do
	    if compare(temp, x[j]) < 0 then
		x[j+1] = x[j]
	    else
		final = j + 1
		exit
	    end if
	end for
	x[final] = temp
    end for
end procedure

procedure best_sort(natural m, natural n)
-- put x[m..n] into (roughly) ascending order
-- using recursive quick sort 
    natural last, mid
    object midval, temp

    if n - m < hybrid_limit then 
	return
    end if
    mid = floor((m + n) / 2)
    midval = x[mid]
    x[mid] = x[m]

    last = m
    for i = m+1 to n do
	if compare(x[i], midval) < 0 then
	    last += 1
	    temp = x[last]  x[last] = x[i]  x[i] = temp
	end if
    end for
    x[m] = x[last]
    x[last] = midval
    best_sort(m, last-1)
    best_sort(last+1, n)
end procedure

global function great_sort(sequence a)
-- Avoids dynamic storage allocation - just passes indexes into
-- a global sequence.
-- Not much better than hybrid_sort which makes full use of dynamic
-- storage allocation.
-- Note that we only partition down to a certain degree, then do an
-- insertion sort which runs fast because things are roughly in order.
-- See Knuth for the details.
    x = a
    best_sort(1, length(x))
    g_insertion_sort()
    return x
end function

global function merge_sort(sequence x)
-- put x into ascending order
-- using recursive merge sort
    natural n, mid
    sequence merged, a, b

    n = length(x)
    if n < 2 then
	return x
    end if

    mid = floor(n/2)
    a = merge_sort(x[1..mid])       -- sort the first half
    b = merge_sort(x[mid+1..n])     -- sort the second half
    
    -- merge the two sorted halves into one
    merged = {}
    while length(a) > 0 and length(b) > 0 do
	if compare(a[1], b[1]) < 0 then
	    merged = append(merged, a[1])
	    a = a[2..length(a)]
	else
	    merged = append(merged, b[1])
	    b = b[2..length(b)]
	end if
    end while
    return merged & a & b -- merged data plus leftovers
end function

integer min_value, max_value -- for bucket sort

function bucket_sort(sequence s)
-- Sort s into ascending order. No elements are compared.
-- The values of s must be integers from min_value to max_value.
    sequence count, sorted
    integer value, k, offset, c
    
    count = repeat(0, max_value-min_value+1)
    offset = min_value - 1
    -- count the number of occurrences of each integer value:
    for i = 1 to length(s) do
	value = s[i] - offset
	count[value] += 1
    end for
    sorted = repeat(0, length(s))
    k = 1
    -- make the resulting sorted sequence
    for i = 1 to length(count) do
	c = count[i]
	if c then
	    sorted[k..k+c-1] = i + offset
	    k += c
	end if  
    end for
    return sorted 
end function

procedure check_results(sequence sdata, sequence data)
-- compare results with another sort to make sure they are correct
    if CHECK_RESULTS then
	if not equal(sdata, shell_sort(data)) then
	    puts(2, "\nabort!\n")
	    print(2, 1/0)
	end if
    end if
end procedure

integer nsquared_ok
nsquared_ok = TRUE -- do N-squared sorts

integer iterations
sequence data
file_number printer

procedure measure(sequence name)
-- time one sort    
    integer id
    atom t0, t
    sequence sdata

    id = routine_id(name)
    t0 = time()
    for i = 1 to iterations do
	sdata = call_func(id, {data[i]})
	
	check_results(sdata, data[i])
    end for
    t = time() - t0
    if t > 6 then
	nsquared_ok = FALSE -- time to give up on the slow ones!
    end if
    printf(printer, "%15s %9.4f\n", {name, t/iterations})
end procedure

procedure all_sorts()
-- test all sorting routines over a range of numbers of items

    natural nitems
    
    printer = 1  -- open("PRN", "w")
    hybrid_limit = 20
    
    nitems = 5
    iterations = floor(MAX/nitems)
    
    while TRUE do
	-- get several sets of data of length nitems
	printf(printer, "\ntime (sec.) to sort %d items (averaged over %d trials)\n",
			{nitems, iterations})

	data = rand(repeat(repeat(nitems, nitems), iterations))
	min_value = 1
	max_value = nitems

	if nsquared_ok then
	    -- do the really slow ones
	    measure("bubble_sort")
	    measure("simple_sort")
	    measure("insertion_sort")
	end if

	measure("merge_sort")
	measure("quick_sort")
	measure("hybrid_sort")  
	measure("great_sort")
	measure("shell_sort")
	measure("bucket_sort")      

	nitems *= 2
	iterations = floor(MAX/nitems)
	if iterations < 1 then
	    exit
	end if
	
	puts(1, "\nPress Enter to continue. Press q to quit: ")
	if find('q', gets(0)) then
	    abort(0)
	end if
    end while
end procedure

all_sorts()

