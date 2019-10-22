# This is an example of using 
# dynamic datatypes.
# 
# In this case we're using the mpz 
# integer type from the gmp package...
#
import datatype mpz

main	
define lv_a mpz
define b integer
#call load_datatype("mpz")

let lv_a="400000000001"

display lv_a
for b=1 to 10
	# This one returns a value...
	call lv_a.nextprime() returning lv_a
	display lv_a
	display "And the next will be : ", lv_a.nextprime()
end for


display "Setting fib numbers"

for b=1 to 10
	call lv_a.setfib(b)  # This has been defined as an 'inplace' function
			  # ie. it changes the internal value rather than returning a value...
	display lv_a
end for


end main

