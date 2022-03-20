report.

" A customer had background jobs that were planned at fixed intervals.
" Over a given period of time, the job periods would start getting
" planned at the exact moment, causing certain job runs to fail

" My solution to the customer was to quickly just adjust the code 
" To start the job that'll begin after a random amount of minutes
" between a given range.

" When I searched google to find different methods for generating 
" random numbers I got a different answer from each site. On top 
" of that, doing a quick search in transaction SE37 I came across
" 81 different functions for generating random data.

" I don't know there are so many. But I'll try to document a few
" of them here and maybe try to explain the thought behind the function.

*> This function is based on C's rand() function, the number
*> which you receive back from this function is inbetween 0 to the range.
*> C: int rand = low_value % rand() + high_value;
data random type i.
call function 'GENERAL_GET_RANDOM_INT'
  importing range = 100
  exporting random = random.

*> This function is just described as 'random number generation'
*> The last time this function was modified was in 1998, I wouldn't use it.
data random_2 type qf00-ran_int.
call function 'QF05_RANDOM_INTEGER'
  importing ran_int_max = 100
            ran_int_min = 0
  exporting ran_int = random_2
 exceptions invalid_input = 1.
if ( sy-subrc = 0 ).
  write : / 'You really shouldn''t have a problem passing a number to an interface.'.
endif.

*> This function is also just described as 'random number generation'
*> It's in the same function group as the above function
*> But this one exports a SEED value as well it seems.
data random_3 type qf00-ran_int.
data seed_3 type qf00-ran_seed.
call function 'QF05_RANDOM'
  importing ran_number  = random_3
            ran_seed = seed_3.
