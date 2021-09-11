cimport cython

ctypedef fused my_fused_type:
    cython.int
    cython.double

ctypedef fused my_fused_type2:
    cython.int
    cython.double

cdef func(my_fused_type a, my_fused_type2 b):
    # a and b may have the same or different types here
    print("SAME!" if my_fused_type is my_fused_type2 else "NOT SAME!)
    return a + b

ctypedef fused A:
    int
    long

ctypedef fused B:
    int
    long

def myfunc(A[:] a, B[:] b):
    # a and b are independent types here and may have different item types
    ...

cdef myfunc(cython.floating, cython.integral):
    ...

# assign directly
cdef object (*funcp)(float, int)
funcp = myfunc
funcp(f, i)

ctypedef fused bunch_of_types:
    ...

ctypedef fused string_t:
    cython.p_char
    bytes
    unicode

cdef cython.integral myfunc(cython.integral i, bunch_of_types s):
    cdef int *int_pointer
    cdef long *long_pointer

    # Only one of these branches will be compiled for each specialization!
    if cython.integral is int:
        int_pointer = &i
    else:
        long_pointer = &i

    if bunch_of_types in string_t:
        print("s is a string!")
        
cimport cython

ctypedef fused double_or_object:
    cython.double
    object

def increment(double_or_object x):
    with nogil(double_or_object is cython.double):
        # Same code handles both cython.double (GIL is released)
        # and python object (GIL is not released).
        x = x + 1
    return x

DEF FREE_GIL = True

with nogil(FREE_GIL):
    <code to be executed with the GIL released>

    with gil(False):
       <GIL is still released>
