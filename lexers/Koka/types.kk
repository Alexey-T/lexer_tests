/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen. 

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Core types.

   This module is implicitly imported and all functions and types
   are always available.
   These types are required to be defined for the compiler
   to work correctly (i.e. types like `:int` or `:div`)
*/
public module std/core/types

infixr 30  (&&)
infixr 20  (||)

// ----------------------------------------------------------------------------
// Core types
// ----------------------------------------------------------------------------

// An arbitrary precision signed integer.
type int

// A string is a sequence of unicode character points (`char`).
// The encoding of a string is internal and there
// is no constant-time indexing of characters in a string.
// Use the `:sslice` type for efficient matching and retrieving
// sub-strings from string.\
// See also the [``std/text/string``](std_text_string.html) module.
type string

// A 64-bit IEEE 754 floating point value.
// See also `std/num/double` for more operations on `:double`s.
value type double

// A 32-bit signed integer (represented in two's complement).
// See the `module std/num/int32` module for operations on 32-bit integers.
value type int32

// A 64-bit signed integer (represented in two's complement).
// See the `module std/num/int64` module for operations on 64-bit integers.
value type int64

// A signed two's complement integer equal to a signed `size_t` in C, i.e.
// it can hold the byte size of any object, in particular the maximum possible
// byte size of a vector or string.
// Provides no operations but is used mostly for interaction with external code.
// The Javascript backend uses `int32` for this.
value type ssize_t

// A signed two's complement integer equal to an `intptr_t` in C, i.e.
// it has the same bit-size as a native pointer (`void*`).
// Provides no operations but is used mostly for interaction with external code.
// The Javascript backend uses `int64` for this.
value type intptr_t

// A 8-bit unsigned integer.
// Provides no operations but is used mostly for interaction with external code.
value type byte

// A 32-bit IEEE 754 floating point value.
// Not supported by the Javascript backend
// Provides no operations but is used mostly for interaction with external code.
value type float32




// An any type. Used for extern calls
reference type any

// Internal type used for in-place update of unique pattern matches
type reuse

extern no-reuse() : reuse {
  c inline  "NULL"
  js inline "null"
}

// A unicode character.
// Characters are unicode _codepoint_\/s.
// This is different from a unicode _grapheme_ which represents a single displayed
// symbol and can consists of multiple codepoints due to combining characters and marks.
// (see also the [``std/text/unicode``](std_text_unicode.html) module.)
value type char

// The _total_ effect represents the absence of any effect.
type <> :: E

// The effect constructor extends an effect with another effect.
type <|> :: (X,E) -> E

// The type of immutable arrays is called `:vector`.
type vector<a>


// The divergence effect: a divergent function may not terminate.
type div :: X

// The non-determism effect.
type ndet :: X

// The alloc effect signifies a function may allocate in a heap `:h`
type alloc :: H -> X

// The read effect: signifies that a function may read from from heap `:h`.
type read :: H -> X

// The write effect: signifies that a function may write to heap `:h`.
type write :: H -> X

// Stateful funs can manipulate heap `:h` using allocations, reads and writes.
alias st<h> = <read<h>, write<h>, alloc<h>>

// The `:global` heap is a special heap constant to denote the global shared heap
type global :: H


// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

// The `:void` type is empty and has no constructors.
// See also the `:()` unit type and the `:bool` type.
type void

// The type of booleans has two inhabitants: `True` and `False`.
type bool {
  con False
  con True
}

// The unit type `:()` is inhabited by just a single value, namely `()`.
// See also the `:void` type and the `:bool` type.
struct ()

// A pair of values `:a` and `:b`.
struct (,)<a,b>(fst:a,snd:b)

// A triple of values.
struct (,,)<a,b,c>(fst:a,snd:b,thd:c)

// A quadruple of values.
struct (,,,)<a,b,c,d>(fst:a,snd:b,thd:c,field4:d)

// A quintuple of values.
struct (,,,,)<a,b,c,d,e>(fst:a,snd:b,thd:c,field4:d,field5:e)

// The `:maybe` type is used to represent either a value (`Just(x)`) or `Nothing`.
// This type is often used to represent values that can be _null_.
type maybe<a> {
  con Nothing
  con Just( value : a )
}

// The choice type represents one of two possible types `:a` or `:b`.
type either<a,b> {
  con Left( left : a )
  con Right( right : b )
}

// An enumeration to represent order
type order {
  Lt
  Eq
  Gt
}

// Explicitly box values using the `Box` constructor.
value type box<a> {
  con Box( unbox : a )
}

// Explicitly heap allocate using the `Hbox` constructor.
reference type hbox<a> {
  con Hbox( unhbox : a )
}

fun hbox( x : a ) : hbox<a> {
  Hbox(x)
}

noinline fun keep( x : a ) : a {
  x
}

// ----------------------------------------------------------------------------
// Standard functions
// ----------------------------------------------------------------------------

fun id(x : a) : a {
  x
}

// Logical conjuction
fun (&&)( x : bool, y : bool) : bool {  // inlined in the compiler for short-circuit evaluation
  if (x) then y else False
}

// Logical disjunction
fun (||)( x : bool, y : bool) : bool { // inlined in the compiler for short-circuit evaluation
  if (x) then True else y
}


// inline extern (&&) : (bool,bool) -> bool  { inline "(#1 && #2)" }
// inline extern (||) : (bool,bool) -> bool  { inline "(#1 || #2)" }

fun (!)( b : bool ) : bool 
  if (b) then False else True

fun not( b : bool ) : bool 
  if (b) then False else True

//inline extern not  : (bool) -> bool  { inline "!(#1)" }
//inline extern (!)  : (bool) -> bool  { inline "!(#1)" }

// needed for markers in `std/core/hnd`.
inline extern zero32() : int32 { inline "0" }


// Generated by type inference and later refined into one of the `open` variants in `std/core/hnd`.
public noinline extern ".open"<e1::E,e2::E,a,b>( x : a ) : e2 b {
  inline "(#1)"
}

// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

// The predicate `:hdiv<h,a,e>` signifies that if the type `:a` contains a reference to `:h`,
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// when reading from the heap (see `(!)`) and is necessary to catch cases where code can diverge
// by storing self referential functions in the heap.
type hdiv :: (H,V,E) -> P

// The predicate `:ediv<x,a,e>` signifies that if the type `:a` contains a reference to effect constant `:x",
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// for operations on first-class effects where code can diverge through operations.
type ediv :: (X,V,E) -> P


// A reference `:ref<h,a>` points to a value of type `:a` in heap `:h`.
type ref :: (H,V) -> V

// Allocate a fresh reference with an initial value.
inline extern ref( value : a) : alloc<h> ref<h,a> {
  c inline "kk_ref_alloc(#1,kk_context())"
  cs inline "new Ref<##1,##2>(#1)"
  js inline "{ value: #1 }"
}

// Assign a new value to a reference.
inline extern set( ^ref : ref<h,a>, assigned : a) : <write<h>> () {
  c inline "kk_ref_set_borrow(#1,#2,kk_context())"
  cs inline "#1.Set(#2)"
  js inline "((#1).value = #2)"
}

// Read the value of a reference.
inline extern (!) : forall<h,a,e> ( ref : ref<h,a>) -> <read<h>|e> a with(hdiv<h,a,e>)  {
  c inline "kk_ref_get(#1,kk_context())"
  cs inline "#1.Value"
  js inline "((#1).value)"
}

// Modify the value of a reference.
// This is especially useful when the reference contains a vector, because
// getting the vector into a local variable and then setting it back into the reference later
// would mean that we hold on to a copy (and we can't update the vector in place then!).
// In contrast, this function will pass its ownership to the given function.
inline extern modify : forall<h,a,b,e> ( ref : ref<h,a>, f : forall<s> local-var<s,a> -> <local<s>|e> b) -> <read<h>,write<h>|e> b with(hdiv<h,a,e>)  {
  c  inline "kk_ref_modify(#1, #2, kk_context())"
  js inline "((#2)(#1))"
}

// If a heap effect is unobservable, the heap effect can be erased by using the `run` fun.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
extern run : forall<e,a> ( action : forall<h> () -> <alloc<h>,read<h>,write<h> | e> a ) -> e a  {
  c  inline "(kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),#1,(#1,kk_context())))"
  cs inline "Primitive.Run<##2>(#1)"
  js inline "((#1)())"
}

// ----------------------------------------------------------------------------
// Local Variables
// ----------------------------------------------------------------------------

// A local variable `:var<s,a>` points to a value of type `:a` in local scope `:s`
type local :: (H) -> X
type local-var :: (H,V) -> V

// Allocate a fresh local with an initial value.
inline extern local-new<a,s,e>(value:a) : <local<s>|e> local-var<s,a> {
  c inline "kk_ref_alloc(#1,kk_context())"
  cs inline "new Ref<##1,##2>(#1)"
  js inline "{ value: #1 }"
}

// Assign a new value to a local variable
inline extern local-set<a,s,e>( ^v: local-var<s,a>, assigned: a) : <local<s>|e> () {
  c inline "(kk_ref_set_borrow(#1,#2,kk_context()))"
  cs inline "#1.Set(#2)";
  js inline "((#1).value = #2)"
}

// Read the value of a local variable.
inline extern local-get : forall<a,s,e> (v: local-var<s,a>) -> <local<s>|e> a with(hdiv<s,a,e>)  {
  c inline "(kk_ref_get(#1,kk_context()))"
  cs inline "#1.Value";
  js inline "((#1).value)";
}

private inline extern unsafe-no-local-cast : forall<a,e> ( action : forall<h> () -> <local<h>|e> a ) -> (() -> e a) { inline "#1" }

// If local mutation is unobservable, the `:local` effect can be erased by using the `local-scope` function.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
inline fun local-scope<a,e>( action : forall<h> () -> <local<h> | e> a) : e a  {
  unsafe-no-local-cast(action)()
}

/*
inline extern local-scope : forall<e,a> ( action : forall<h> () -> <local<h> | e> a) -> e a  {
  c  "(kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),#1,(#1,kk_context())))"
  cs inline "Primitive.Run<##2>(#1)";
  js inline "((#1)())";
}
*/

inline extern byref(loc : a) : a { inline "#1" }

// ----------------------------------------------------------------------------
// Unsafe: todo: move to separate module
// ----------------------------------------------------------------------------

// _Unsafe_. Mark a function parameter as decreasing to suppress the non-termination effect (`:div`).
inline extern unsafe-decreasing( x : a ) : a { inline "#1" }

private inline extern unsafe-total-cast : forall<e,a> ( action : () -> e a ) -> (() -> a) { inline "#1" }

// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
fun unsafe-total( action : () -> e a ) : a {
  unsafe-total-cast( action )()
}

// _Unsafe_. This function pretends the give action was deterministic
fun unsafe-no-ndet( action : () -> <ndet|e> a ) : e a {
  unsafe-total-cast( action )()
}

// _Unsafe_. This function pretends the give action is terminating
fun unsafe-no-div( action : () -> <div|e> a ) : e a {
  unsafe-total-cast( action )()
}

// ----------------------------------------------------------------------------
// Internal types
// ----------------------------------------------------------------------------

// Optional is used internally by the compiler to pass optional arguments.
// It is usually displayed as `:?a` for some type `:a`.
value type optional<a> {
  // The `Optional` constructor is used when an optional argument is given.
  con Optional( value : a )
  // `None` is used when an optional argument is not provided.
  con None
}

// Handled effects are lifted to an atomic effect using the `:handled` type constructor
type handled    :: HX -> X;

// Linear effects are lifted to an atomic effect using the `:handled1` type constructor
type handled1   :: HX1 -> X;



// ----------------------------------------------------------------------------
// These primitives are used by the compiler for
// _tail recursion module cons_ (TRMC) optimization.
// ----------------------------------------------------------------------------

extern import 
  c header-end-file "types-ctail-inline.h"
  js file "types-ctail-inline.js"


// _Internal_. Internal type for _tail recursion module cons_ (TRMC) optimization.
// Holds the address to a field of type `:a` in a constructor.
value type cfield<a>

// _Internal_. Internal type for _tail recursion module cons_ (TRMC) optimization.
abstract value type ctail<a>
  ".CTail"( res : a, hole : cfield<a> )

/*
// address of a field in a constructor (this is implemented in the compiler backend)
inline extern ".cfield-of"( obj : a, conname : string, fieldname : string ) : cfield<a> { } 

// create 'hole' of a certain type that is to be filled in later by ctail-link/ctail-resolve
// implemented in the compiler backend as it needs to by typed correctly (and should not be unboxed)
inline extern ".cfield-hole"() : a { }

// initial accumulator (generated in the Core/CTail.hs)
inline extern ".ctail-nil"() : ctail<a> {
  c inline "kk_ctail_nil()"
}

// accumulate (generated in the Core/CTail.hs)
inline extern ".ctail-link"( acc : ctail<a>, res : a, field : cfield<a> ) : ctail<a> {
  c inline "kk_ctail_link(#1,#2,#3)"
}

// resolve to final result (generated in the Core/CTail.hs)
inline extern ".ctail-resolve"( acc : ctail<a>, res : a ) : ctail<a> {
  c inline "kk_ctail_resolve(#1,#2)"
}
*/
