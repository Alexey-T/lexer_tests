(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  only by permission.                                                *)
(*                                                                     *)
(***********************************************************************)


(*

  We learn here:

  - Arguments of a program are the elements of the predefined array Sys.argv

  - The length (or number of elements) of array v is returned by
       Array.length v

  - You access to elem number i of array v by the notation
       v.(i)
    (numerotation starts form 0, hence the first element of v is v.(0))

  - The conditional (or alternative construct) has syntax
       if ... then ... else ... 

  - Convertions are explicit in Caml. The general naming convention for
    convertions is <type1>_of_<type2>, that converts a value of type type2
    into a value of type type1. Hence, use
       int_of_string s to obtain the integer represented by string s.

  - The general naming convention for printing functions is
       print_<type>. Hence, use print_int to print an integer value.

  - The predefined function print_newline outputs a newline then flushes
    the standard output.

*)
let args = Sys.argv in
if Array.length args = 2 then
  let n = int_of_string Sys.argv.(1) in
  print_int (n * n);
  print_newline ()
else
  print_string "Usage: square <number>\n";;

