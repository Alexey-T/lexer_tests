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

(* A simple program to welcome a user.

 We learn here that:

 A sequence of commands is denoted by the symbole ``;''.
   To evaluate stm1 then stm2, use 
     stm1; stm2

 Output is bufferized:
   to flush the standard output, use flush stdout

 To define identifier name to have the value e, use
     let name = e in
     ...

 Reading strings:
   to read a single line from standard input, use input_line stdin.

 To catenate strings s1 and s2, use s1 ^ s2

 To terminate a program, use
   exit 0 (for normal termination)
   exit 2 (for failure or abnormal termination)
*)
print_string "What's your name ? ";
flush stdout;
let answer = input_line stdin in
print_string ("Hello " ^ answer);
print_string ", nice to meet you!\n";
exit 0;;
