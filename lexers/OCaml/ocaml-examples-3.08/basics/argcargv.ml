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

(* Prints out arguments and the number of arguments of a caml program.

Usage: compile with ocamlc, then execute the a.out program generated
       by ocamlc.

ocamlc argcargv.ml

Then try:
a.out
a.out 1 2
a.out 1 2 "ok" -f "/tmp/foo"
*)
open Printf;;

let main argc argv =
 printf "Command line has %i arguments\n" argc;
 for i = 0 to argc - 1 do
  printf "argument %i is %s\n" i argv.(i)
 done;;

main (Array.length Sys.argv) Sys.argv;;
