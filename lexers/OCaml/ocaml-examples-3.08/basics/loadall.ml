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
#use "fib.ml";;
#use "sieve.ml";;
#use "queens.ml";;
#use "wc.ml";;
#use "strstr.ml";;
#use "soli.ml";;
print_string "To run:
        fib <some number>;;
        sieve <upper bound>;;
        queens <chess size>;;
        count \"file name\";;
        strstr \"pattern\" \"string\";;
        solve_solitaire ();;
";
print_newline();;

