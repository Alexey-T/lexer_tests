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

let run_command com f =
 match Sys.command (com ^ " " ^ f) with
 | 0 -> ()
 | _ -> failwith (Printf.sprintf "Cannot succeed to run ``%s %s''" com f);;

let compile f = run_command "ocamlc -c" f;;

run_command "ocamlyacc" "parser.mly";;

compile "parser.mli";;

run_command "ocamllex" "lexer.mll";;
compile "lexer.ml";;

compile "parser.ml";;

compile "calc.ml";;

#load "lexer.cmo";;
#load "parser.cmo";;
#load "calc.cmo";;

compile "main.ml";;

#load "main.cmo";;

open Main;;

print_string
 "\nTo run: type\n   calc ();;\n\n \
  Try for instance:\n  \
   1 + 2\n\
  See the README file for more information.\n";
print_newline();;

