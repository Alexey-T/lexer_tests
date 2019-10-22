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
let compile f =
 match Sys.command ("ocamlc -c " ^ f) with
 | 0 -> ()
 | _ -> failwith ("Cannot compile " ^ f);;

compile "syntaxe.mli";;

compile "eval.mli";;
compile "eval.ml";;
#load "eval.cmo";;

compile "lexuniv.mli";;
compile "lexuniv.ml";;
#load "lexuniv.cmo";;

compile "syntaxe.ml";;
#load "syntaxe.cmo";;

compile "types.mli";;
compile "types.ml";;
#load "types.cmo";;

compile "synthese.mli";;
compile "synthese.ml";;
#load "synthese.cmo";;

compile "caml.ml";;
#load "caml.cmo";;
open Caml;;

print_string
 "Pour lancer: boucle();;\n\
  Essayez par exemple:\n\
  let rec fib =\n\
    function x -> if x <= 1 then 1 else fib (x - 1) + fib (x - 2);;";
print_newline();;



