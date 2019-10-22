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

compile "lexuniv.mli";;
compile "lexuniv.ml";;
#load "lexuniv.cmo";;

compile "syntaxe.mli";;
compile "syntaxe.ml";;
#load "syntaxe.cmo";;

compile "valeur.mli";;
compile "valeur.ml";;
#load "valeur.cmo";;

print_string "coucou"; print_newline();;

compile "envir.mli";;
compile "envir.ml";;
#load "envir.cmo";;

compile "interp.mli";;
compile "interp.ml";;
#load "interp.cmo";;

compile "typage.mli";;
compile "typage.ml";;
#load "typage.cmo";;

compile "ipascal.ml";;
#load "ipascal.cmo";;

compile "compil.mli";;
compile "compil.ml";;
#load "compil.cmo";;

compile "cpascal.ml";;
#load "cpascal.cmo";;

open Ipascal;;
open Cpascal;;

print_string
 "Pour lancer:\n  \
  interprète_fichier \"fichier source\";;\n  \
  compile_fichier \"fichier source\";;\n\n\
  Par exemple:\n  \
  compile_fichier \"fib2.p\";;\n  \
  ou encore\n  \
  compile_fichier \"reines.p\";;\n";
print_newline ();;

