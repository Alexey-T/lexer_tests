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

compile "expr.mli";;
compile "expr.ml";;
#load "expr.cmo";;
open Expr;;

compile "auto.mli";;
compile "auto.ml";;
#load "auto.cmo";;
open Auto;;

compile "ensent.mli";;
compile "ensent.ml";;
#load "ensent.cmo";;
open Ensent;;

compile "determ.mli";;
compile "determ.ml";;
#load "determ.cmo";;
open Determ;;

compile "grep.ml";;
#load "grep.cmo";;
open Grep;;

print_string "Pour lancer: grep \"expression rationnelle\" \"nom de fichier\"";
print_newline();;
