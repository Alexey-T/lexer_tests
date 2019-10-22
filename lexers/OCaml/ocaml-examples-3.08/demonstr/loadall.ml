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

compile "prop.mli";;
compile "prop.ml";;
#load "prop.cmo";;

compile "lexuniv.mli";;
compile "lexuniv.ml";;
#load "lexuniv.cmo";;

compile "asynt.mli";;
compile "asynt.ml";;
#load "asynt.cmo";;

compile "demo.ml";;
#load "demo.cmo";;
open Demo;;

print_string "Pour lancer: boucle();;"; print_newline();;
