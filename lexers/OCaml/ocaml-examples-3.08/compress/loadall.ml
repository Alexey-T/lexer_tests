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

compile "esbit.mli";;
compile "esbit.ml";;
#load "esbit.cmo";;

compile "fileprio.mli";;
compile "fileprio.ml";;
#load "fileprio.cmo";;

compile "huffman.mli";;
compile "huffman.ml";;
#load "huffman.cmo";;

compile "compr.ml";;
#load "compr.cmo";;
open Compr;;

print_string
"Pour lancer: compresse_fichier \"nom du fichier\" ou
             décompresse_fichier \"nom du fichier\"";
print_newline();;
