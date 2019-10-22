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
type proposition =
   | Vrai
   | Faux
   | Non of proposition
   | Et of proposition * proposition
   | Ou of proposition * proposition
   | Implique of proposition * proposition
   | Équivalent of proposition * proposition
   | Variable of string;;

exception Réfutation of (string * bool) list;;

val vérifie_tautologie : proposition -> string list -> unit;;
val variables_libres : proposition -> string list;;

