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
type lexème =
   | Mot of string
   | Symbole of char
   | Constante_entière of int
   | Constante_flottante of float;;

val analyseur_lexical : char Stream.t -> lexème Stream.t;;

val string_of_lexème :  lexème -> string;;
