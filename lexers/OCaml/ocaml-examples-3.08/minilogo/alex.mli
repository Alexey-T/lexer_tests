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
type lex�me =
   | Mot of string
   | Symbole of char
   | Constante_enti�re of int
   | Constante_flottante of float;;

val analyseur_lexical : char Stream.t -> lex�me Stream.t;;

val string_of_lex�me :  lex�me -> string;;
