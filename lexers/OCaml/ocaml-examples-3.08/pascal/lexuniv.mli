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
   | MC of string
   | Ident of string
   | Entier of int;;

val construire_analyseur :
     string list -> (char Stream.t -> lex�me Stream.t);;

val string_of_lex�me : lex�me -> string;;
