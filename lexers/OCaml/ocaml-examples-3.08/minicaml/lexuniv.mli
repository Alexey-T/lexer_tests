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
   | MC of string
   | Ident of string
   | Entier of int;;

val construire_analyseur :
     string list -> (char Stream.t -> lexème Stream.t);;

val string_of_lexème : lexème -> string;;
