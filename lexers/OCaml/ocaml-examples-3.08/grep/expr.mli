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
type expr =
  | Epsilon
  | Caract�res of char list
  | Alternative of expr * expr
  | S�quence of expr * expr
  | R�p�tition of expr;;

val lire : char Stream.t -> expr;;
