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
type �tat =
  { mutable dtransitions : transition array;
    dterminal : bool }
and transition =
  | Vers of �tat
  | Rejet;;

val d�terminise : Auto.�tat -> �tat;;
val reconna�t : �tat -> string -> bool;;
