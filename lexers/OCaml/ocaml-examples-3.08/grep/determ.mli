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
type état =
  { mutable dtransitions : transition array;
    dterminal : bool }
and transition =
  | Vers of état
  | Rejet;;

val déterminise : Auto.état -> état;;
val reconnaît : état -> string -> bool;;
