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
type 'a t;;

val vide : 'a t;;
val ajoute : 'a t -> int -> 'a -> 'a t;;
val extraire : 'a t -> int * 'a * 'a t;;

exception File_vide;;
