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
open Code;;

exception Erreur of string * int;;

val lire_mémoire : int -> int;;
val écrire_mémoire : int -> int -> unit;;
val lire_registre : int -> int;;
val écrire_registre : int -> int -> unit;;
val tableau_des_appels_système: (int -> int) array;;

val exécute: instruction array -> int -> unit;;
