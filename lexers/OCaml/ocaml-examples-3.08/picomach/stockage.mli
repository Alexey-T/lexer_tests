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

exception Erreur of string;;

val initialise : unit -> unit;;
val assemble : instruction -> unit;;
val poser_étiquette : string -> unit;;
val valeur_étiquette : string -> int;;
val extraire_code : unit -> instruction array;;
