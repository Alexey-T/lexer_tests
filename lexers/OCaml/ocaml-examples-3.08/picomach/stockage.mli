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
val poser_�tiquette : string -> unit;;
val valeur_�tiquette : string -> int;;
val extraire_code : unit -> instruction array;;
