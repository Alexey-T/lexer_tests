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

val lire_m�moire : int -> int;;
val �crire_m�moire : int -> int -> unit;;
val lire_registre : int -> int;;
val �crire_registre : int -> int -> unit;;
val tableau_des_appels_syst�me: (int -> int) array;;

val ex�cute: instruction array -> int -> unit;;
