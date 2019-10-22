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
type valeur =
   | Inconnue
   | Ent of int
   | Bool of bool
   | Tableau of int * valeur array;;

exception Erreur_exécution of string;;

val ent_val : valeur -> int;;
val bool_val : valeur -> bool;;
val tableau_val : valeur -> int * valeur array;;
val affiche_valeur : valeur -> unit;;
val lire_valeur : unit -> valeur;;
