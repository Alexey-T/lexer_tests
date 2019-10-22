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
open Syntaxe;;
open Types;;

type environnement = (string * sch�ma_de_types) list;;

val type_exp : environnement -> expression -> type_simple;;
val type_d�f : environnement -> d�finition -> environnement;;

exception Erreur of string;;
