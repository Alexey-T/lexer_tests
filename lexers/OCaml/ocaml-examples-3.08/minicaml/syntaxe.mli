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
type expression =
   | Variable of string
   | Fonction of (motif * expression) list
   | Application of expression * expression
   | Let of d�finition * expression
   | Bool�en of bool
   | Nombre of int
   | Paire of expression * expression
   | Nil
   | Cons of expression * expression

and motif =
  | Motif_variable of string
  | Motif_bool�en of bool
  | Motif_nombre of int
  | Motif_paire of motif * motif
  | Motif_nil
  | Motif_cons of motif * motif

and d�finition =
  { r�cursive: bool;
    nom: string;
    expr: expression };;

type phrase =
  | Expression of expression
  | D�finition of d�finition;;

val lire_phrase : char Stream.t -> phrase;;
