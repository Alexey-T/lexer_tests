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
open Expr;;

type état =
  { mutable transitions : (char * état) list;
    mutable epsilon_transitions : état list;
    mutable terminal : bool;
    numéro : int };;

val expr_vers_automate : expr -> état;;
