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

type �tat =
  { mutable transitions : (char * �tat) list;
    mutable epsilon_transitions : �tat list;
    mutable terminal : bool;
    num�ro : int };;

val expr_vers_automate : expr -> �tat;;
