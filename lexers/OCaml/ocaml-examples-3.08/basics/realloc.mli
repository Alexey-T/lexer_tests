(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Module Realloc: reallocation of vectors. *)

(* File realloc.mli contains the interface of module [Realloc]:
   it defines the signature of functions defined in the module. *)

val realloc : 'a array -> int -> 'a array;;
  (* [realloc v n] returns an array of length at least [n] that
     contains the elements of array [v] as its initial segment. *)
