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

(* Module [Realloc]: reallocation of vectors. *)

(* File realloc.ml contains the implementation of module [Realloc]. *)

let realloc v n =
  let l = Array.length v in
  if n <= l then v else
  if l = 0 then invalid_arg "cannot realloc empty vectors" else
  let res = Array.make n v.(0) in
  Array.blit v 1 res 1 (l - 1);
  res;;
