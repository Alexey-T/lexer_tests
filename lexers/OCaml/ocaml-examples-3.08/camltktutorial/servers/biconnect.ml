(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(* Copyright 2001, 2004 Institut National de Recherche en Informatique *)
(* et en Automatique. All rights reserved. This file is distributed    *)
(* under the terms of the Q Public License version 1.0.                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: Exp *)

(* The biconnect program that connects two programs via stdin/stdout. *)

let usage () =
  prerr_endline "Usage: biconnect prog1 prog2";
  exit 2;;

let main () =
  let args = Sys.argv in
  if Array.length args <> 3 then usage () else
  Bipipe.launch_connected_processes args.(1) args.(2);;

main ();;
