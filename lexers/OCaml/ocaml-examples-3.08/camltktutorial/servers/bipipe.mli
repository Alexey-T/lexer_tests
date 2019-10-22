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

(* The bipipe module to launch two programs connected via stdin/stdout. *)

val launch_connected_processes : string -> string -> unit;;
 (** [launch_connected_processes prog1 prog2] launches [prog1] and
 [prog2] with each other reading and writing to the corresponding
 [stdin/stdout] of the other program. *)
