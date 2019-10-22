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

(* The GUI of the simple computation server.
   The GUI has two buttons to interact with the computation server:
   - a button to clik on,
   - a button to report the number of clicks so far.
   A third button is available to quit the GUI.

The GUI only recognizes the new click count that the computation
server should return. *)

open Camltk;;

let parse_compute_message() =
  let s = input_line stdin in
  try
    ignore (int_of_string s); s
  with
  | Failure _ -> "Invalid server response";;

let bclick_cb b () =
  (* Output a message to the compuation server. *)
  output_string stdout "button\n"; flush stdout;
  (* Parse its answer and act accordingly. *)
  let s = parse_compute_message () in
  Button.configure b [Text s];;

let topwindow = openTk ();;

let bcount = Button.create topwindow [Text "never clicked"];;

let bclick = Button.create topwindow
  [Text "hello world!"; Command (bclick_cb bcount)];;

let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;

let gui_server () =
  Button.configure bquit
    [Background (NamedColor "red"); Foreground (NamedColor "white")];
  pack [bclick; bcount; bquit] [Side Side_Left];
  mainLoop();;

gui_server ();;
