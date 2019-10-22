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

(* A simple computation server: it receives messages from the GUI and
treat them accordingly.

 In this example, there is a single acceptable message, namely
 "button". When receiving this message, the server simply add one to the
 number of such messages it has received so far. Then, the server's answer
 to the GUI is just the new count. *)

let button_press_number = ref 0;;

let treat_button_press () =
  incr button_press_number;
  string_of_int !button_press_number;;

let parse_gui_message () =
  let s = input_line stdin in
  match s with
  | "button" -> treat_button_press ()
  | s -> "Syntax error: " ^ s;;

let treat_gui_message () =
  let answer = parse_gui_message() in
  output_string stdout answer;
  output_char stdout '\n';
  flush stdout;;

let computation_server () =
  try while true do treat_gui_message () done
  with End_of_file -> exit 0;;

computation_server ();;
