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

(* The interactive additionner program.

Compile using:
 ocamlc -I +labltk -c addition.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  addition.cmo -o addition

Try with ./addition
*)

open Camltk;;

let addition () =
  let main_window = openTk () in
  (* The widgets. They all have "main_window" as parent widget. *)
  let first_operand =
    Entry.create main_window [TextWidth 6; Relief Sunken] in
  let operation = Label.create main_window [Text "+"] in
  let second_operand =
    Entry.create main_window [TextWidth 6 ; Relief Sunken] in
  let result_sign = Label.create main_window [Text "="] in
  let result = Label.create main_window [] in
  (* References that contains the values of variables of the program. *)
  let x = ref 0
  and y = ref 0  in
  (* Refresh result *)
  let refresh () =
    Label.configure result [Text (string_of_int (!x + !y))]  in
  (* Electric *)
  let get_and_refresh (w, r) _ _ =
    try
      r := int_of_string (Entry.get w);
      refresh ()
    with
    | Failure "int_of_string" ->
        Label.configure result [Text "error"] in
  (* Set the callbacks *)
  Entry.configure
    first_operand [XScrollCommand (get_and_refresh (first_operand, x))];
  Entry.configure
    second_operand [XScrollCommand (get_and_refresh (second_operand, y))];
  let quit_button =
    Button.create main_window
      [Text "Quit"; Command closeTk] in
  (* Map the widgets *)
  pack
    [first_operand; operation; second_operand; result_sign; result]
    [Side Side_Left];
  pack [quit_button] [];
  (* Make the window resizable *)
  Wm.minsize_set main_window 1 1;
  (* Start interaction (event-driven program) *)
  mainLoop ();;

if !Sys.interactive then () else Printexc.catch addition ();;
