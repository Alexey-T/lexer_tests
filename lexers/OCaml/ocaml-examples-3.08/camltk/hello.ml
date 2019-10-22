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

(* The interactive Hello world program: creates a single button with a
call back function to print "Hello world!" on standard input.

Compile using:
 ocamlc -I +labltk -c hello.ml
Link using:
 ocamlc -I +labltk -custom labltk.cma  hello.cmo -o hello

Try with ./hello

*)

open Camltk;;

let action () = print_string "Hello world!"; print_newline ();;

let hello () =
  let main_window = openTk () in
  let bouton_press =
    Button.create main_window
      [Text "Press me"; Command action] in
  pack [bouton_press] [];
  mainLoop ();;

if !Sys.interactive then () else begin hello (); closeTk () end;;
