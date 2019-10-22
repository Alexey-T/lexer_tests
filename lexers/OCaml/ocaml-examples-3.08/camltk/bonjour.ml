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

(* Le programme interactif [bonjour]: on cr�e un bouton qui, lorsqu'il
   est press�, appelle une fonction qui �crit "Bonjour!" au terminal.

Compilation:
 ocamlc -I +labltk -c bonjour.ml
�dition des liens
 ocamlc -I +labltk -custom labltk.cma  bonjour.cmo -o bonjour

Essayez en lan�ant ./bonjour

*)

open Camltk;;

let action () = print_string "Bonjour!"; print_newline ();;

let bonjour () =
  let fen�tre_principale = openTk () in
  let bouton_press =
    Button.create fen�tre_principale
      [Text "Pressez-moi"; Command action] in
  pack [bouton_press] [];
  mainLoop ();;

if !Sys.interactive then () else begin bonjour (); closeTk () end;;

