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
open Graphics;;
open Langage;;
open Alex;;
open Asynt;;

let boucle () =
  let flux_d'entrée = Stream.of_channel stdin in
  let flux_lexèmes = analyseur_lexical flux_d'entrée in
  try
    Crayon.vide_écran ();
    while true do
      print_string "? "; flush stdout;
      try exécute_programme (analyse_programme flux_lexèmes) with
      | Stream.Error s ->
          print_string ("Erreur de syntaxe : " ^ s); print_newline ()
      | Failure s ->
          print_string "Erreur à l'exécution: "; print_string s;
          print_newline ()
    done
  with
  | Stream.Failure -> ();;

if not !Sys.interactive then begin boucle (); close_graph (); exit 0 end;;
