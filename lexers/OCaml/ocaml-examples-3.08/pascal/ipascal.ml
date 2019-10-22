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
open Syntaxe;;

let interprète_fichier nom =
  try
    let canal = open_in nom in
    try
      let prog = lire_programme (Stream.of_channel canal) in
      close_in canal;
      Typage.type_programme prog;                 (* ligne ajoutée *)
      Interp.exécute_programme prog
    with
    | Stream.Error s ->
        prerr_string
          ("Erreur de syntaxe : " ^ s ^ " aux alentours du caractère numéro ");
        prerr_int (pos_in canal);
        prerr_endline ""
    | Typage.Erreur_typage err ->
        Typage.affiche_erreur err;
        exit 2
    | Valeur.Erreur_exécution message ->
        prerr_string "Erreur pendant l'exécution: ";
        prerr_endline message
  with
  | Sys_error message ->
      prerr_string "Erreur du système: ";
      prerr_endline message
  | Stream.Failure -> ();;

if not !Sys.interactive
  then begin interprète_fichier Sys.argv.(1); exit 0 end;;







