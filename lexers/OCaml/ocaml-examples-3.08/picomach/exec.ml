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
open Code;;
open Simul;;

exception Fichier_incorrect;;

let ex�cute_fichier nom_fichier taille_m�moire =
    let canal = open_in_bin nom_fichier in
    let programme =
      try (input_value canal : instruction array)
      with Failure _ -> raise Fichier_incorrect in
    close_in canal;
    ex�cute programme taille_m�moire;;

exception Mauvais_arguments;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv < 2 then raise Mauvais_arguments;
  let taille_m�moire =
    if Array.length Sys.argv < 3
    then 1024
    else try int_of_string Sys.argv.(2)
         with Failure _ -> raise Mauvais_arguments in
  ex�cute_fichier Sys.argv.(1)
                  (taille_du_mot * taille_m�moire);
  exit 0
with
| Mauvais_arguments ->
    prerr_endline "Usage: pico_run <fichier> [taille m�moire]";
    exit 2
| Fichier_incorrect ->
    prerr_endline "Le fichier ne contient pas du code ex�cutable";
    exit 2
| Erreur(message, param) ->
    prerr_string "Erreur � l'ex�cution: ";
    prerr_string message;
    prerr_string " ("; prerr_int param; prerr_endline ")";
    exit 2
| Sys_error message ->
    prerr_string "Erreur du syst�me: "; prerr_endline message;
    exit 2;;
