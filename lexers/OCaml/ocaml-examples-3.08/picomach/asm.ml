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
let assemble_fichier nom_entrée nom_sortie =
    let entrée = open_in nom_entrée in
    let sortie = open_out_bin nom_sortie in
      try
        output_value sortie
          (Lecture.programme (Stream.of_channel entrée));
        close_in entrée;
        close_out sortie;
        0
      with exc ->
        close_in entrée;
        close_out sortie;
        Sys.remove nom_sortie;
        match exc with
        | Stream.Error _ ->
            prerr_string
              "Erreur de syntaxe aux alentours du caractère numéro ";
            prerr_int (pos_in entrée);
            prerr_endline "";
            1
        | Stockage.Erreur message ->
            prerr_string "Erreur d'assemblage: ";
            prerr_endline message;
            1
        | _ ->
            raise exc;;

exception Mauvais_arguments;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv <> 3 then raise Mauvais_arguments;
  exit (assemble_fichier Sys.argv.(1) Sys.argv.(2))
with
| Mauvais_arguments ->
    prerr_endline
      "Usage: pico_asm <fichier assembleur> <fichier de code>";
    exit 2
| Sys_error message ->
    prerr_string "Erreur du système: "; prerr_endline message;
    exit 2;;
