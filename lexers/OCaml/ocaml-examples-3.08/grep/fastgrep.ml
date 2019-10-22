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
open Expr;;
open Auto;;
open Determ;;

let ligne_trouv�e = ref false;;

let grep_sur_canal auto nom_fich canal =
  try
    while true do
      let ligne = input_line canal in
      if Fastrec.reconna�t auto ligne then begin
        ligne_trouv�e := true;
        print_string nom_fich;
        print_string": ";
        print_endline ligne
      end
    done
  with End_of_file -> ();;

let grep_sur_fichier auto nom_fich =
  try
    let canal = open_in nom_fich in
    try grep_sur_canal auto nom_fich canal; close_in canal
    with exc -> close_in canal; raise exc
  with Sys_error message ->
    prerr_string "Erreur sur le fichier ";
    prerr_string nom_fich;
    prerr_string ": ";
    prerr_endline message;;

let construire_auto expr =
  d�terminise(expr_vers_automate(lire(Stream.of_string expr)));;

let grep expr fichier =
  grep_sur_fichier (construire_auto expr) fichier;;

if !Sys.interactive then () else
  if Array.length Sys.argv < 2 then begin
    prerr_endline "Utilisation: grep <motif> <fichiers>";
    exit 2
  end else begin
    let auto =
      try construire_auto Sys.argv.(1) with
      | Stream.Error s ->
          prerr_endline ("Erreur de syntaxe dans l'expression: " ^ s);
          exit 2
      | Stream.Failure ->
          prerr_endline "Erreur de syntaxe dans l'expression";
          exit 2 in
    if Array.length Sys.argv >= 3 then
      for i = 2 to Array.length Sys.argv - 1 do
        grep_sur_fichier auto Sys.argv.(i)
      done
    else
      grep_sur_canal auto "(entr�e standard)" stdin;
    exit (if !ligne_trouv�e then 0 else 1)
  end;;
