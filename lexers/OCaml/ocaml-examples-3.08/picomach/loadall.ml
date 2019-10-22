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
let run_command com f =
 match Sys.command (com ^ " " ^ f) with
 | 0 -> ()
 | _ -> failwith (Printf.sprintf "Cannot succeed to run ``%s %s''" com f);;

let compile f = run_command "ocamlc -c" f;;

compile "code.mli";;
compile "code.ml";;
#load "code.zo";;
compile "simul.mli";;
compile "simul.ml";;
#load "simul.zo";;
compile "exec.ml";;
#load "exec.zo";;
compile "stockage.mli";;
compile "stockage.ml";;
#load "stockage.zo";;
compile "lexuniv.mli";;
compile "lexuniv.ml";;
#load "lexuniv.zo";;
compile "lecture.mli";;
compile "lecture.ml";;
#load "lecture.zo";;
compile "asm.ml";;
#load "asm.zo";;

open Exec;;
open Asm;;

print_string
"Pour assembler un fichier:
     assemble_fichier \"fichier source\" \"fichier résultat\"
Pour executer le fichier produit:
     exécute_fichier \"fichier résultat\" 4096";
print_newline();;
