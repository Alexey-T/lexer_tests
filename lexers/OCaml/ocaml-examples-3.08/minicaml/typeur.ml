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
open Types;;
open Synthese;;

let type_arithm�tique = sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_int)
and type_comparaison = sch�ma_trivial
  (type_fl�che (type_produit type_int type_int) type_bool);;

let env_initial =
  ["+",  type_arithm�tique; "-", type_arithm�tique;
   "*",  type_arithm�tique; "/", type_arithm�tique;
   "=",  type_comparaison; "<>", type_comparaison;
   "<",  type_comparaison; ">", type_comparaison;
   "<=", type_comparaison; ">=", type_comparaison;
   "not", sch�ma_trivial (type_fl�che type_bool type_bool);
   "read_int", sch�ma_trivial (type_fl�che type_int type_int);
   "write_int", sch�ma_trivial (type_fl�che type_int type_int)];;

let boucle () =
  let env_global = ref env_initial in
  let flux_d'entr�e = Stream.of_channel stdin in
  try
    while true do
      print_string "# "; flush stdout;
      try
        match lire_phrase flux_d'entr�e with
        | Expression expr ->
            let ty = type_exp !env_global expr in
            print_string "- : "; imprime_type ty;
            print_newline ()
        | D�finition d�f ->
            let nouvel_env = type_d�f !env_global d�f in
            begin match nouvel_env with
            | (nom, sch�ma) :: _ ->
                print_string nom; print_string " : ";
                imprime_sch�ma sch�ma; print_newline ()
            | _ -> failwith "mauvaise gestion des d�fintions"
            end;
            env_global := nouvel_env
      with
      | Stream.Error s ->
          print_string ("Erreur de syntaxe: " ^ s); print_newline ()
      | Conflit (ty1, ty2) ->
          print_string "Incompatibilit� de types entre ";
          imprime_type ty1; print_string " et ";
          imprime_type ty2; print_newline()
      | Circularit� (var, ty) ->
          print_string "Impossible d'identifier ";
          imprime_type var; print_string " et ";
          imprime_type ty; print_newline()
      | Erreur msg ->
          print_string "Erreur de typage: "; print_string msg;
          print_newline ()
    done
  with Stream.Failure -> ();;

if not !Sys.interactive then boucle ();;
