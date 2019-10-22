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
open Eval;;

let code_nombre n =
    Val_nombre n
and d�code_nombre = function
  | Val_nombre n -> n
  | _ -> raise(Erreur "entier attendu")
and code_bool�en b =
    Val_bool�enne b
and d�code_bool�en = function
  | Val_bool�enne b -> b
  | _ -> raise(Erreur "bool�en attendu");;

(* Pour transformer une fonction Caml en valeur fonctionnelle *)

let prim1 codeur calcul d�codeur =
  Val_primitive (function v -> codeur (calcul (d�codeur v)))
and prim2 codeur calcul d�codeur1 d�codeur2 =
  Val_primitive (function
   | Val_paire (v1, v2) ->
       codeur (calcul (d�codeur1 v1) (d�codeur2 v2))
   | _ -> raise (Erreur "paire attendue"));;

(* L'environnement initial *)

let env_initial =
  ["+",  prim2 code_nombre  ( + ) d�code_nombre d�code_nombre;
   "-",  prim2 code_nombre  ( - ) d�code_nombre d�code_nombre;
   "*",  prim2 code_nombre  ( * ) d�code_nombre d�code_nombre;
   "/",  prim2 code_nombre  ( / ) d�code_nombre d�code_nombre;
   "=",  prim2 code_bool�en ( = ) d�code_nombre d�code_nombre;
   "<>", prim2 code_bool�en ( <> ) d�code_nombre d�code_nombre;
   "<",  prim2 code_bool�en ( < ) d�code_nombre d�code_nombre;
   ">",  prim2 code_bool�en ( > ) d�code_nombre d�code_nombre;
   "<=", prim2 code_bool�en ( <= ) d�code_nombre d�code_nombre;
   ">=", prim2 code_bool�en ( >= ) d�code_nombre d�code_nombre;
   "not", prim1 code_bool�en ( not ) d�code_bool�en;
   "read_int", prim1 code_nombre (fun x -> read_int ()) d�code_nombre;
   "write_int", prim1 code_nombre
                      (fun x -> print_int x; print_newline (); 0)
                      d�code_nombre];;
let boucle () =
  let env_global = ref env_initial in
  let flux_d'entr�e = Stream.of_channel stdin in
  try
    while true do
      print_string "# "; flush stdout;
      try
        match lire_phrase flux_d'entr�e with
        | Expression expr ->
            let r�s = �value !env_global expr in
            print_string "- = "; imprime_valeur r�s;
            print_newline ()
        | D�finition d�f ->
            let nouvel_env = �value_d�finition !env_global d�f in
            begin match nouvel_env with
            | (nom, v) :: _ ->
                print_string nom; print_string " = ";
                imprime_valeur v; print_newline ()
            | _ -> failwith "mauvaise gestion des d�finitions"
            end;
            env_global := nouvel_env
      with
      | Stream.Error s ->
          print_string ("Erreur de syntaxe: " ^ s); print_newline ()
      | Erreur msg ->
          print_string "Erreur � l'�valuation: "; print_string msg;
          print_newline ()
    done
  with Stream.Failure -> ();;

if not !Sys.interactive then boucle ();;
