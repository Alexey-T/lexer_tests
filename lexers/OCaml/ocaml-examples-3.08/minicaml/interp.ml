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
and décode_nombre = function
  | Val_nombre n -> n
  | _ -> raise(Erreur "entier attendu")
and code_booléen b =
    Val_booléenne b
and décode_booléen = function
  | Val_booléenne b -> b
  | _ -> raise(Erreur "booléen attendu");;

(* Pour transformer une fonction Caml en valeur fonctionnelle *)

let prim1 codeur calcul décodeur =
  Val_primitive (function v -> codeur (calcul (décodeur v)))
and prim2 codeur calcul décodeur1 décodeur2 =
  Val_primitive (function
   | Val_paire (v1, v2) ->
       codeur (calcul (décodeur1 v1) (décodeur2 v2))
   | _ -> raise (Erreur "paire attendue"));;

(* L'environnement initial *)

let env_initial =
  ["+",  prim2 code_nombre  ( + ) décode_nombre décode_nombre;
   "-",  prim2 code_nombre  ( - ) décode_nombre décode_nombre;
   "*",  prim2 code_nombre  ( * ) décode_nombre décode_nombre;
   "/",  prim2 code_nombre  ( / ) décode_nombre décode_nombre;
   "=",  prim2 code_booléen ( = ) décode_nombre décode_nombre;
   "<>", prim2 code_booléen ( <> ) décode_nombre décode_nombre;
   "<",  prim2 code_booléen ( < ) décode_nombre décode_nombre;
   ">",  prim2 code_booléen ( > ) décode_nombre décode_nombre;
   "<=", prim2 code_booléen ( <= ) décode_nombre décode_nombre;
   ">=", prim2 code_booléen ( >= ) décode_nombre décode_nombre;
   "not", prim1 code_booléen ( not ) décode_booléen;
   "read_int", prim1 code_nombre (fun x -> read_int ()) décode_nombre;
   "write_int", prim1 code_nombre
                      (fun x -> print_int x; print_newline (); 0)
                      décode_nombre];;
let boucle () =
  let env_global = ref env_initial in
  let flux_d'entrée = Stream.of_channel stdin in
  try
    while true do
      print_string "# "; flush stdout;
      try
        match lire_phrase flux_d'entrée with
        | Expression expr ->
            let rés = évalue !env_global expr in
            print_string "- = "; imprime_valeur rés;
            print_newline ()
        | Définition déf ->
            let nouvel_env = évalue_définition !env_global déf in
            begin match nouvel_env with
            | (nom, v) :: _ ->
                print_string nom; print_string " = ";
                imprime_valeur v; print_newline ()
            | _ -> failwith "mauvaise gestion des définitions"
            end;
            env_global := nouvel_env
      with
      | Stream.Error s ->
          print_string ("Erreur de syntaxe: " ^ s); print_newline ()
      | Erreur msg ->
          print_string "Erreur à l'évaluation: "; print_string msg;
          print_newline ()
    done
  with Stream.Failure -> ();;

if not !Sys.interactive then boucle ();;
