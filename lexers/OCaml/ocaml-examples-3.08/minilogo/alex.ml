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
type lexème =
   | Mot of string
   | Symbole of char
   | Constante_entière of int
   | Constante_flottante of float;;

let string_of_lexème = function
  | Mot s -> "Mot " ^ s
  | Symbole c -> "Symbole " ^ String.make 1 c
  | Constante_entière i -> "Constante_entière " ^ string_of_int i
  | Constante_flottante f -> "Constante_flottante " ^ string_of_float f;;

let rec saute_blancs = parser
  | [< '' '; flux >] -> saute_blancs flux  (* ' ' est l'espace *)
  | [< ''\t'; flux >] -> saute_blancs flux (* '\t' est la tabulation *)
  | [< ''\n'; flux >] -> saute_blancs flux (* '\n' est la fin de ligne *)
  | [< >] -> ();;

let rec saute_blancs = parser
  | [< ' (' ' | '\t' | '\n'); flux >] -> saute_blancs flux
  | [< >] -> ();;

let rec lire_entier accumulateur = parser
  | [< ' ('0' .. '9' as c); flux >] ->
      lire_entier (10 * accumulateur + int_of_char c - 48) flux
  | [< >] -> accumulateur;;

let rec lire_décimales accumulateur échelle = parser
  | [< ' ('0' .. '9' as c); flux >] ->
      lire_décimales
        (accumulateur +.
           float_of_int(int_of_char c - 48) *. échelle)
        (échelle /. 10.0) flux
  | [< >] -> accumulateur;;

let tampon = String.make 16 '-';;

let rec lire_mot position = parser
  | [< '( 'A' .. 'Z' | 'a' .. 'z' | 'é' | 'è' | '_' as c); flux >] ->
      if position < String.length tampon then
        tampon.[position] <- c;
      lire_mot (position + 1) flux
  | [< >] ->
      String.sub tampon 0 (min position (String.length tampon));;

let rec lire_lexème flux =
  saute_blancs flux;
  let find_lexème = parser
    | [< '( 'A' .. 'Z' | 'a' .. 'z' | 'é' | 'è' as c) >] ->
        tampon.[0] <- c;
        Mot(lire_mot 1 flux)
    | [< '( '0' .. '9' as c) >] ->
        let n = lire_entier (int_of_char c - 48) flux in
        begin match flux with parser
        | [< ''.' >] ->
            Constante_flottante
              (lire_décimales (float_of_int n) 0.1 flux)
        | [< >] -> Constante_entière n end
    | [< 'c >] -> Symbole c in
  find_lexème flux;;

let analyseur_lexical flux =
  Stream.from
   (fun _ -> try Some (lire_lexème flux) with Stream.Failure -> None);;
