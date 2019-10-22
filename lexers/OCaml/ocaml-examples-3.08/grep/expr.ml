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
open List;;

type expr =
  | Epsilon
  | Caract�res of char list
  | Alternative of expr * expr
  | S�quence of expr * expr
  | R�p�tition of expr;;

let subtract l1 l2 =
  match l1, l2 with
  | _, [] | [], _ -> l1
  | _ ->
     let rec sub = function
       | [] -> []
       | elem :: l -> if mem elem l2 then sub l else elem :: sub l in
     sub l1;;

let union l1 l2 =
  fold_right (fun e res -> if mem e res then res else e :: res) l1 l2;;

let intervalle c1 c2 =
  let rec interv n1 n2 =
    if n1 > n2 then [] else char_of_int n1 :: interv (n1 + 1) n2 in
  interv (int_of_char c1) (int_of_char c2);;

let tous_car = intervalle '\000' '\255';;

let rec lire_expr = parser
  | [< r1 = lire_s�q; r2 = lire_alternative r1 >] -> r2

and lire_alternative r1 = parser
  | [< ''|'; r2 = lire_expr >] -> Alternative(r1,r2)
  | [< >] -> r1

and lire_s�q = parser
  | [< r1 = lire_r�p�t; r2 = lire_fin_s�q r1 >] -> r2

and lire_fin_s�q r1 = parser
  | [< r2 = lire_s�q >] -> S�quence(r1,r2)
  | [< >] -> r1

and lire_r�p�t = parser
  | [< r1 = lire_simple; r2 = lire_fin_r�p�t r1 >] -> r2

and lire_fin_r�p�t r1 = parser
  | [< ''*' >] -> R�p�tition r1
  | [< ''+' >] -> S�quence(r1, R�p�tition r1)
  | [< ''?' >] -> Alternative(r1, Epsilon)
  | [< >] -> r1

and lire_simple = parser
  | [< ''.' >] -> Caract�res tous_car
  | [< ''['; cl = lire_classe >] -> Caract�res cl
  | [< ''('; r = lire_expr; '')' >] -> r
  | [< ''\\'; 'c >] -> Caract�res [c]
  | [< 'c when c <> '|' && c <> ')' && c <> '$' >] ->
      Caract�res [c]

and lire_classe = parser
  | [< ''^'; cl = lire_ensemble >] -> subtract tous_car cl
  | [< cl = lire_ensemble >] -> cl

and lire_ensemble = parser
  | [< '']' >] -> []
  | [< c1 = lire_car; c2 = lire_intervalle c1 >] -> c2

and lire_intervalle c1 = parser
  | [< ''-'; c2 = lire_car; reste = lire_ensemble >] ->
        union (intervalle c1 c2) reste
  | [< reste = lire_ensemble >] -> union [c1] reste

and lire_car = parser
  | [< ''\\'; 'c >] -> c
  | [< 'c >] -> c;;

let lire = parser
  | [< chapeau = (parser | [< ''^' >] -> true | [< >] -> false);
       r = lire_expr;
       dollar = (parser | [< ''$' >] -> true | [< >] -> false) >] ->
      let r1 =
       if dollar then r else S�quence(r, R�p�tition(Caract�res tous_car)) in
      if chapeau then r1 else S�quence(R�p�tition(Caract�res tous_car), r1);;
