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
open Prop;;
open Lexuniv;;

let rec lire_proposition f = proposition5 f

and proposition0 = parser
  | [< 'Ident s >] -> Variable s
  | [< 'MC "vrai" >] -> Vrai
  | [< 'MC "faux" >] -> Faux
  | [< 'MC "("; p = lire_proposition; 'MC ")" >] -> p

and proposition1 = parser
  | [< 'MC "non"; p = proposition0 >] -> Non p
  | [< p = proposition0 >] -> p

and proposition2 = parser
  | [< p = proposition1; q = reste2 p >] -> q
and reste2 p = parser
  | [< 'MC "et"; q = proposition1; r = reste2 (Et (p, q)) >] -> r
  | [<>] -> p

and proposition3 = parser
  | [< p = proposition2; q = reste3 p >] -> q
and reste3 p = parser
  | [< 'MC "ou"; q = proposition2; r = reste3 (Ou (p, q)) >] -> r
  | [<>] -> p

and proposition4 = parser
  | [< p = proposition3; q = reste4 p >] -> q
and reste4 p = parser
  | [< 'MC "=>"; q = proposition3; r = reste4 (Implique (p, q)) >] -> r
  | [<>] -> p

and proposition5 = parser
  | [< p = proposition4; q = reste5 p >] -> q
and reste5 p = parser
  | [< 'MC "<=>"; q = proposition4; r = reste5 (Équivalent(p,q)) >] -> r
  | [<>] -> p;;

let lire_opération lire_opérateur lire_base constructeur =
  let rec lire_reste e1 = parser
  | [< _ = lire_opérateur;
       e2 = lire_base;
       e = lire_reste (constructeur (e1, e2)) >] -> e
  | [< >] -> e1 in
 parser [< e1 = lire_base; e = lire_reste e1 >] -> e;;

let rec lire_proposition f = proposition5 f

and proposition0 = parser
  | [< 'Ident s >] -> Variable s
  | [< 'MC "vrai" >] -> Vrai
  | [< 'MC "faux" >] -> Faux
  | [< 'MC "("; p = lire_proposition; 'MC ")" >] -> p

and proposition1 = parser
  | [< 'MC "non"; p = proposition0 >] -> Non p
  | [< p = proposition0 >] -> p

and proposition2 flux =
    lire_opération
      (parser [< 'MC "et" >] -> ())
      proposition1
      (function (p, q) -> Et (p,q))
      flux
and proposition3 flux =
    lire_opération
      (parser [< 'MC "ou" >] -> ())
      proposition2
      (function (p, q) -> Ou (p,q))
      flux
and proposition4 flux =
    lire_opération
      (parser [< 'MC "=>" >] -> ())
      proposition3
      (function (p, q) -> Implique (p,q))
      flux
and proposition5 flux =
    lire_opération
      (parser [< 'MC "<=>" >] -> ())
      proposition4
      (function (p, q) -> Équivalent (p,q))
      flux;;

let analyseur_lexical =
    construire_analyseur
     ["vrai"; "faux"; "("; ")"; "non"; "et"; "ou"; "=>"; "<=>"];;

let analyse_proposition chaîne =
    lire_proposition (analyseur_lexical (Stream.of_string chaîne));;
