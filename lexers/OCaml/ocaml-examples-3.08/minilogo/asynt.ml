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
open Langage;;
open Alex;;

let rec analyse_programme s = Programme(analyse_prog s)

and analyse_prog = parser
  | [< ph = analyse_phrase; p = analyse_prog >] -> ph :: p
  | [< 'Symbole '.' >] -> []
  | [< 'lxm >] ->
     raise (Stream.Error ("Lexème inconnu: " ^ string_of_lexème lxm))

and analyse_phrase = parser
  | [< 'Mot "pour"; 'Mot s;
       variables = paramètres; ordres = suite_d'ordres >] ->
     Pour (s, {paramètres = variables; corps = ordres})
  | [< ord = ordre >] -> Ordre ord

and paramètres = parser
  | [< 'Symbole ':'; 'Mot s; l = paramètres >] -> s :: l
  | [< >] -> []

and ordre = parser
  | [< '(Mot "avance" | Mot "av"); e = expression >] -> Av e
  | [< '(Mot "recule" | Mot "re"); e = expression >] -> Re e
  | [< '(Mot "droite" | Mot "td"); e = expression >] -> Td e
  | [< '(Mot "gauche" | Mot "tg"); e = expression >] -> Tg e
  | [< '(Mot "baisse_crayon" | Mot "bc") >] -> Bc
  | [< '(Mot "lève_crayon" | Mot "lc") >] -> Lc
  | [< '(Mot "vide_écran" | Mot "ve") >] -> Ve
  | [< 'Mot "stop" >] -> Stop
  | [< 'Mot "si";
        e1 = expression; 'Symbole '>';  'Symbole '='; e2 = expression;
        alors = liste_d'ordres;
        sinon = liste_d'ordres >] -> Si (e1, e2, alors, sinon) 
  | [< '(Mot "répète" | Mot "rep");
        e = expression; l = liste_d'ordres >] -> Rep (e,l)
  | [< 'Mot f; es = liste_d'expressions >] -> Exécute (f, es)

and liste_d'ordres = parser
  | [< 'Symbole '['; l = suite_d'ordres; 'Symbole ']' >] -> l
and suite_d'ordres = parser
  | [< ord = ordre; l = suite_d'ordres >] -> ord :: l
  | [< >] -> []

and nombre = parser
  | [< 'Symbole '-'; n = nombre >] ->
     begin match n with
     | Entier i -> Entier (-i)
     | Flottant f -> Flottant (-. f)
     end
  | [< 'Constante_entière i >] -> Entier i
  | [< 'Constante_flottante f >] -> Flottant f

and expression_simple = parser
  | [< n = nombre >] -> Constante n
  | [< 'Symbole ':'; 'Mot var >] -> Variable var
  | [< 'Symbole '('; e = expression; 'Symbole ')' >] -> e

and expression = parser
  | [< e = expression_simple; e' = reste_de_l'expression e >] -> e'
and reste_de_l'expression e = parser
  | [< 'Symbole '+'; e2 = expression >] -> Somme (e, e2)
  | [< 'Symbole '*'; e2 = expression >] -> Produit (e, e2)
  | [< 'Symbole '-'; e2 = expression >] -> Différence (e, e2)
  | [< 'Symbole '/'; e2 = expression >] -> Quotient (e, e2)
  | [< >] -> e

and liste_d'expressions = parser
  | [< e = expression; l = liste_d'expressions >] -> e :: l
  | [< >] -> [];;
