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
open Lexuniv;;

type constante =
  | Enti�re of int
  | Bool�enne of bool;;

type expr_type =
  | Integer                          (* le type des entiers *)
  | Boolean                          (* le type des bool�ens *)
  | Array of int * int * expr_type;; (* le type des tableaux *)
                         (* (les deux "int" sont les bornes) *)
type expression =
  | Constante of constante
  | Variable of string
  | Application of string * expression list
  | Op_unaire of string * expression
  | Op_binaire of string * expression * expression
  | Acc�s_tableau of expression * expression;;

type instruction =
  | Affectation_var of string * expression
  | Affectation_tableau of expression * expression * expression
  | Appel of string * expression list   (* appel de proc�dure *)
  | If of expression * instruction * instruction
  | While of expression * instruction
  | Write of expression
  | Read of string
  | Bloc of instruction list;;          (* bloc begin ... end *)

type d�cl_proc =
  { proc_param�tres: (string * expr_type) list;
    proc_variables: (string * expr_type) list;
    proc_corps: instruction }
and d�cl_fonc =
  { fonc_param�tres: (string * expr_type) list;
    fonc_type_r�sultat: expr_type;
    fonc_variables: (string * expr_type) list;
    fonc_corps: instruction };;

type programme =
  { prog_variables: (string * expr_type) list;
    prog_proc�dures: (string * d�cl_proc) list;
    prog_fonctions: (string * d�cl_fonc) list;
    prog_corps: instruction };;

let analyseur_lexical = construire_analyseur
  ["false";"true";"("; ","; ")"; "["; "]"; "not"; "*"; "/"; "-"; "+";
   "="; "<>"; "<"; ">"; "<="; ">="; "and"; "or"; "if"; "then"; "else";
   "while"; "do"; "write"; "read"; "begin"; ";"; "end"; ":=";
   "integer"; "boolean"; "array"; "of"; ".."; "var"; ":";
   "procedure"; "function"; "program"];;

let lire_liste lire_�l�ment s�parateur =
  let rec lire_reste = parser
    | [< 'MC s when s = s�parateur;
         elt = lire_�l�ment;
         reste = lire_reste >] -> elt :: reste
    | [< >] -> [] in
  parser
  | [< elt = lire_�l�ment; reste = lire_reste >] -> elt :: reste
  | [< >] -> [];;

let lire_op�rateur op�rateurs = parser
  [< 'MC op when List.mem op op�rateurs >] -> op;;

let lire_op�ration lire_base op�rateurs =
  let rec lire_reste e1 = parser
  | [< op = lire_op�rateur op�rateurs;
       e2 = lire_base;
       e = lire_reste (Op_binaire(op, e1, e2)) >] -> e
  | [< >] -> e1 in
 parser [< e1 = lire_base; e = lire_reste e1 >] -> e;;

let rec lire_expr0 flux =
  match flux with parser
  | [< 'Entier n >] -> Constante(Enti�re n)
  | [< 'MC "false" >] -> Constante(Bool�enne false)
  | [< 'MC "true" >] -> Constante(Bool�enne true)
  | [< 'Ident nom >] ->
      begin match flux with parser
      | [< 'MC "("; el = lire_liste lire_expr ","; 'MC ")">] ->
                 Application(nom, el)
      | [< >] -> Variable nom
      end
  | [< 'MC "("; e = lire_expr; 'MC ")" >] -> e

and lire_expr1 flux =
  match flux with parser
  | [< e1 = lire_expr0 >] ->
      match flux with parser
      | [< 'MC "["; e2 = lire_expr; 'MC "]" >] -> Acc�s_tableau(e1,e2)
      | [< >] -> e1

and lire_expr2 = parser
  | [< 'MC "-"; e = lire_expr1 >] -> Op_unaire("-", e)
  | [< 'MC "not"; e = lire_expr1 >] -> Op_unaire("not", e)
  | [< e = lire_expr1 >] -> e

and lire_expr3 flux = 
  lire_op�ration lire_expr2 ["*"; "/"] flux
and lire_expr4 flux = 
  lire_op�ration lire_expr3 ["+"; "-"] flux
and lire_expr5 flux = 
  lire_op�ration lire_expr4 ["="; "<>"; "<"; ">"; "<="; ">="] flux
and lire_expr6 flux = 
  lire_op�ration lire_expr5 ["and"] flux
and lire_expr flux = 
  lire_op�ration lire_expr6 ["or"] flux;;

let rec lire_instr flux =
  match flux with parser
  | [< 'MC "if"; e1 = lire_expr; 'MC "then"; i2 = lire_instr >] ->
      begin match flux with parser
      | [< 'MC "else"; i3 = lire_instr >] -> If(e1, i2, i3)
      | [< >] -> If(e1, i2, Bloc [])
      end
  | [< 'MC "while"; e1 = lire_expr; 'MC "do"; i2 = lire_instr >] ->
      While(e1, i2)
  | [< 'MC "write"; 'MC "("; e = lire_expr; 'MC ")" >] ->
      Write e
  | [< 'MC "read"; 'MC "("; 'Ident nom; 'MC ")" >] ->
      Read nom
  | [< 'MC "begin"; il = lire_liste lire_instr ";"; 'MC "end" >] ->
      Bloc il
  | [< e = lire_expr >] ->
      match e with
      | Application(nom, el) ->
          Appel(nom, el)
      | Variable nom ->
          begin match flux with parser
          | [< 'MC ":="; e = lire_expr >] ->
              Affectation_var(nom, e)
          end
      | Acc�s_tableau(e1, e2) ->
          begin match flux with parser
            [< 'MC ":="; e3 = lire_expr >] ->
              Affectation_tableau(e1, e2, e3)
          end
      | _ -> raise (Stream.Error "Illegal instruction");;

let rec lire_type = parser
  | [< 'MC "integer" >] -> Integer
  | [< 'MC "boolean" >] -> Boolean
  | [< 'MC "array"; 'MC "["; 'Entier bas; 'MC ".."; 'Entier haut;
       'MC "]"; 'MC "of"; ty = lire_type >] -> Array(bas, haut, ty);;

let rec lire_variables = parser
  | [< 'MC "var"; 'Ident nom; 'MC ":"; ty = lire_type; 'MC ";";
       reste = lire_variables >] -> (nom,ty)::reste
  | [< >] -> [];;

let lire_un_param�tre = parser
    [< 'Ident nom; 'MC ":"; ty = lire_type >] -> (nom,ty);;

let lire_param�tres = parser
    [< 'MC "(";
       param�tres = lire_liste lire_un_param�tre ",";
       'MC ")" >] -> param�tres;;

let lire_proc�dure = parser
  [< 'MC "procedure"; 'Ident nom; p = lire_param�tres; 'MC ";";
     v = lire_variables; i = lire_instr; 'MC ";" >] ->
       (nom, {proc_param�tres = p; proc_variables = v; proc_corps = i});;

let lire_fonction = parser
  [< 'MC "function"; 'Ident nom; p = lire_param�tres; 'MC ":";
     ty = lire_type; 'MC ";"; v = lire_variables;
     i = lire_instr; 'MC ";" >] ->
       (nom, {fonc_param�tres = p; fonc_type_r�sultat = ty;
              fonc_variables = v; fonc_corps = i});;

let rec lire_proc_fonc = parser
  | [< proc = lire_proc�dure; (procs, foncs) = lire_proc_fonc >] ->
      (proc :: procs, foncs)
  | [< fonc = lire_fonction; (procs, foncs) = lire_proc_fonc >] ->
       (procs, fonc :: foncs)
  | [< >] -> ([], []);;

let lire_prog = parser
    [< 'MC "program"; 'Ident nom_du_programme; 'MC ";";
       v = lire_variables; (p, f) = lire_proc_fonc; i = lire_instr >] ->
    { prog_variables = v; prog_proc�dures = p;
      prog_fonctions = f; prog_corps = i };;

let lire_programme flux = lire_prog (analyseur_lexical flux);;
