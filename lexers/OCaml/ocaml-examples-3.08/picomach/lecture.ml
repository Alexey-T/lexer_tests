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
open Code;;
open Stockage;;
open Lexuniv;;

let registre = parser
  | [< 'MC "r"; 'Entier nbr >] -> nbr
  | [< 'MC "sp" >] -> sp
  | [< 'MC "ra" >] -> ra;;

let constante = parser
  | [< 'Entier nbr >] -> nbr
  | [< 'Ident nom_�tiq >] -> valeur_�tiquette nom_�tiq;;

let op�rande = parser
  | [< r = registre >] -> Reg r
  | [< c = constante >] -> Imm c;;

let rec instruction = parser
  | [< op = op�ration; (r1, o, r2) = reg_op_reg >] ->
          assemble(Op(op, r1, o, r2))
  | [< test = test_invers�; (r1, o, r2) = reg_op_reg >] ->
          assemble(Op(test, r1, o, r2));
          assemble(Op(Seq, r2, Reg 0, r2))
  | [< 'MC "jmp"; o = op�rande; 'MC ","; r = registre >] ->
          assemble(Jmp(o, r))
  | [< 'MC "braz"; r = registre; 'MC ","; c = constante >] ->
          assemble(Braz(r, c))
  | [< 'MC "branz"; r = registre; 'MC ","; c = constante >] ->
          assemble(Branz(r, c))
  | [< 'MC "scall"; 'Entier n >] -> assemble (Scall n)
  | [< 'MC "write" >] -> assemble (Scall 1)
  | [< 'MC "read" >] -> assemble (Scall 0)
  | [< 'MC "stop" >] -> assemble Stop

and reg_op_reg = parser
  | [< r1 = registre; 'MC ","; o = op�rande; 'MC ","; r2 = registre >] ->
      (r1, o, r2)

and op�ration = parser
  | [< 'MC "load" >] -> Load    | [< 'MC "store" >] -> Store
  | [< 'MC "add" >]  -> Add     | [< 'MC "mult" >]  -> Mult
  | [< 'MC "sub" >]  -> Sub     | [< 'MC "div" >]   -> Div
  | [< 'MC "and" >]  -> And     | [< 'MC "or" >]    -> Or
  | [< 'MC "xor" >]  -> Xor     | [< 'MC "shl" >]   -> Shl
  | [< 'MC "shr" >]  -> Shr     | [< 'MC "slt" >]   -> Slt
  | [< 'MC "sle" >]  -> Sle     | [< 'MC "seq" >]   -> Seq

and test_invers� = parser
  | [< 'MC "sgt" >] -> Sle
  | [< 'MC "sge" >] -> Slt
  | [< 'MC "sne" >] -> Seq;;

let d�finition_d'�tiquette = parser
  | [< 'Ident nom_�tiq; 'MC ":" >] -> poser_�tiquette nom_�tiq;;

let rec instruction_�tiq = parser
  | [< _ = d�finition_d'�tiquette; _ = instruction_�tiq >] -> ()
  | [< _ = instruction >] -> ();;

let rec suite_d'instructions = parser
  | [< _ = instruction_�tiq; flux >] -> suite_d'instructions flux
  | [< >] -> ();;

let analyseur_lexical =
    construire_analyseur
      ["r"; "sp"; "ra"; "load"; "store"; "add"; "mult"; "sub"; "div";
       "and"; "or"; "xor"; "shl"; "shr"; "sgt"; "sge"; "sne"; 
       "slt"; "sle"; "seq"; "jmp"; "braz"; "branz";
       "scall"; "write"; "read"; "stop"; ","; ":"];;

let programme flux =
    initialise ();
    suite_d'instructions (analyseur_lexical flux);
    extraire_code ();;
