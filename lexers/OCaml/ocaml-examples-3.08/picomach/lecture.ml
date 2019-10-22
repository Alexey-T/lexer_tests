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
  | [< 'Ident nom_étiq >] -> valeur_étiquette nom_étiq;;

let opérande = parser
  | [< r = registre >] -> Reg r
  | [< c = constante >] -> Imm c;;

let rec instruction = parser
  | [< op = opération; (r1, o, r2) = reg_op_reg >] ->
          assemble(Op(op, r1, o, r2))
  | [< test = test_inversé; (r1, o, r2) = reg_op_reg >] ->
          assemble(Op(test, r1, o, r2));
          assemble(Op(Seq, r2, Reg 0, r2))
  | [< 'MC "jmp"; o = opérande; 'MC ","; r = registre >] ->
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
  | [< r1 = registre; 'MC ","; o = opérande; 'MC ","; r2 = registre >] ->
      (r1, o, r2)

and opération = parser
  | [< 'MC "load" >] -> Load    | [< 'MC "store" >] -> Store
  | [< 'MC "add" >]  -> Add     | [< 'MC "mult" >]  -> Mult
  | [< 'MC "sub" >]  -> Sub     | [< 'MC "div" >]   -> Div
  | [< 'MC "and" >]  -> And     | [< 'MC "or" >]    -> Or
  | [< 'MC "xor" >]  -> Xor     | [< 'MC "shl" >]   -> Shl
  | [< 'MC "shr" >]  -> Shr     | [< 'MC "slt" >]   -> Slt
  | [< 'MC "sle" >]  -> Sle     | [< 'MC "seq" >]   -> Seq

and test_inversé = parser
  | [< 'MC "sgt" >] -> Sle
  | [< 'MC "sge" >] -> Slt
  | [< 'MC "sne" >] -> Seq;;

let définition_d'étiquette = parser
  | [< 'Ident nom_étiq; 'MC ":" >] -> poser_étiquette nom_étiq;;

let rec instruction_étiq = parser
  | [< _ = définition_d'étiquette; _ = instruction_étiq >] -> ()
  | [< _ = instruction >] -> ();;

let rec suite_d'instructions = parser
  | [< _ = instruction_étiq; flux >] -> suite_d'instructions flux
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
