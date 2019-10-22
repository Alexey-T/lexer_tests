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
type registre = int;;

type opérande =
   | Reg of registre
   | Imm of int;;

type instruction =
   | Op of opération * registre * opérande * registre
   | Jmp of opérande * registre
   | Braz of registre * int
   | Branz of registre * int
   | Scall of int
   | Stop

and opération =
  | Load | Store | Add | Mult | Sub | Div
  | And | Or | Xor | Shl | Shr
  | Slt | Sle | Seq;;

let nombre_de_registres = 32
and sp = 30
and ra = 31
and taille_du_mot = 4;;
