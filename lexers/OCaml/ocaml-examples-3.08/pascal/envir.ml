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

type 'a env =
  { vars : (string * 'a) list;
    procs : (string * d�cl_proc) list;
    foncs : (string * d�cl_fonc) list };;

exception Pas_trouv� of string;;

let environnement_initial p f =
  { vars = []; procs = p; foncs = f };;

let ajoute_variable nom info env =
  { vars = (nom,info) :: env.vars; procs = env.procs; foncs = env.foncs };;

let cherche nom liste =
  try List.assoc nom liste with Not_found -> raise(Pas_trouv� nom);;

let cherche_variable nom env = cherche nom env.vars
and cherche_fonction nom env = cherche nom env.foncs
and cherche_proc�dure nom env = cherche nom env.procs;;
