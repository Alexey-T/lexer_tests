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
   | MC of string
   | Ident of string
   | Entier of int;;

let string_of_lexème = function
  | MC s -> "MC " ^ s
  | Ident id -> "Ident " ^ id
  | Entier i -> "Entier " ^ string_of_int i;;

let rec lire_entier accumulateur = parser
  | [< ' ('0' .. '9' as c); flux >] ->
      lire_entier (10 * accumulateur + int_of_char c - 48) flux
  | [< >] ->
      accumulateur;;

let tampon = String.make 10 '-';;

let rec lire_mot position = parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | 
          'é' | 'à' | 'è' | 'ù' | 'â' | 'ê' | 'î' | 'ô' | 'û' |
          'ë' | 'ï' | 'ü' | 'ç' | 
          'É' | 'À' | 'È' | 'Ù' | 'Â' | 'Ê' | 'Î' | 'Ô' | 'Û' |
          'Ï' | 'Ü' | 'Ç' as c); flux >] ->
      if position < String.length tampon then tampon.[position] <- c;
      lire_mot (position + 1) flux
  | [< >] ->
      String.sub tampon 0 (min position (String.length tampon));;

let rec lire_symbole position = parser
  | [< ' ('!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | 
          ';' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' as c); flux >] ->
      if position < String.length tampon then tampon.[position] <- c;
      lire_symbole (position + 1) flux
  | [< >] ->
      String.sub tampon 0 (min position (String.length tampon));;

let rec lire_commentaire = parser
  | [< ''\n' >] -> ()
  | [< 'c; flux >] -> lire_commentaire flux;;

let mc_ou_ident table_des_mots_clés ident =
    try Hashtbl.find table_des_mots_clés ident
    with Not_found -> Ident(ident);;

let mc_ou_erreur table_des_mots_clés caractère =
    let ident = String.make 1 caractère in
    try Hashtbl.find table_des_mots_clés ident
    with Not_found -> raise (Stream.Error ("Illegal character " ^ ident));;

let rec lire_lexème table = parser
  | [< ' (' ' | '\n' | '\r' | '\t'); flux >] ->
      lire_lexème table flux
  | [< ''#'; flux >] ->
      lire_commentaire flux; lire_lexème table flux
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | 
          'é' | 'à' | 'è' | 'ù' | 'â' | 'ê' | 'î' | 'ô' | 'û' |
          'ë' | 'ï' | 'ü' | 'ç' | 
          'É' | 'À' | 'È' | 'Ù' | 'Â' | 'Ê' | 'Î' | 'Ô' | 'Û' |
          'Ë' | 'Ï' | 'Ü' | 'Ç' as c); flux >] ->
      tampon.[0] <- c;
      mc_ou_ident table (lire_mot 1 flux)
  | [< ' ('!' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | ':' | ';' | 
          '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' as c); flux >] ->
      tampon.[0] <- c;
      mc_ou_ident table (lire_symbole 1 flux)
  | [< ' ('0' .. '9' as c); flux >] ->
      Entier(lire_entier (int_of_char c - 48) flux)
  | [< ''-'; flux >] ->
      begin parser
      | [< ' ('0' .. '9' as c) >] ->
          Entier(- (lire_entier (int_of_char c - 48) flux))
      | [< >] ->
          tampon.[0] <- '-';
          mc_ou_ident table (lire_symbole 1 flux)
      end flux
  | [< 'c >] ->
      mc_ou_erreur table c;;

let rec analyseur table flux =
    Stream.from
      (function n ->
        (parser
         | [< lexème = lire_lexème table >] -> Some lexème
         | [< >] -> raise Stream.Failure) flux);;

let construire_analyseur mots_clés =
    let table_des_mots_clés = Hashtbl.create 17 in
    List.iter
      (function mot -> Hashtbl.add table_des_mots_clés mot (MC mot))
      mots_clés;
    analyseur table_des_mots_clés;;
