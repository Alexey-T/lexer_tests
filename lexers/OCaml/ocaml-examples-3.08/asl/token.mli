(* $Id: token.mli,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

type token_type =
   | IDENT of string | INT of int | OP of string
   | BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
   | THEN
;;

val next_token : char Stream.t -> token_type;;
val reset_lexer : char Stream.t -> unit;;
val token_name : token_type -> string;;
