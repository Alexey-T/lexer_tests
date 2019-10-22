(* $Id: parser.mli,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

open Token;;

type asl =
   | Const of int
   | Var of int
   | Cond of asl * asl * asl
   | App of asl * asl
   | Abs of string * asl

and top_asl = Decl of string * asl;;

exception Unbound of string;;

val init_env : string list;;
val global_env : string list ref;;

val top : token_type Stream.t -> top_asl;;
val expr : token_type Stream.t -> string list -> asl;;
val expr0 : token_type Stream.t -> string list -> asl;;

val print_top : top_asl -> string Stream.t;;
val print_expr : asl -> string Stream.t;;

