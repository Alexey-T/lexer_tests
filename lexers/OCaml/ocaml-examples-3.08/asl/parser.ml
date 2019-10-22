(* $Id: parser.ml,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

open Prel;;
open Asl;;
open Token;;

type asl =
   | Const of int
   | Var of int
   | Cond of asl * asl * asl
   | App of asl * asl
   | Abs of string * asl

and top_asl = Decl of string * asl;;

exception Unbound of string;;

let binding_depth s rho = 
  try Var(index s rho + 1)
  with Not_found -> raise (Unbound s);;

let init_env =  ["+"; "-"; "*"; "/"; "="];;

let global_env = ref init_env;;

let rec list p = parser
| [< x = p; l = list p >] -> x :: l
| [< >] -> []
;;

let option p = parser
| [< x = p >] -> Some x
| [< >] -> None
;;

(* parsing *)

let rec expr = parser
| [< 'BSLASH ; 'IDENT s ; 'DOT ; e = expr >] ->
    (function rho -> Abs(s, e(s :: rho)))
| [< e = expr0 ; l = list expr0 >] ->
    List.fold_left (fun e1 e2 rho -> App(e1 rho, e2 rho)) e l

and expr0 = parser
| [< 'INT n >] -> (function _ -> Const n)
| [< 'IDENT s >] -> binding_depth s
| [< 'OP s >] -> binding_depth s
| [< 'EQUAL >] -> binding_depth "="
| [< 'IF ; e1 = expr ; 'THEN ; e2 = expr ; 'ELSE ; e3 = expr ; 'FI >] ->
    (function rho -> Cond(e1 rho, e2 rho, e3 rho))
| [< 'LPAREN ; e = option expr ; 'RPAREN >] ->
    (match e with | Some e -> e | _ -> (fun _-> Const(-1)))
;;

let top = parser
| [< 'LET ; 'IDENT s ; 'EQUAL ; e = expr ; 'SEMICOL >] ->
    Decl(s, e !global_env)
| [< e = expr ; 'SEMICOL >] -> Decl("it", e !global_env)
| [< '_ >] -> raise Stream.Failure
;;

(* impression de l'arbre *)

let rec print_expr = function
| Abs(s, a) -> [< '"Abs (\""; 's; '"\", "; print_expr a; '")" >]
| App(e1, e2) -> [< '"App ("; print_expr e1; '", "; print_expr e2; '")" >]
| Const c -> [< '"Const "; 'string_of_int c >]
| Var v -> [< '"Var "; 'string_of_int v >]
| Cond(e1, e2, e3) -> [<
    '"Cond ("; print_expr e1; '", "; print_expr e2; '", ";
    print_expr e3; '")"
  >]
;;

let print_top = function
| Decl(s,a) -> [< '"Decl (\""; 's; '"\", "; print_expr a; '")" >]
;;
