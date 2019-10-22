(* $Id: token.ml,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

type token_type =
   | IDENT of string | INT of int | OP of string
   | BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
   | THEN
;;

let id x = x;;

let keywords =
  let t = Hashtbl.create 13 in
  Hashtbl.add t "else" ELSE;
  Hashtbl.add t "fi" FI;
  Hashtbl.add t "if" IF;
  Hashtbl.add t "let" LET;
  Hashtbl.add t "then" THEN;
  t
;;

let buff = String.create 2000;;

(***
let rec ident len = function
  [<
    '(`a`..`z` | `A` .. `Z` | `0` .. `9` | `_` | `'`) as c;
    (set_nth_char buff len c; ident(succ len)) i
  >] -> i
| [< >] ->
    let str = sub_string buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;
***)

let rec ident len = parser
| [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c; s >] ->
    buff.[len] <- c; ident (succ len) s
| [< >] ->
    let str = String.sub buff 0 len in
    (try Hashtbl.find keywords str with _ -> IDENT str)
;;

let rec number n = parser
| [< ''0' .. '9' as d; s >] ->
    number(10 * n + int_of_char d - int_of_char '0') s
| [< >] -> n
;;

let rec next_token = parser
| [< ' ('a' .. 'z' | 'A' .. 'Z') as c; s >] ->
    buff.[0] <- c; ident 1 s
| [< ''0' .. '9' as d; s >] ->
    INT(number (int_of_char d-int_of_char '0') s)
| [< '' ' | '\n' | '\t'; s >] -> next_token s
| [< ''+' | '-' | '*' | '/' as c >] -> OP (String.make 1 c)
| [< ''.' >] -> DOT
| [< ''=' >] -> EQUAL
| [< ''\\' >] -> BSLASH
| [< '';' >] -> SEMICOL
| [< ''(' >] -> LPAREN
| [< '')' >] -> RPAREN
| [< 'x >] -> failwith ("Bad char: " ^ String.make 1 x)
;;

let rec reset_lexer = parser
| [< ''\n' >] -> ()
| [< '_; _ = reset_lexer >] -> ()
| [< >] -> ()
;;

let token_name = function
| IDENT _ -> "IDENT" | INT _ -> "INT" | OP _ -> "OP"
| BSLASH -> "\\" | DOT -> "." | ELSE -> "else" | EQUAL -> "="
| FI -> "fi" | IF -> "if" | LET -> "let" | LPAREN -> "("
| RPAREN -> ")" | SEMICOL -> ";" | THEN -> "then" 
;;
