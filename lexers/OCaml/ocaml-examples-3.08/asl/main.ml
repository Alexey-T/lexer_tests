(* $Id: main.ml,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

open Prel;;
open Asl;;
open Token;;
open Parser;;
open Semant;;
open Typing;;

let input_stream = ref stdin;;
let trace_parsing = ref false;;

let print_prompt () =
 print_newline (); print_string ">> "; flush stdout
;;

let read_fun =
 let bol = ref true in
 fun n ->
  if !bol then print_prompt ();
  let c = input_char !input_stream in
  if !input_stream != stdin then print_char c;
  bol := c == '\n';
  Some c
;;

let replace_decl (Decl(s, _)) sm sigma =
  begin try
    let i = index s !global_env in
    global_env := except_nth !global_env i;
    global_sem := except_nth !global_sem i;
    global_typing_env := except_nth !global_typing_env i
  with _ -> ()
  end;
  global_env := s :: !global_env;
  global_sem := sm :: !global_sem;
  global_typing_env := sigma :: !global_typing_env
;;

exception Break;;

let stream_skip strm =
 match strm with parser
 | [< '_ >] -> ();;

let go () =
  try
    let cstrm = Stream.from read_fun in
    let strm = Stream.from (fun _ -> Some (next_token cstrm)) in
    while true do
      try
        let ta = top strm in
        let (Decl(s,_)) = ta in
        if !trace_parsing then (
          print_string "   ";
          Stream.iter print_string (print_top ta); print_newline ()
        );
        let sigma = typing ta in
        print_string "   "; print_string s; print_string " : ";
        print_type_scheme sigma; print_newline ();
        let sm = semant_asl ta in
        print_string "   "; print_string s; print_string " = ";
        print_semval sm; print_newline ();
        replace_decl ta sm sigma
      with
      | Stream.Failure ->
          print_newline ();
          raise Break
      | Stream.Error s ->
          print_newline ();
          print_string "*** Syntax error. :";
          print_string s; print_newline ();
          reset_lexer cstrm; stream_skip strm
      | Unbound s ->
          print_newline ();
          print_string "*** Unbound ASL identifier: ";
          print_string s; print_newline ();
          reset_lexer cstrm
      | Illtyped ->
          print_newline ();
          print_string "*** Ill typed"; print_newline ()
      | Error s ->
          print_newline ();
          print_string "*** Error: "; print_string s; print_newline ();
          raise Break
      | Failure s ->
          print_newline ();
          print_string "*** Failed: "; print_string s; print_newline ()
    done
  with
  | Break ->
      ()
  | Failure s ->
      print_string "*** Failed: "; print_string s; print_newline ()
;;

global_env := "magic" :: !global_env;;
global_sem := (Funval (function x -> x)) :: !global_sem;;
global_typing_env :=
  Forall(
    [1; 2],
    Arrow
     (TypeVar {index = 1; value = Unknown},
      TypeVar {index = 2; value = Unknown})) :: !global_typing_env;;

