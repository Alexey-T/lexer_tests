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
(*
- Records must be defined in Caml.
  Field contents cannot be modified unless the field has been declared mutable. 

- If r is a record with field f then r.f denotes the contents of field f.

- If r is a record with mutable field f then r.f <- v writes v in field f of r.
*)
type counts = {
 mutable chars : int;
 mutable lines : int;
 mutable words : int;
};;

let wc_count = {chars = 0; lines = 0; words = 0}
and wc_count_total = {chars = 0; lines = 0; words = 0};;

let reset_wc_count () =
 wc_count.chars <- 0; wc_count.lines <- 0; wc_count.words <- 0;;

let cumulate_wc_count () =
 wc_count_total.chars <- wc_count_total.chars + wc_count.chars;
 wc_count_total.lines <- wc_count_total.lines + wc_count.lines;
 wc_count_total.words <- wc_count_total.words + wc_count.words;;

let rec counter ic iw =
 let c = input_char ic in
 wc_count.chars <- wc_count.chars + 1;
 match c with
 | '\n' ->
    wc_count.lines <- wc_count.lines + 1;
    counter ic false
 | ' ' | '\t' ->
    counter ic false
 | _ ->
    if not iw then wc_count.words <- wc_count.words + 1 else ();
    counter ic true;;

let count_channel ic =
 reset_wc_count ();
 try counter ic false with
 | End_of_file -> cumulate_wc_count (); close_in ic;;

(*
- The Printf.printf function is analoguous to the C printf function.
*)
let print_wc l w c f =
 Printf.printf "%10d%10d%10d %s\n" l w c f;;

let print_wc_file f =
 print_wc wc_count.lines wc_count.words wc_count.chars f
;;

let print_wc_total () =
 print_wc
  wc_count_total.lines wc_count_total.words wc_count_total.chars "total"
;;

let count_file file_name =
 try
  count_channel (open_in file_name);
  print_wc_file file_name
 with Sys_error s -> print_string s; print_newline (); exit 2
;;

let main () =
 let args = Sys.argv in
 let nb_files = Array.length args - 1 in
 for i = 1 to nb_files do
  count_file args.(i)
 done;
 if nb_files > 1 then print_wc_total ();
 exit 0;;

if !Sys.interactive then () else main ();;
