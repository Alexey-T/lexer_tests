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

(* Reverses the lines contained into a file.
   Simple recursive version; reads the data from stdin. *)
let rec reverse_input ic =
  try
    let l = input_line ic in
    reverse_input ic;
    print_endline l
  with
  | End_of_file -> ();;

if !Sys.interactive then () else reverse_input stdin;;
