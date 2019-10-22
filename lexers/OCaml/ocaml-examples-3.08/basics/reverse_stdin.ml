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
   Simple recursive version; reads the data from stdin.
   Try for instance
   reverse_stdin reverse_stdin.ml
*)
let reverse_stdin () =
 let i = ref 0 in
 let nlines = ref 100 in
 let lines = ref (Array.make !nlines "") in
 let ic = stdin in

 let rec loop j =
   let line = input_line ic in
   if j >= !nlines then begin
      nlines := 2 * !nlines;
      lines := Array.append !lines !lines
   end;
   !lines.(j) <- line;
   incr i;
   loop (j + 1) in

 try loop 0 with
 | End_of_file ->
     for j = !i - 1 downto 0 do print_endline !lines.(j) done
;;

if !Sys.interactive then () else reverse_stdin ();;
