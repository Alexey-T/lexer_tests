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

let move start destination =
    print_endline ("I move a discus from " ^ start ^ " to " ^ destination);;

let rec hanoi height start temp destination =
    if height > 0 then
     begin
       hanoi (height - 1) start destination temp;
       move start destination;
       hanoi (height - 1) temp start destination
     end;;

let game height = hanoi height "A" "B" "C";;

if !Sys.interactive then () else begin
   let l = Array.length Sys.argv in
   if l <= 1 then begin
     prerr_endline "Usage: hanoi <number of discusses>";
     exit 2 end;
   game (int_of_string (Sys.argv.(1)));
   exit 0
end;;
