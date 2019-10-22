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

let calc () =
  try
    while true do
      let result = Calc.parse () in
      print_int result; print_newline (); flush stdout
    done
  with _ -> ();;

if !Sys.interactive then () else begin
  calc ();
  exit 0
end;;
