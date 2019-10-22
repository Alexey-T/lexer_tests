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

(* The Fibonacci function, once more. *)

(*
- Global or toplevel definitions have the syntax let ident = expression;;

- If the definition is recursive, you must write let rec instead of
   let.
*)

let rec fib n =
  if n < 2 then 1 else fib (n - 1) + fib (n - 2)
;;

(*  
- In the branches of conditionals, sequences must be enclosed into
  begin ... end. Hence, use
    if cond then begin e1; e2 end else begin e3; e4 end

- Some functions may fail to operate properly, in case of bad input.
  In those situations, the function normally stop the computation, by
  <EM>raising some exception</EM>. For instance int_of_string raises ``Failure
  "int_of_string"'' when called with a string that cannot be
  interpreted as a number.  You can check this situation, and continue
  the computation appropriately, using the try ... with
  ... construct. Hence, use
    try expr with Exc -> e
  to return e when the expression expr fails (raising exception Exc).
*)

let main () =
  if Array.length Sys.argv <> 2 then begin
    print_string "Usage: fib <number>";
    print_newline()
  end else begin
    try
      print_int (fib (int_of_string Sys.argv.(1)));
      print_newline ()
    with Failure "int_of_string" ->
      print_string "Bad integer constant";
      print_newline ()
  end
;;

(*
- !Sys.interactive tests if the program is called in an interactive
   system or not.
  If so, just do nothing (let the user call the fib function
   directly).
  Otherwise call the main procedure.
*)
if !Sys.interactive then () else main ();;
