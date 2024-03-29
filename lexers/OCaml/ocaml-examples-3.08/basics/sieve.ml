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
  Manipulation of lists.

  - [] is the empty list
  - :: is the infix operator that builds list cells.
    Hence 1 :: (2 :: []) is the list that contains 1 and 2 in this order.

*)
(* interval min max = [min; min+1; ...; max-1; max] *)

let rec interval min max =
  if min > max then [] else min :: interval (succ min) max
;;

(*

  Case analysis on list l is written
  match l with
  | [] -> ``nil case''
  | x :: tail -> ``non nil case,
                   with x (the head of l) and tail (the tail of l) available''

  Function can perform direct case analysis on their argument,
  if introduced by the function keyword. Hence,

    let f (x) =
      match x with
      | [] -> ...
      | ...

  can be abreviated as

    let f = function
      | [] -> ...
      | ...

*)

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

let rec filter p = function
  | []  -> []
  | a :: r -> if p a then a :: filter p r else filter p r
;;

(* Application: removing all numbers multiple of n from a list of integers *)

let remove_multiples_of n =
  filter (fun m -> m mod n <> 0)
;;

(* The sieve itself *)

let sieve max =
  let rec filter_again = function
  | [] -> []
  | n :: r as l ->
      if n * n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)
;;

let usage() =
  print_string "Usage: sieve <n>\n";
  exit 2
;;

(* The entry point *)

let main () =
  if Array.length Sys.argv <> 2 then usage () else
  begin
    try
      let n = int_of_string Sys.argv.(1) in
      List.iter (fun n -> print_int n; print_string " ") (sieve n);
      print_newline ()
    with Failure "int_of_string" ->
      print_string "Bad integer constant";
      print_newline ()
  end
;;

if !Sys.interactive then () else main ();;
