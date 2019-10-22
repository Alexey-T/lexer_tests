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

(* Erathostene sieve, imperative version.
   A vector is initialized with integers.
   Then the vector is sieved. *)

let fixed_bound = 5000000;;

let sieve max =

 let v = Array.init max (fun i -> i + 1) in
 for i = 0 to max - 1 do v.(i) <- i + 1 done;
 
 let prime_count = ref 0 in

 v.(0) <- 0;
 prime_count := 0;

 for i = 1 to max - 1 do
  if Array.unsafe_get v i <> 0 then begin
   prime_count := !prime_count + 1;
   let prime = i + 1 in
   let rec sieve j =
    if j < max then begin Array.unsafe_set v j 0; sieve (j + prime) end in
   sieve (i + prime)
  end
 done;
 Printf.printf
  "There are %d primes less than or equal to %d.\n" !prime_count max;;

let main () =
 let max =
   try int_of_string (Sys.argv.(1))
   with _ -> fixed_bound in
 sieve max;;

main ();;
