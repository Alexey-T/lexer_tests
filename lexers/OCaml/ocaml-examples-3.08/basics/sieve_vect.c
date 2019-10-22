/*(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Erathostene sieve, imperative version.
   A vector is initialized with integers.
   Then the vector is sieved. *)

*/

#include <stdio.h>

#define fixed_bound 5000000

long v[fixed_bound];

void sieve (long max) {
 long i;
 long j;
 long prime_count = 0;
 long prime;

 for (i = 0; i < max; i++) {
   v[i] = i + 1;
 };

 v[0] = 0;
 prime_count = 0;

 for (i = 0; i < max; i++) {
   if (v[i] != 0) {
     prime_count = prime_count + 1;
     prime = i + 1;
     for (j = i + prime; j < max; j = j + prime) {
       if (j < max) {
         v[j] = 0;
       };
     };
   };
 };
 /* for (i = 0; i < max; i++) {printf ("%d ", v[i]);};*/
 printf ("There are %d primes less than or equal to %d.\n", prime_count, max);
}

void main () {
  sieve (fixed_bound);
}
