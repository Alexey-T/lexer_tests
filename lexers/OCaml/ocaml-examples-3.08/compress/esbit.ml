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
type tampon = { mutable value : int; mutable nbits : int };;

let tampon = { value = 0; nbits = 0 };;

let initialise () = tampon.value <- 0; tampon.nbits <- 0;;

let écrire_bit sortie bit =
  tampon.value <- tampon.value lor (bit lsl tampon.nbits);
  tampon.nbits <- tampon.nbits + 1;
  if tampon.nbits >= 8 then begin
    output_char sortie (char_of_int tampon.value);
    tampon.value <- 0;
    tampon.nbits <- 0
  end;;

let finir sortie =
  if tampon.nbits > 0 then
    output_char sortie (char_of_int tampon.value);;

let lire_bit entrée =
  if tampon.nbits <= 0 then begin
    tampon.value <- int_of_char(input_char entrée);
    tampon.nbits <- 8
  end;
  let res = tampon.value land 1 in
  tampon.value <- tampon.value lsr 1;
  tampon.nbits <- tampon.nbits - 1;
  res;;
