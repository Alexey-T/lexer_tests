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
open Graphics;;

open_graph "";;

type état = {
  mutable direction : float;
  mutable x : float;
  mutable y : float
};;

let point = {direction = 0.0; x = 0.0 ; y =0.0};;

let tourne a = point.direction  <- point.direction +. a;;

let avance d =
  let dir = point.direction in
  point.x <- point.x +. d *. (cos dir);
  point.y <- point.y +. d *. (sin dir);
  lineto (int_of_float point.x) (int_of_float point.y);;

let pi = 4.0 *. atan 1.0;;

let pi_sur_3 = pi /. 3. ;;

let rec motif n d =
  match n with
  | 0 -> avance d
  | n ->
     let c = d /. 3. in
     motif (n - 1) c;
     tourne pi_sur_3;
     motif (n - 1) c;
     tourne (-2. *. pi_sur_3);
     motif (n - 1) c;
     tourne pi_sur_3;
     motif (n - 1) c;;

let flocon n d =
  for i = 1 to 3 do
    motif n d;
    tourne (-2. *. pi_sur_3)
  done;;

let colors = [| red; green; blue; cyan; magenta; yellow; black; |];;

let wait_key () =
  while not (key_pressed ()) do () done;
  while key_pressed () do ignore (read_key ()) done;;

let frac n d =
 let nb_cols = Array.length colors in
 for i = 0 to n - 1 do
  flocon i d;
  wait_key ();
  set_color colors.(i mod nb_cols)
 done;;


let main () =
 point.x <- float (size_x () / 3);
 point.y <- float (3 * size_y () / 4);
 moveto (int_of_float point.x) (int_of_float point.y);

 frac 7 300.;
 wait_key ();;

if not !Sys.interactive then begin
 main ();
 exit 0
end;;

