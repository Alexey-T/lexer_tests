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

let round x =
  if x >= 0.0
  then int_of_float (x +. 0.5)
  else - (int_of_float (-. x +. 0.5));;

type �tat =
   { mutable x : float; mutable y : float;
     mutable vis�e : float; mutable lev� : bool };;

let crayon = { x = 0.0; y = 0.0; vis�e = 0.0; lev� = false };;

let fixe_crayon b = crayon.lev� <- b;;

let pi_sur_180 =
    let pi = 4.0 *. (atan 1.0) in pi /. 180.0;;

let tourne angle =
    crayon.vis�e <- (crayon.vis�e +. angle *. pi_sur_180);;

let avance d =
    let dx = d *. cos (crayon.vis�e)
    and dy = d *. sin (crayon.vis�e) in
    crayon.x <- crayon.x +. dx;
    crayon.y <- crayon.y +. dy;
    if crayon.lev�
    then moveto (round crayon.x) (round crayon.y)
    else lineto (round crayon.x) (round crayon.y);;

let couleur_du_trac� = foreground;;
let couleur_du_fond = background;;

let z�ro_x = float_of_int ((size_x ()) / 2);;
let z�ro_y = float_of_int ((size_y ()) / 2);;

let vide_�cran () =
    clear_graph ();
    set_color couleur_du_trac�;
    crayon.x <- z�ro_x;
    crayon.y <- z�ro_y;
    crayon.vis�e <- 0.0;
    crayon.lev� <- false;
    moveto (round crayon.x) (round crayon.y);;
