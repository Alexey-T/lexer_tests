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

open Camltk;;

let rgb () =
  let fenêtre_principale = openTk () in

  let créer_glissière nom =
    Scale.create fenêtre_principale
      [Label nom; From 0.0; To 255.0;
       Length(Centimeters 10.0); Orient Horizontal] in

  let rouge = créer_glissière "Rouge"
  and vert = créer_glissière "Vert"
  and bleu = créer_glissière "Bleu"
  and échantillon =
    Frame.create fenêtre_principale
      [Height (Centimeters 1.5); Width (Centimeters 6.0)]
  and quitter =
    Button.create fenêtre_principale
      [Text "Quitter"; Command closeTk] in

  let rafraîchir_couleur x =
    let r = int_of_float (Scale.get rouge)
    and v = int_of_float (Scale.get vert)
    and b = int_of_float (Scale.get bleu) in
    let couleur = Printf.sprintf "#%02x%02x%02x" r v b in
    Frame.configure échantillon [Background (NamedColor couleur)] in

  Scale.configure rouge [ScaleCommand rafraîchir_couleur];
  Scale.configure vert [ScaleCommand rafraîchir_couleur];
  Scale.configure bleu [ScaleCommand rafraîchir_couleur];
  pack [rouge; vert; bleu] [Side Side_Top];
  pack [quitter] [Side Side_Bottom];
  pack [échantillon] [Side Side_Bottom; PadY (Millimeters 2.0)];
  mainLoop ();;

if !Sys.interactive then () else begin rgb (); exit 0 end;;
