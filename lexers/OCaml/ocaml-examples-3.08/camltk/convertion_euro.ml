(***********************************************************************)
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

open Camltk;;

let synchronise_zones source dest taux_source taux_dest =
  function infos ->
    try
      let montant_source = float_of_string (Entry.get source) in
      let montant_dest =
        montant_source *. !taux_source /. !taux_dest in
      Entry.delete_range dest (At 0) End;
      Entry.insert dest (At 0)
                         (Printf.sprintf "%.2f" montant_dest)
    with Failure _ ->
      Entry.delete_range dest (At 0) End;
      Entry.insert dest (At 0) "erreur";;

let convertion_en_francs () =
  let fp = openTk () in

  let ligne1 = Frame.create fp []
  and ligne2 = Frame.create fp [] in

  let �tiq1 = Label.create ligne1 [Text "Francs:"]
  and entr�e1 = Entry.create ligne1 [TextWidth 10; Relief Sunken]

  and �tiq2 = Label.create ligne2 [Text "Euros:"]
  and entr�e2 = Entry.create ligne2 [TextWidth 10; Relief Sunken] in

  let quit = Button.create fp [Text "Quit"; Command closeTk] in

  let taux1 = ref 1.0     (* francs pour 1 franc *)
  and taux2 = ref 6.55957 (* francs pour 1 euro *) in

  bind entr�e1 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e1 entr�e2 taux1 taux2));
  bind entr�e2 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e2 entr�e1 taux2 taux1));
 
  pack [�tiq1] [Side Side_Left]; pack [entr�e1] [Side Side_Right];
  pack [�tiq2] [Side Side_Left]; pack [entr�e2] [Side Side_Right];
  pack [ligne1; ligne2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 
  mainLoop ();;

if !Sys.interactive then () else begin convertion_en_francs (); exit 0 end;;
