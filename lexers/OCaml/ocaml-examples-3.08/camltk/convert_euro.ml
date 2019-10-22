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

let synchronise_zones src dst src_rate dst_rate =
  function infos ->
    try
      let amount_src = float_of_string (Entry.get src) in
      let amount_dst =
        amount_src *. !src_rate /. !dst_rate in
      Entry.delete_range dst (At 0) End;
      Entry.insert dst (At 0)
        (Printf.sprintf "%.2f" amount_dst)
    with Failure _ ->
      Entry.delete_range dst (At 0) End;
      Entry.insert dst (At 0) "error";;

let convert_in_francs () =
  let fp = openTk () in

  let line1 = Frame.create fp []
  and line2 = Frame.create fp [] in

  let lab1 = Label.create line1 [Text "Francs:"]
  and entry1 = Entry.create line1 [TextWidth 10; Relief Sunken]

  and lab2 = Label.create line2 [Text "Euros:"]
  and entry2 = Entry.create line2 [TextWidth 10; Relief Sunken] in

  let quit = Button.create fp [Text "Quit"; Command closeTk] in

  let taux1 = ref 1.0     (* francs pour 1 franc *)
  and taux2 = ref 6.55957 (* francs pour 1 euro *) in

  bind entry1 [[], KeyRelease]
       (BindSet([], synchronise_zones entry1 entry2 taux1 taux2));
  bind entry2 [[], KeyRelease]
       (BindSet([], synchronise_zones entry2 entry1 taux2 taux1));
 
  pack [lab1] [Side Side_Left]; pack [entry1] [Side Side_Right];
  pack [lab2] [Side Side_Left]; pack [entry2] [Side Side_Right];
  pack [line1; line2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 
  mainLoop ();;

if !Sys.interactive then () else begin convert_in_francs (); exit 0 end;;
