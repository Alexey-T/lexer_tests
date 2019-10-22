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

let convert () =
  let fp = openTk () in

  let line1 = Frame.create fp [] and line2 = Frame.create fp [] in
  let cur_src = Label.create line1 [Text "Francs:"]
  and src = Entry.create line1 [TextWidth 10; Relief Sunken]
  and cur_dst = Label.create line2 [Text "Euros:"]
  and dst = Entry.create line2 [TextWidth 10; Relief Sunken] in
  let rate_src = ref 1.0
  and rate_dst = ref 6.55957 in
  bind src [[], KeyRelease]
    (BindSet([], synchronise_zones src dst rate_src rate_dst));
  bind dst [[], KeyRelease]
    (BindSet([], synchronise_zones dst src rate_dst rate_src));
  pack [cur_src] [Side Side_Left]; pack [src] [Side Side_Right];
  pack [cur_dst] [Side Side_Left]; pack [dst] [Side Side_Right];

  let menu_bar =
    Frame.create fp [Relief Raised; BorderWidth (Pixels 2)] in

  let button_src =
    Menubutton.create menu_bar
      [Text "Source"; UnderlinedChar 0]
  and button_dst =
    Menubutton.create menu_bar
      [Text "Destination"; UnderlinedChar 0] in

  let menu_src = Menu.create button_src []
  and menu_dst = Menu.create button_dst [] in

  Menubutton.configure button_src [Menu menu_src];
  Menubutton.configure button_dst [Menu menu_dst];
  pack [button_src; button_dst] [Side Side_Left];

  let currency_list =
    [ "US Dollars", 5.9389; "Canadian Dollars", 3.933046;
      "Euros", 6.55957; "Francs", 1.0; "Belgium Francs", 0.162531;
      "Switzerland Francs", 4.116; "Lires", 0.00338617; "GB Pounds", 9.552;
      "Marks", 3.354; "Pesetas", 0.0394061; "Yens", 0.05011 ] in

  List.iter
    (function (name, rate) ->
      Menu.add_command menu_src
        [Label name;
         Command 
           (function () ->
             Label.configure cur_src [Text(name ^ ":")];
             rate_src := rate;
             synchronise_zones src dst rate_src rate_dst ())])
    currency_list;

  List.iter
    (function (name, rate) ->
      Menu.add_command menu_dst
        [Label name;
         Command(function () ->
                   Label.configure cur_dst [Text(name ^ ":")];
                   rate_dst := rate;
                   synchronise_zones dst src rate_dst rate_src ())])
    currency_list;

  let quit = Button.create fp [Text "Quit"; Command closeTk] in

  pack [menu_bar] [Side Side_Top; Fill Fill_X];
  pack [line1; line2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 

  mainLoop ();;

if !Sys.interactive then () else begin convert (); exit 0 end;;
