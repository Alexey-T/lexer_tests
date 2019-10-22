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

let convertion () =
  let fp = openTk () in

  let ligne1 = Frame.create fp [] and ligne2 = Frame.create fp [] in
  let �tiq1 = Label.create ligne1 [Text "Francs:"]
  and entr�e1 = Entry.create ligne1 [TextWidth 10; Relief Sunken]
  and �tiq2 = Label.create ligne2 [Text "Euros:"]
  and entr�e2 = Entry.create ligne2 [TextWidth 10; Relief Sunken] in
  let taux1 = ref 1.0     (* francs pour 1 franc *)
  and taux2 = ref 6.55957 (* francs pour 1 euro *) in
  bind entr�e1 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e1 entr�e2 taux1 taux2));
  bind entr�e2 [[], KeyRelease]
       (BindSet([], synchronise_zones entr�e2 entr�e1 taux2 taux1));
  pack [�tiq1] [Side Side_Left]; pack [entr�e1] [Side Side_Right];
  pack [�tiq2] [Side Side_Left]; pack [entr�e2] [Side Side_Right];

  let barre_de_menus =
    Frame.create fp [Relief Raised; BorderWidth (Pixels 2)] in

  let bouton_source =
    Menubutton.create barre_de_menus
                       [Text "Source"; UnderlinedChar 0]
  and bouton_cible =
    Menubutton.create barre_de_menus
                       [Text "Cible"; UnderlinedChar 0] in

  let source = Menu.create bouton_source []
  and cible = Menu.create bouton_cible [] in

  Menubutton.configure bouton_source [Menu source];
  Menubutton.configure bouton_cible [Menu cible];
  pack [bouton_source; bouton_cible] [Side Side_Left];

  let liste_de_devises =
    [ "Dollars US", 5.9389;  "Dollars canadiens", 3.933046;
      "Euros", 6.55957;  "Francs", 1.0;  "Francs belges", 0.162531;
      "Francs suisses", 4.116;  "Lires", 0.00338617;  "Livres", 9.552;
      "Marks", 3.354;  "Pesetas", 0.0394061;  "Yens", 0.05011 ] in

  List.iter
    (function (nom, taux) ->
      Menu.add_command source
        [Label nom;
         Command(function () ->
                   Label.configure �tiq1 [Text(nom ^ ":")];
                   taux1 := taux;
                   synchronise_zones entr�e1 entr�e2 taux1 taux2 ())])
    liste_de_devises;

  List.iter
    (function (nom, taux) ->
      Menu.add_command cible
        [Label nom;
         Command(function () ->
                   Label.configure �tiq2 [Text(nom ^ ":")];
                   taux2 := taux;
                   synchronise_zones entr�e2 entr�e1 taux2 taux1 ())])
    liste_de_devises;

  let quit = Button.create fp [Text "Quit"; Command closeTk] in

  pack [barre_de_menus] [Side Side_Top; Fill Fill_X];
  pack [ligne1; ligne2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X]; 

  mainLoop ();;

if !Sys.interactive then () else begin convertion (); exit 0 end;;
