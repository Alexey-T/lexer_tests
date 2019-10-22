(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(* Copyright 2001, 2004 Institut National de Recherche en Informatique *)
(* et en Automatique. All rights reserved. This file is distributed    *)
(* only by permission.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mytext.ml,v 1.2 2004/07/07 09:39:15 weis Exp $ *)

(* A text widget with kill and yank capabilities ``à la'' Emacs. *)

open Tk;;

let top = openTk ();;

let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)];;

let f = Frame.create top [];;
let text = Text.create f [];;
let scrollbar = Scrollbar.create f [];;

(* kill buffer *)
let kill_ring = ref [];;

let add_to_kill_ring s = kill_ring := s :: !kill_ring;;

let get_killed_text () =
 match !kill_ring with
 | [] -> ""
 | s :: l -> s;;

(* Note: for the text widgets, the insertion cursor is 
    not TextIndex (Insert, []),
    but TextIndex (Mark  "insert", []) *) 
let insertMark = TextIndex (Mark "insert", []);;
let eol_insertMark = TextIndex (Mark "insert", [LineEnd]);;

let kill () =
  let s = Text.get text insertMark eol_insertMark in
  add_to_kill_ring s;
  prerr_endline ("Killed: " ^ s);
  Text.delete text insertMark eol_insertMark;;

let yank () =
 let s = get_killed_text () in
 Text.insert text insertMark s [];
 prerr_endline ("Yanked: " ^ s);;

let yank_more () =
  let ring = !kill_ring in
  let more = ref ring in
  let rec get_killed_more () =
    match !more with
    | [] -> more := ring; get_killed_more ()
    | s :: l -> more := l; s in
  let insert_killed_more () =
    let s = get_killed_more () in
    prerr_endline ("Yanked more: " ^ s);
    Text.insert text insertMark s [] in
  insert_killed_more ();
  bind text [[Alt], KeyPressDetail "y"]
     (BindSet ([], fun _ -> insert_killed_more ()));;

let main () =
  scroll_link scrollbar text;

  pack [text; scrollbar][Side Side_Left; Fill Fill_Y];
  pack [f][];

  bind text [[Control], KeyPressDetail "y"]
     (BindSet ([], fun _ -> yank ()));

  bind text [[Alt], KeyPressDetail "y"]
     (BindSet ([], fun _ -> yank_more () ));

  bind text [[Control], KeyPressDetail "k"]
     (BindSet ([], fun _ -> kill () ));

  bind text [[Control], KeyPressDetail "c"]
     (BindSet ([], fun _ -> exit 0 ));

  mainLoop ();;

main ();;

