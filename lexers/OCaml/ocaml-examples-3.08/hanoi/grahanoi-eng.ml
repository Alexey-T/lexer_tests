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

remember_mode false;;
auto_synchronize true;;

type discus = {
  mutable x : int;
  mutable y : int;
  w : int;
  h : int;
  bg : image;
  fg : image;
};;

type pin = {
  mutable summit : int;
  discus : discus option array;
  xt : int;
};;

let wood_color = black;;
let text_color = black;;

let pin_width = (size_x () / 5);;

let wood_width = 1 + 2 * 5;;

let half_pin_width = (pin_width - wood_width) / 2;;

let text_size_x s = let x, _ = text_size s in x;;
let text_size_y s = let _, y = text_size s in y;;

let pin_height, wood_height, baseline, vtab =
    let y = text_size_y "Graphics" in
    let vtab i = i * y in
    let pin_height = size_y () - 8 * y in
    pin_height, pin_height, vtab 5, vtab;;

let center i =
    let eps = size_x () / 10 in
    let h = half_pin_width in
    eps +  h + (i - 1) * (eps + 2 * h);;

let empty_pin i nb_discus =
    {summit = -1;
     discus = Array.make nb_discus None;
     xt = center i;
    };;

let make_color i =
    let colors = [| black; red; green; blue; yellow; cyan; magenta |] in
    colors.(i mod 7);;

let draw_wood x y =
    let x = x - ((wood_width - 1) / 2) in
    set_color wood_color;
    fill_rect x y wood_width pin_height;;

let make_discus pin nb_discus i =
    let inc = half_pin_width / nb_discus in
    let h =
     let h1 = pin_height / (nb_discus + 1) in
     min h1 (3 * wood_width) in
    let r0 = h / 2 in
    let wr =
      let inc = half_pin_width / nb_discus in
      let hwr = (nb_discus - i) * inc in
      2 * hwr in
    let w = wr + 2 * r0 in
    let cur_bg = get_image 0 0 w (wood_height) in
    draw_wood (w / 2) 0;
    let bg = get_image 0 0 w h in
    let c = make_color i in
    set_color c;
    let x0 = r0 in
    fill_rect x0 0 wr h;
    fill_circle x0 r0 r0;
    fill_circle (x0 + wr) r0 r0;
    let fg = get_image 0 0 w h in
    let x = pin.xt - w / 2 in
    let y = baseline + i * h in
    let discus = { x = x; y = y; w = w; h = h; bg = bg; fg = fg} in
    draw_image cur_bg 0 0;
    discus;;

let full_pin i nb_discus =
    let t = empty_pin i nb_discus in
    for i = 0 to nb_discus - 1 do
     t.discus.(i) <- Some (make_discus t nb_discus i)
    done;
    t.summit <- nb_discus - 1;
    t;;

let pop_discus pin =
    let s = pin.summit in
    let discus =
     match pin.discus.(s) with
     | None -> assert false
     | Some d -> d in
    draw_image discus.bg discus.x discus.y;
    pin.discus.(s) <- None;
    pin.summit <- s - 1;
    discus;;

let push_discus pin discus =
    pin.summit <- pin.summit + 1;
    let s = pin.summit in
    let x = pin.xt - (discus.w / 2) in
    let y =
      if s = 0 then baseline else
      match pin.discus.(s - 1) with
      | None -> assert false
      | Some d -> d.y + discus.h in
    discus.x <- x;
    discus.y <- y;
    draw_image discus.fg discus.x discus.y;
    pin.discus.(pin.summit) <- Some discus;;

let move (start_name, start) (destination_name, destination) =
    let discus = pop_discus start in
    push_discus destination discus;;

let draw_pin t =
    draw_wood t.xt baseline;
    let discus = t.discus in
    for i = t.summit downto 0 do
      match discus.(i) with
      | None -> ()
      | Some d -> draw_image d.fg d.x d.y;
    done;;

let center_text s x y =
    let trans = text_size_x s / 2 in
    moveto (x - trans) y;
    draw_string s;;

let print_game title
    (name_left, left) (name_midle, midle) (name_right, right) =
    let baseline = vtab 1 in
    set_color text_color;
    center_text name_left left.xt baseline;
    center_text name_midle midle.xt baseline;
    center_text name_right right.xt baseline;
    center_text title midle.xt (vtab 3);
    draw_pin left;
    draw_pin midle;
    draw_pin right;;

let wait () =
    print_string "Press return to continue"; print_newline ();
    ignore (read_line ());;

let rec hanoi height start temp destination =
    if height > 0 then
     begin
       hanoi (height - 1) start destination temp;
       wait ();
       Printf.printf "Movement from %s to %s\n" (fst start) (fst destination);
       move start destination;
       hanoi (height - 1) temp start destination
     end;;

let game nb_discus =
    clear_graph ();
    let left = ("A", full_pin 1 nb_discus)
    and midle = ("B", empty_pin 2 nb_discus)
    and right = ("C", empty_pin 3 nb_discus) in
    print_game "Lucas productions present" left midle right;
    hanoi nb_discus left midle right;;

if !Sys.interactive then () else begin
   let l = Array.length Sys.argv in
   if l <= 1 then begin
     prerr_endline "Usage: hanoi <number of discusses>";
     exit 2 end;
   game (int_of_string (Sys.argv.(1)));
   wait ();
   exit 0
end;;
