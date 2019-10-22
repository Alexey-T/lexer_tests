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

type disque = {
  mutable x : int;
  mutable y : int;
  w : int;
  h : int;
  bg : image;
  fg : image;
};;

type tige = {
  mutable sommet : int;
  disques : disque option array;
  xt : int;
};;

let couleur_bois = black;;
let couleur_texte = black;;

let largeur_tige = (size_x () / 5);;

let largeur_bois = 1 + 2 * 5;;

let demie_largeur_tige = (largeur_tige - largeur_bois) / 2;;

let text_size_x s = let x, _ = text_size s in x;;
let text_size_y s = let _, y = text_size s in y;;

let hauteur_tige, hauteur_bois, base_tiges, vtab =
  let y = text_size_y "Graphics" in
  let vtab i = i * y in
  let hauteur_tige = size_y () - 8 * y in
  hauteur_tige, hauteur_tige, vtab 5, vtab;;

let centre i =
 let eps = size_x () / 10 in
 let h = demie_largeur_tige in
 eps +  h + (i - 1) * (eps + 2 * h);;

let tige_vide i nombre_de_disques =
  {sommet = -1;
   disques = Array.make nombre_de_disques None;
   xt = centre i;
  };;

let make_couleur i =
  let couleurs = [| black; red; green; blue; yellow; cyan; magenta |] in
  couleurs.(i mod 7);;

let trace_bois x y =
 let x = x - ((largeur_bois - 1) / 2) in
 set_color couleur_bois;
 fill_rect x y largeur_bois hauteur_tige;;

let make_disque tige nombre_de_disques i =
  let inc = demie_largeur_tige / nombre_de_disques in
  let h =
   let h1 = hauteur_tige / (nombre_de_disques + 1) in
   min h1 (3 * largeur_bois) in
  let r0 = h / 2 in
  let wr =
    let inc = demie_largeur_tige / nombre_de_disques in
    let hwr = (nombre_de_disques - i) * inc in
    2 * hwr in
  let w = wr + 2 * r0 in
  let cur_bg = get_image 0 0 w (hauteur_bois) in
  trace_bois (w / 2) 0;
  let bg = get_image 0 0 w h in
  let c = make_couleur i in
  set_color c;
  let x0 = r0 in
  fill_rect x0 0 wr h;
  fill_circle x0 r0 r0;
  fill_circle (x0 + wr) r0 r0;
  let fg = get_image 0 0 w h in
  let x = tige.xt - w / 2 in
  let y = base_tiges + i * h in
  let disque = { x = x; y = y; w = w; h = h; bg = bg; fg = fg} in
  draw_image cur_bg 0 0;
  disque;;

let tige_pleine i nombre_de_disques =
  let t = tige_vide i nombre_de_disques in
  for i = 0 to nombre_de_disques - 1 do
   t.disques.(i) <- Some (make_disque t nombre_de_disques i)
  done;
  t.sommet <- nombre_de_disques - 1;
  t;;

let dépile_disque tige =
  let s = tige.sommet in
  let disque =
   match tige.disques.(s) with
   | None -> assert false
   | Some d -> d in
  draw_image disque.bg disque.x disque.y;
  tige.disques.(s) <- None;
  tige.sommet <- s - 1;
  disque;;

let empile_disque tige disque =
  tige.sommet <- tige.sommet + 1;
  let s = tige.sommet in
  let x = tige.xt - (disque.w / 2) in
  let y =
    if s = 0 then base_tiges else
    match tige.disques.(s - 1) with
    | None -> assert false
    | Some d -> d.y + disque.h in
  disque.x <- x;
  disque.y <- y;
  draw_image disque.fg disque.x disque.y;
  tige.disques.(tige.sommet) <- Some disque;;

let déplace (nom_départ, départ) (nom_destination, destination) =
  let disque = dépile_disque départ in
  empile_disque destination disque;;

let trace_tige t =
 trace_bois t.xt base_tiges;
 let disques = t.disques in
 for i = t.sommet downto 0 do
   match disques.(i) with
   | None -> ()
   | Some d -> draw_image d.fg d.x d.y;
 done;;

let centre_text s x y =
 let trans = text_size_x s / 2 in
 moveto (x - trans) y;
 draw_string s;;

let imprime_jeu title
    (nom_gauche, gauche) (nom_milieu, milieu) (nom_droite, droite) =
  let baseline = vtab 1 in
  set_color couleur_texte;
  centre_text nom_gauche gauche.xt baseline;
  centre_text nom_milieu milieu.xt baseline;
  centre_text nom_droite droite.xt baseline;
  centre_text title milieu.xt (vtab 3);
  trace_tige gauche;
  trace_tige milieu;
  trace_tige droite;;

let attend () =
  print_string "Appuyez sur return pour continuer"; print_newline ();
  ignore (read_line ());;

let rec hanoi hauteur départ intermédiaire destination =
    if hauteur > 0 then
     begin
       hanoi (hauteur - 1) départ destination intermédiaire;
       attend ();
       Printf.printf "Mouvement de %s à %s\n" (fst départ) (fst destination);
       déplace départ destination;
       hanoi (hauteur - 1) intermédiaire départ destination
     end;;

let jeu nombre_de_disques =
    clear_graph ();
    let gauche = ("A", tige_pleine 1 nombre_de_disques)
    and milieu = ("B", tige_vide 2 nombre_de_disques)
    and droite = ("C", tige_vide 3 nombre_de_disques) in
    imprime_jeu "Les productions Lucas présentent" gauche milieu droite;
    hanoi nombre_de_disques gauche milieu droite;;

if !Sys.interactive then () else begin
  let l = Array.length Sys.argv in
  if l <= 1 then begin
    prerr_endline "Usage: hanoi <nombre de disques>";
    exit 2 end;
  jeu (int_of_string (Sys.argv.(1)));
  attend ();
  exit 0
end;;
