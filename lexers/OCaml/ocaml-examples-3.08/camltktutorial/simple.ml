(*
Q: On choisit de configurer le bouton *avant* de l'afficher.

Q: On veut effectivement afficher.
*)


(* Programme simple.
open Tk;;
let topwindow = openTk();;
let b1 = Button.create topwindow [Text "hello world!"];;
let b2 = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = pack [b1; b2] [];;
let _ = mainLoop();;

(* Programme simple. *)
open Tk;;
let c = ref 0;;
let topwindow = openTk();;
let bclick = Button.create topwindow [Text "hello world!"];;
let bcount = Button.create topwindow [Text "hello world!"];;
let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = pack [bclick; bcount; bquit] [];;
let _ = mainLoop();;

(* Programme simple. *)
open Tk;;
let c = ref 0;;
let incr_counter() = incr c;;
let display_counter b = Button.configure b [Text(string_of_int !c)];;
let topwindow = openTk();;
let bcount = Button.create topwindow [Text "never clicked"];;
let bclick = Button.create topwindow
    [Text "hello world!"; Command(fun () -> incr_counter(); display_counter bcount)];;
let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = pack [bclick; bcount; bquit] [];;
let _ = mainLoop();;

(* Programme simple. *)
open Tk;;

let incr_counter, display_counter, read_c =
   let c = ref 0 in
   (fun () -> incr c),
   (fun b -> Button.configure b [Text(string_of_int !c)]),
   (fun() -> !c);;

let topwindow = openTk();;
let bcount = Button.create topwindow [Text "never clicked"];;
let bclick = Button.create topwindow
    [Text "hello world!"; Command(fun () -> incr_counter(); display_counter bcount)];;
let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = pack [bclick; bcount; bquit] [];;
let _ = mainLoop();;

(* Programme simple. *)
open Tk;;

let incr_counter, display_counter =
  let c = ref 0 in
  (fun () -> incr c),
  (fun b -> Button.configure b [Text(string_of_int !c)]);;

let topwindow = openTk();;
let bcount = Button.create topwindow [Text "never clicked"];;
let bclick = Button.create topwindow
    [Text "hello world!"; Command(fun () -> incr_counter(); display_counter bcount)];;
let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = pack [bclick; bcount; bquit] [];;
let _ = mainLoop();;
*)

(* Programme simple. *)
open Tk;;

let incr_counter, display_counter =
  let c = ref 0 in
  (fun () -> incr c),
  (fun b -> Button.configure b [Text(string_of_int !c)]);;

let topwindow = openTk();;
let label = Label.create topwindow [Text "un petit label"]
let bcount = Button.create topwindow [Text "never clicked"];;
let bclick = Button.create topwindow
    [Text "hello world!"; Command(fun () -> incr_counter(); display_counter bcount)];;
let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;
let _ = Button.configure bquit
    [Background(NamedColor "red"); Foreground(NamedColor "white")];;
let _ = pack [label] [];;
let _ = pack [bclick; bcount] [Side Side_Top];;
let _ = pack [bquit] [Side Side_Top];;
let _ = mainLoop();;

(*
Q: "Wait a minute": qu'est ce qui se passe effectivement ? Tk a "pris la main"
    du programme ? Autrement dit, si je "termine" ma session Tk, je continue le
    programme OCaml "après" le mainLoop comme si de rien était ?

Q: Ok, je comprends. Et si je "tue" l'application graphique en fermant
    "brutalement" la fenêtre ?

Q: Ok, je comprends. J'ai hâte de voir ça!

Q: Joli, je vois maintenant comment rajouter le deuxième bouton à condition de
    me donner la commande à lui affecter pour "quitter" la fenêtre ?

Q: Ok, maintenant je veux un "état" (et ne me parler pas d'objets!): prenons un
    exemple simple; je souhaite rajouter un compteur qui sera affiché (dans un
    troisième bouton ?) et qui sera incrémenté à chaque fois qu'on appuye sur
    le boutton "hello...".

Q: Oui, oui: local au bouton svp!

Q: C'est brutal, allons-y, on verra si on peut faire mieux après

Q: Peut-on contrôler l'endroit où s'ouvre ma fenêtre principale (au lieu de
   s'ouvrir toujours en bas à gauche) ?

Q: On avait évoqué la "brutalité"; peut-on être plus "cultivé" ?

Q: Je voudrais encore afficher un texte (ici un texte fixe, mais ce pourrait
   être le contenu d'un fichier texte), en coloriant un mot, du genre:
   "J'aime OCaml et ses outils graphiques", en mettant OCaml en rouge.

*)


