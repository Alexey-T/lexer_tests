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
    programme OCaml "apr�s" le mainLoop comme si de rien �tait ?

Q: Ok, je comprends. Et si je "tue" l'application graphique en fermant
    "brutalement" la fen�tre ?

Q: Ok, je comprends. J'ai h�te de voir �a!

Q: Joli, je vois maintenant comment rajouter le deuxi�me bouton � condition de
    me donner la commande � lui affecter pour "quitter" la fen�tre ?

Q: Ok, maintenant je veux un "�tat" (et ne me parler pas d'objets!): prenons un
    exemple simple; je souhaite rajouter un compteur qui sera affich� (dans un
    troisi�me bouton ?) et qui sera incr�ment� � chaque fois qu'on appuye sur
    le boutton "hello...".

Q: Oui, oui: local au bouton svp!

Q: C'est brutal, allons-y, on verra si on peut faire mieux apr�s

Q: Peut-on contr�ler l'endroit o� s'ouvre ma fen�tre principale (au lieu de
   s'ouvrir toujours en bas � gauche) ?

Q: On avait �voqu� la "brutalit�"; peut-on �tre plus "cultiv�" ?

Q: Je voudrais encore afficher un texte (ici un texte fixe, mais ce pourrait
   �tre le contenu d'un fichier texte), en coloriant un mot, du genre:
   "J'aime OCaml et ses outils graphiques", en mettant OCaml en rouge.

*)


