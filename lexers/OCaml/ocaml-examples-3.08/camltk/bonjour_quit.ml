open Tk;;

let action_press () = print_string "Bonjour!"; print_newline ();;

let hello_quit () =
  let fen�tre_principale = openTk () in
  let bouton_press =
    Button.create fen�tre_principale
                   [Text "Pressez-moi"; Command action_press] in
  let bouton_quit =
    Button.create fen�tre_principale
      [Text "Quittez-moi"; Command closeTk] in
  pack [bouton_press; bouton_quit] [Side Side_Left];
  mainLoop ();;

if !Sys.interactive then () else begin hello_quit(); exit 0 end;;
