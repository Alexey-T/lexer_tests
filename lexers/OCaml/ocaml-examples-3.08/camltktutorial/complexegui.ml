open Tk;;

let parse_logic_message() =
  let s = input_line stdin in
  try
    ignore (int_of_string s); s
  with
    Failure _ -> "invalid server response";;

let bclick_cb b () =
  output_string stdout "bouton\n"; flush stdout;
  let s = parse_logic_message () in
  Button.configure b [Text s];;

let topwindow = openTk();;

let bcount = Button.create topwindow [Text "never clicked"];;

let bclick = Button.create topwindow
    [Text "hello world!"; Command(bclick_cb bcount)];;

let bquit = Button.create topwindow [Text "Quit"; Command closeTk];;

let gui_server () =
  Button.configure bquit
    [Background (NamedColor "red"); Foreground (NamedColor "white")];
  pack [bclick; bcount; bquit] [Side Side_Left];
  mainLoop();;
