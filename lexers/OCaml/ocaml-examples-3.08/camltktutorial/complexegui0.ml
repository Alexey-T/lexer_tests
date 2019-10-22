open Tk

let ic = open_in "ebo"
let oc = open_out "ebi"

let parse_logic_message() =
  let s = input_line ic in
  try
    ignore (int_of_string s); s
  with
    Failure _ -> "invalid server response"

let bclick_cb b () =
  output_string oc "bouton\n"; flush oc;
  let s = parse_logic_message () in
  Button.configure b [Text s]

let topwindow = openTk()
let bcount = Button.create topwindow [Text "never clicked"];;
let bclick = Button.create topwindow
    [Text "hello world!"; Command(bclick_cb bcount)]
let bquit = Button.create topwindow [Text "Quit"; Command closeTk]
let _ = Button.configure bquit
    [Background(NamedColor "red"); Foreground(NamedColor "white")]
let _ = pack [bclick; bcount; bquit] [Side Side_Left]
let _ = mainLoop()
