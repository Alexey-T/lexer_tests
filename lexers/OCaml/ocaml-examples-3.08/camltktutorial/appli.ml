(*
ocamlc -I +camltk -c complexegui.ml
ocamlc -c complexe.ml
ocamlc -custom -I +camltk unix.cma camltk.cma complexe.cmo complexegui.cmo appli.ml

let connect_stdio fdin fdout =
  Unix.dup2 fdin Unix.stdin;
  Unix.dup2 fdout Unix.stdout;;

let bi_directional_pipe serv ihm =
  let fdi1, fdo1 = Unix.pipe () in
  let fdi2, fdo2 = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
     connect_stdio fdi1 fdo2;
     Unix.close fdo1;
     Unix.close fdi2;
     ihm ()
  | _ -> 
     connect_stdio fdi2 fdo1;
     Unix.close fdo2;
     Unix.close fdi1;
     serv ();;

let connect_stdio proc (fdin1, fdout1) (fdin2, fdout2) =
  Unix.dup2 fdin1 Unix.stdin;
  Unix.dup2 fdout2 Unix.stdout;
  Unix.close fdout1;
  Unix.close fdin2;
  proc ();;

let bi_directional_connect serv ihm =
  let p1 = Unix.pipe () in
  let p2 = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdio ihm p1 p2
  | _ -> connect_stdio serv p2 p1;;
*)
let connect_stdio proc (fdin1, fdout1) (fdin2, fdout2) =
  Unix.dup2 fdin1 Unix.stdin;
  Unix.dup2 fdout2 Unix.stdout;
  Unix.close fdout1;
  Unix.close fdin2;
  proc ();;

let connect_bi_directional proc1 proc2 =
  let p1 = Unix.pipe () in
  let p2 = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdio proc2 p1 p2
  | _ -> connect_stdio proc1 p2 p1;;

let main () =
 connect_bi_directional Complexegui.gui_server Complexe.computation_server;;

main ();;

