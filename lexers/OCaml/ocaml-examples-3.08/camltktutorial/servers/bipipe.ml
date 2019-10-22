(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(* Copyright 2001, 2004 Institut National de Recherche en Informatique *)
(* et en Automatique. All rights reserved. This file is distributed    *)
(* under the terms of the Q Public License version 1.0.                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: Exp *)

(* The bipipe module, to launch two programs connected via stdin/stdout. *)

(* The shell version:

#!/bin/sh
# creates two fifos (or named pipes)
mknod p1top2 p
mknod p2top1 p
# Launch the first argument reading from one fifo and writing to the other one.
$1 < p2top1 > p1top2 &
# Launch the second argument WRITING to one fifo and reading from
# the other one (order of file descriptor opening is relevant here,
# otherwise we get a deadlock).
$2 > p2top1 < p1top2 &
rm -f p1top2 p2top1

# Or better and simpler:

mknod fifo p
< fifo $1 | $2  > fifo &
rm -f fifo

#Generalisation to more than 2 processes:

mknod fifo p
L="$1"
shift
for CMD in $*; do L="$L | $CMD"; done
sh -c "< fifo $L > fifo &"
rm -f fifo

*)

(* The Caml version, using module Unix. *)
let connect_stdio proc (fdin1, fdout1) (fdin2, fdout2) =
  Unix.close fdout1;
  Unix.close fdin2;
  Unix.dup2 fdin1 Unix.stdin;
  Unix.close fdin1;
  Unix.dup2 fdout2 Unix.stdout;
  Unix.close fdout2;
  proc ();;

let connect_bi_directional proc1 proc2 =
  let p1 = Unix.pipe () in
  let p2 = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdio proc2 p1 p2
  | _ -> connect_stdio proc1 p2 p1;;

let launch prog () = Unix.execv prog [| prog |];;

let launch_connected_processes prog1 prog2 =
  connect_bi_directional (launch prog1) (launch prog2);;
