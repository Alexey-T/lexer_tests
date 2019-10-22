(* $Id: main.mli,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

val read_fun : int -> char option;;
val go : unit -> unit;;
val input_stream : in_channel ref;;
val trace_parsing : bool ref;;


