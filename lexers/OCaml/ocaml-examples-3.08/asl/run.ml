(* $Id: run.ml,v 1.1.1.1 2002/05/28 15:59:16 weis Exp $ *)

open Main;;

input_stream := stdin;;

if !Sys.interactive then () else begin go(); exit 0 end;;
