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

#use "start.ml";;
#use "hello.ml";;
#use "hello_quit.ml";;
#use "addition.ml";;
#use "convertion_euro.ml";;
#use "convert_euro.ml";;
#use "convertion.ml";;
#use "convert.ml";;
#use "rgb.ml";;
#use "camleyes.ml";;
#use "taquin.ml";;
#use "tetris.ml";;

print_string "\n
To run type in one of:
        start ();;
        Camltk.closeTk();;
        hello ();;
        Camltk.closeTk();;
        hello_quit ();;
        addition ();;
        convertion_en_francs ();;
        convert_in_francs ();;
        convertion ();;
        convert ();;
        rgb ();;
        caml_eyes ();;
        taquin \"file name\" <number> <number>
        (* For instance *)
        taquin \"joconde.gif\" 3 5;;
        tetris ();;
";;

print_newline();;

