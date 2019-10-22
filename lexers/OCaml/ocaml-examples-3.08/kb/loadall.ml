let compile f =
 match Sys.command ("ocamlc -c " ^ f) with
 | 0 -> ()
 | _ -> failwith ("Cannot compile " ^ f);;

compile "prelude.ml";;
#load "prelude.cmo";;
open Prelude;;

compile "terms.ml";;
#load "terms.cmo";;
open Terms;;

compile "equation.ml";;
#load "equation.cmo";;
open Equation;;

compile "order.ml";;
#load "order.cmo";;
open Order;;

compile "kb.ml";;
#load "kb.cmo";;
open Kb;;

compile "go.ml";;
#load "go.ml";;
