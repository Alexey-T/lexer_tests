(****************** Term manipulations *****************)
open Prelude;;

type term =
   | Var of int
   | Term of string * term list;;

let rec vars = function
  | Var n -> [n]
  | Term (_, l) -> vars_of_list l
and vars_of_list = function
  | [] -> []
  | t :: r -> union (vars t) (vars_of_list r)
;;

let substitute subst =
  let rec subst_rec = function
  | Term (oper, sons) -> Term (oper, List.map subst_rec sons)
  | Var (n) as t      -> try List.assoc n subst with Not_found -> t in
  subst_rec
;;

let change f =
  let rec change_rec l n =
  match l with
  | h :: t -> if n = 1 then f h :: t else h :: change_rec t (n - 1)
  | _ -> failwith "change" in
  change_rec
;;

(* Term replacement replace m u n => m[u<-n] *)
let replace m u n =
  let rec reprec = function
  | _, [] -> n
  | Term (oper, sons), (n :: u) ->
             Term (oper, change (fun p -> reprec (p, u)) sons n)
  | _ -> failwith "replace" in
 reprec (m, u)
;;

(* matching = - : (term -> term -> subst) *)
let matching term1 term2 =
  let rec match_rec subst t1 t2 =
    match t1, t2 with
    | Var v, m ->
        if List.mem_assoc v subst then
          if m = List.assoc v subst then subst else failwith "matching"
        else
          (v,t2) :: subst
    | Term (op1, sons1), Term (op2, sons2) ->
	if op1 = op2 then List.fold_left2 match_rec subst sons1 sons2
                     else failwith "matching"
    | _, _ ->
        failwith "matching" in
  match_rec [] term1 term2
;;

(* A naive unification algorithm *)

let compsubst subst1 subst2 = 
  (List.map (fun (v, t) -> (v, substitute subst1 t)) subst2) @ subst1
;;

let occurs n =
  let rec occur_rec = function
  | Var m -> m=n
  | Term (_, sons) -> List.exists occur_rec sons in
  occur_rec
;;

let rec unify = function
  | (Var n1 as term1), term2 ->
      if term1 = term2 then []
      else if occurs n1 term2 then failwith "unify1"
      else [n1,term2]
  | term1, Var n2 ->
      if occurs n2 term1 then failwith "unify2"
      else [n2,term1]
  | Term (op1, sons1), Term (op2, sons2) ->
      if op1 = op2 then 
	List.fold_left2 (fun s t1 t2 -> compsubst (unify (substitute s t1,
                                                   substitute s t2)) s)
                 [] sons1 sons2
      else failwith "unify3"
;;

(* We need to print terms with variables independently from input terms
  obtained by parsing. We give arbitrary names v1,v2,... to their variables. *)

let infixes = ["+";"*";"-";"/"];;

let rec pretty_term = function
  | Var n ->
      print_string "v"; print_int n
  | Term (oper, sons) ->
      if List.mem oper infixes then
        match sons with
          | [s1;s2] ->
              pretty_close s1; print_string oper; pretty_close s2
          | _ ->
              failwith "pretty_term : infix arity <> 2"
      else
       (print_string oper;
        match sons with
	  |  []   -> ()
          | t::lt -> print_string "(";
                     pretty_term t;
                     List.iter (fun t -> print_string ","; pretty_term t) lt;
                     print_string ")")
and pretty_close = function
  | Term (oper, _) as m ->
      if List.mem oper infixes then
        (print_string "("; pretty_term m; print_string ")")
      else pretty_term m
  | m -> pretty_term m
;;

