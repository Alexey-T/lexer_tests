let index a =
  let rec index_rec i = function
    | []  -> raise Not_found
    | b :: l -> if a = b then i else index_rec (succ i) l in
  index_rec 0;;

let flat l = List.fold_left ( @) [] l;;

let subtract f l =
 match f, l with
 | f, [] -> f
 | f, e ->
    let rec subtract_e = function
      | [] -> []
      | elem :: l ->
         if List.mem elem e then subtract_e l else elem :: subtract_e l in
 subtract_e f
;;

let rec make_set = function
  | []  -> []
  | x :: l -> if List.mem x l then make_set l else x :: make_set l
;;

let except_nth =
  let rec except_n_l n = function
    | [] -> []
    | (elem :: l) -> if n = 0 then l else elem :: except_n_l (n - 1) l in
  fun l n -> except_n_l n l
;;


