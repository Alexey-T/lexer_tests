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
type type_simple =
   | Variable of variable_de_type
   | Terme of string * type_simple array

and variable_de_type =
  { mutable niveau: int;
    mutable valeur: valeur_d'une_variable }

and valeur_d'une_variable =
  | Inconnue
  | Connue of type_simple;;

type sch�ma_de_types =
  { param�tres: variable_de_type list;
    corps: type_simple };;

exception Conflit of type_simple * type_simple;;
exception Circularit� of type_simple * type_simple;;

let type_int = Terme("int", [||])
and type_bool = Terme("bool", [||])
and type_fl�che t1 t2 = Terme("->", [|t1; t2|])
and type_produit t1 t2 = Terme("*", [|t1; t2|])
and type_liste t = Terme("list", [|t|]);;

let rec valeur_de = function
  | Variable({valeur = Connue ty1} as var) ->
      let valeur_de_ty1 = valeur_de ty1 in
      var.valeur <- Connue valeur_de_ty1;
      valeur_de_ty1
  | ty -> ty;;

let test_d'occurrence var ty =
  let rec test t =
    match valeur_de t with
    | Variable var' ->
        if var == var' then raise(Circularit�(Variable var, ty))
    | Terme(constructeur, arguments) ->
        Array.iter test arguments
  in test ty;;

let rec rectifie_niveaux niveau_max ty =
  match valeur_de ty with
  | Variable var ->
      if var.niveau > niveau_max then var.niveau <- niveau_max
  | Terme(constructeur, arguments) ->
      Array.iter (rectifie_niveaux niveau_max) arguments;;

let rec unifie ty1 ty2 =
  let valeur1 = valeur_de ty1
  and valeur2 = valeur_de ty2 in
  if valeur1 == valeur2 then () else
    match (valeur1, valeur2) with
    | Variable var, ty ->
        test_d'occurrence var ty;
        rectifie_niveaux var.niveau ty;        
        var.valeur <- Connue ty
    | ty, Variable var ->
        test_d'occurrence var ty;
        rectifie_niveaux var.niveau ty;        
        var.valeur <- Connue ty
    | Terme(constr1, arguments1), Terme(constr2, arguments2) ->
        if constr1 <> constr2 then
          raise (Conflit(valeur1, valeur2))
        else
          for i = 0 to Array.length arguments1 - 1 do
            unifie arguments1.(i) arguments2.(i)
          done;;
let niveau_de_liaison = ref 0;;

let d�but_de_d�finition () = incr niveau_de_liaison
and fin_de_d�finition () = decr niveau_de_liaison;;

let nouvelle_inconnue () =
  Variable {niveau = !niveau_de_liaison; valeur = Inconnue};;

let g�n�ralisation ty =
  let param�tres = ref [] in
  let rec trouve_param�tres ty =
    match valeur_de ty with
    | Variable var ->
        if var.niveau > !niveau_de_liaison && not (List.memq var !param�tres)
        then param�tres := var :: !param�tres
    | Terme(constr, arguments) ->
        Array.iter trouve_param�tres arguments in
  trouve_param�tres ty;
  {param�tres = !param�tres; corps = ty};;

let sch�ma_trivial ty = {param�tres = []; corps = ty};;

let sp�cialisation sch�ma =
  match sch�ma.param�tres with
  | [] -> sch�ma.corps
  | param�tres ->
      let nouvelles_inconnues =
        List.map (fun var -> (var, nouvelle_inconnue())) param�tres in
      let rec copie ty =
        match valeur_de ty with
        | Variable var as ty ->
            begin try
              List.assq var nouvelles_inconnues
            with Not_found ->
              ty
            end
        | Terme(constr, arguments) ->
            Terme(constr, Array.map copie arguments) in
      copie sch�ma.corps;;

let noms_des_variables = ref ([] : (variable_de_type * string) list)
and compteur_de_variables = ref 0;;

let imprime_var var =
  print_string "'";
  try
    print_string (List.assq var !noms_des_variables)
  with Not_found ->
    let nom =
      String.make 1
        (char_of_int (int_of_char 'a' + !compteur_de_variables)) in
    incr compteur_de_variables;
    noms_des_variables := (var, nom) :: !noms_des_variables;
    print_string nom;;

let rec imprime ty =
  match valeur_de ty with
  | Variable var ->
      imprime_var var
  | Terme(constructeur, arguments) ->
      match Array.length arguments with
      | 0 -> print_string constructeur
      | 1 -> imprime arguments.(0);
             print_string " "; print_string constructeur
      | 2 -> print_string "("; imprime arguments.(0);
             print_string " "; print_string constructeur;
             print_string " "; imprime arguments.(1);
             print_string ")"
      | _ -> failwith "constructeur de type ayant trop d'arguments";;

let imprime_type ty =
  noms_des_variables := [];
  compteur_de_variables := 0;
  imprime ty;;
  
let imprime_sch�ma sch�ma =
  noms_des_variables := [];
  compteur_de_variables := 0;
  if sch�ma.param�tres <> [] then begin
    print_string "pour tout ";
    List.iter (fun var -> imprime_var var; print_string " ")
            sch�ma.param�tres;
    print_string ", "
  end;
  imprime sch�ma.corps;;
