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

open Printf;;

type entry = {
  ident : string;
  first_name : string;
  last_name : string;
  address : string;
  tel : string;
  url : string;
};;

type address_book = {
  file : string;
  entries : (string, entry) Hashtbl.t
};;

let print_entry e =
 printf "First name : %s\n" e.first_name;
 printf "Last name : %s\n" e.last_name;
 printf "Addr : %s\n" e.address;
 printf "Tel  : %s\n" e.tel;
 printf "URL  : %s\n" e.url;;

let print_address_book ab =
 print_string "Adresse book from ";
 print_string ab.file;
 print_newline ();
 Hashtbl.iter (fun k e -> print_entry e; print_newline ()) ab.entries;;

let read_address_book_ic ic =
 (input_value ic : address_book);;

let save_address_book_oc oc ab =
 (output_value oc (ab : address_book));;

let read_address_book ab =
 let ic = open_in_bin ab in
 read_address_book_ic ic;
 close_in ic;;

let save_address_book ab =
 let oc = open_out_bin ab.file in
 save_address_book_oc oc ab;
 close_out oc;;

let make_entry id fn ln a t u = {
 ident = id;
 first_name = fn;
 last_name = ln;
 address = a;
 tel = t;
 url = u
};;

let dummy_entry = make_entry "" "" "" "" "" "";;

let dummy_file = "book";;

let the_book =
 ref {file = dummy_file; entries = Hashtbl.create 100};;

let init_address_book ab =
 if ab <> "" then read_address_book ab else read_address_book dummy_file;;

let verify_absent ab e =
 let es = ab.entries in
 if Hashtbl.mem es e.ident &&
    List.mem e (Hashtbl.find_all es e.ident) then
 failwith
  (sprintf "Entry %s is already stored in adress book %s" e.last_name ab.file);;

let verify_present ab e =
 let es = ab.entries in
 let id = e.ident in
 if not (Hashtbl.mem es id) || not (List.mem e (Hashtbl.find_all es id)) then
 failwith
  (sprintf "Entry %s is not stored in adress book %s" n ab.file);;

let add_entry ab n e =
 verify_absent ab e;
 let es = ab.entries in
 Hashtbl.add es n e;;

let rec list_remove x = function
  | [] -> []
  | y :: l -> if y = x then l else y :: list_remove x l;; 

let remove_entry ab e =
 let es = ab.entries in
 let eqs = Hashtbl.find_all es e.name in
 let but_e = list_remove e eqs in
 List.iter (fun e -> Hashtbl.remove es e.name) eqs;
 List.iter (fun e -> Hashtbl.add es e.name e) but_e;;

let modify_entry ab e new_e =
 verify_present ab e;
 verify_absent ab new_e;
 remove_entry ab e;
 add_entry ab e.name new_e;;
 
let delete_entry ab e =
 verify_present ab e;
 remove_entry ab e;;

let search_named_entry ab n =
 Hashtbl.find_all ab.entries n;;

let search ab p =
 let res = ref [] in
 Hashtbl.iter (fun k v -> if p k v then res := v :: !res) ab.entries;
 !res;;

let search_regexp_entry foldp ab pat =
 let re = if foldp then Str.regexp_case_fold pat else Str.regexp pat in
 search ab (fun k v -> Str.string_match re k 0);;

let full_text_regexp_search foldp ab pat =
 let re = if foldp then Str.regexp_case_fold pat else Str.regexp pat in
 search ab
  (fun k v ->
    Str.string_match re v.name 0 ||
    Str.string_match re v.address 0 ||
    Str.string_match re v.tel 0 ||
    Str.string_match re v.url 0);;

type search_entry_predicate = {
  name_pred : string -> bool;
  address_pred : string -> bool;
  tel_pred : string -> bool;
  url_pred : string -> bool;
};;

type search_entry_pattern = {
  name_pat : string;
  address_pat : string;
  tel_pat : string;
  url_pat : string;
};;

let full_search ab se =
 search ab
  (fun k v ->
    se.name_pred v.name ||
    se.address_pred v.address ||
    se.tel_pred v.tel ||
    se.url_pred v.url);;

let full_text_regexp_search foldp ab pat =
 let regexp = if foldp then Str.regexp_case_fold else Str.regexp in
 let str_match p s = Str.string_match (regexp p) s 0 in
 let se = {
  name_pred = str_match pat.name_pat;
  address_pred = str_match pat.address_pat;
  tel_pred = str_match pat.tel_pat;
  url_pred = str_match pat.url_pat;
 } in
 full_search ab se;;

let make_name_entry_pattern pat = {
  name_pat = pat;
  address_pat = "";
  tel_pat = "";
  url_pat = "";
};;

let make_full_text_entry_pattern pat = {
  name_pat = pat;
  address_pat = pat;
  tel_pat = pat;
  url_pat = pat;
};;

let full_search ab pat =
 full_text_regexp_search true ab (make_full_text_entry_pattern pat);;

let enter_new_entry n a t u = add_entry !the_book n (make_entry n a t u);;

enter_new_entry
 "Weis"
 "Domaine de Voluceau"
 "5738"
 "http://pauillac.inria.fr/~weis/";;

print_address_book !the_book;;
