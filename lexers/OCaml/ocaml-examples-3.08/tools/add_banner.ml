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

let string_of_ic ic =
 let l = in_channel_length ic in
 let buf = String.create 1024 in
 let res = Buffer.create l in
 try
  while true do
   let n = input ic buf 0 1024 in
   if n = 0 then raise Exit else
   Buffer.add_substring res buf 0 n
  done;
  Buffer.contents res
 with Exit -> Buffer.contents res;;

let string_of_file fname =
 let ic = open_in fname in
 let r = string_of_ic ic in
 close_in ic;
 r;;

let copy_channel ic oc =
 let buf = String.create 1024 in
 try
  while true do
   let n = input ic buf 0 1024 in
   if n = 0 then raise Exit else
   output oc buf 0 n
  done
 with Exit -> ();;

let copy_file s copy =
  let ic = open_in s in
  let oc = open_out copy in
  copy_channel ic oc;
  close_out oc;
  close_in ic;;

let bannerize_channel banner ic oc =
 output_string oc banner;
 copy_channel ic oc;;

let ckpname fname = fname ^ "~";;
let ckp_file fname =
 let copy = ckpname fname in
 copy_file fname copy;
 copy;;

let bannerize_file banner s =
 let copy = ckp_file s in
 let ic = open_in copy in
 let oc = open_out s in
 bannerize_channel banner ic oc;
 close_out oc;
 close_in ic;;

let with_in_channel fname f =
 let ic = open_in fname in
 try
  let r = f ic in
  close_in ic;
  r
 with x -> close_in ic; raise x;;

let with_out_channel fname f =
 let oc = open_out fname in
 try
  let r = f oc in
  close_out oc;
  r
 with x -> close_out oc; raise x;;

let string_of_file fname = with_in_channel fname string_of_ic;;

let copy_file s copy =
 with_in_channel s (fun ic -> with_out_channel copy (copy_channel ic));;

let bannerize_file banner fname =
 let copy = ckp_file fname in
 with_in_channel copy
  (fun ic -> with_out_channel fname (bannerize_channel banner ic));;

let skip_banner_channel banner_marker ic =
 let l = try input_line ic with End_of_file -> "" in
 if l = banner_marker then
  try
   while input_line ic <> banner_marker do () done
  with End_of_file ->
  invalid_arg "replace_banner: end of banner not found"
 else seek_in ic 0;;

(* The main function to rewrite files with banners. *)
let replace_banner banner_marker banner fname =
 let copy = ckp_file fname in
 with_in_channel copy
  (fun ic ->
     skip_banner_channel banner_marker ic;
     with_out_channel fname (bannerize_channel banner ic));;

(* To get the banner's marker. *)
let get_first_line fname = with_in_channel fname input_line;;

let main () =
 let files = ref [] in
 let banner_file = ref "banner" in
 let set_banner_file fname = banner_file := fname in
 let record_file_name s = files := s :: !files in
 let b_opt =  Arg.String set_banner_file
 and b_message =
   " <banner file>: set the contents of file <banner file> as the banner   \n\
      (default name for the banner file is \"banner\")." in
 Arg.parse
  ["-b", b_opt, b_message;
   "--banner", b_opt, b_message]
  record_file_name
  "Usage: banner [-b | --banner <banner file>] <files>\n\n\
   This command adds a banner to the top of each file given in the list of\n\
   argument files.\n\
   The text of the banner is the contents of a <banner file> that can\n\
   be fixed with the [-b] option (default banner file name is \"banner\").\n\
   The text of a banner must be comprised between two identical lines,\n\
   the ``banner markers'' (hence the first and the last lines of the\n\
   banner file must be identical).\n\
   Two banners are considered similar if they have the same banner markers.\n\
   If a file already had a similar banner, this banner is removed \
   before adding\n\
   the contents of the <banner file>. This way, slight modifications of the\n\
   banner are easily handled.\n\n\
   Prior to add a banner to the file \"f\", a checkpoint of \"f\" is\n\
   written in the file named [f~].";
 let banner_marker = get_first_line !banner_file in
 let banner = string_of_file !banner_file in
 List.iter (replace_banner banner_marker banner) !files;;

try main () with Sys_error s -> prerr_endline ("Fatal error: " ^ s); exit 1;;

