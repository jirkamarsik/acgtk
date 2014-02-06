(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

module StringSet = Set.Make (String)

module StringMap = Map.Make (String)

module IntMap = Map.Make (struct type t=int let compare i j = i-j end)
module IntSet = Set.Make (struct type t=int let compare i j = i-j end)


let string_of_list sep to_string = function
  | [] -> ""
  | [a] -> to_string a
  | a::tl ->
      let buf = Buffer.create 16 in
      let () = Buffer.add_string buf (to_string a) in
      let () =
	List.iter
	  (fun s -> Buffer.add_string buf (Printf.sprintf "%s%s" sep (to_string s)))
	  tl in
	Buffer.contents buf


let string_of_list_rev sep to_string lst =
  let buf = Buffer.create 16 in
  let rec string_of_list_rev_rec k = function
    | [] -> k ()
    | [a] -> let () = Buffer.add_string buf (to_string a) in k()
    | a::tl ->
	string_of_list_rev_rec (fun () -> let () = 
				  Buffer.add_string buf (Printf.sprintf "%s%s" sep (to_string a))
				in k()) tl in
  let () = string_of_list_rev_rec (fun () -> ()) lst in
    Buffer.contents buf



exception No_file of (string * string )

(** [find_file f dirs msg] tries to find a file with the name [f] in
     the directories listed in [dirs]. If it finds it in [dir], it
     returns the full name [Filename.concat dir f]. To check in the
     current directory, add [""] to the list. It tries in the
     directories of [dirs] in this order and stops when it finds such
     a file. If it can't find any such file, raise the exception
     {!Utils.No_file(f,msg)}. Moreover, if [f] starts with ["/"] or
     ["./"] or ["../"] then it checks wheter [f] exists only in the
     current directory.*)
let find_file name dirs =
  let regexp = Str.regexp "\\(^\\./\\)\\|\\(^\\.\\./\\)\\|\\(^/\\)" in
  let check_dirs = not (Str.string_match regexp name 0) in
  let msg = if check_dirs then
    string_of_list " nor in " (fun x -> if x = "" then "current directory" else Printf.sprintf "\"%s\"" x)  dirs
  else
    "current directory"
  in
    try
      let get_name f = 
	if Sys.file_exists f
	then 
	  f
	else
	  raise (No_file (f,msg)) in
      let rec rec_find_file = function
	| [] -> raise (No_file (name,msg))
	| dir::dirs ->
	    try
	      get_name (Filename.concat dir name)
	    with
	      | No_file _ -> rec_find_file dirs in
	if check_dirs then
	  rec_find_file dirs
	else
	  get_name name
    with
      | Sys_error("Is a directory") -> 
	  failwith (Printf.sprintf "Failed while trying to trace file '%s'" name )
	  

let (>>) f g = fun x -> f (g x)

let log_iteration log_function s =
  IFDEF BOLT THEN
  List.iter
    log_function
    (Bolt.Utils.split "\n" s)
    ELSE
    ()
    END
