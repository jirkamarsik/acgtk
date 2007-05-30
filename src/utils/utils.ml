module StringSet = Set.Make (String)

module StringMap = Map.Make (String)

let rec string_of_list sep to_string = function
  | [] -> ""
  | [a] -> to_string a
  | a::tl -> Printf.sprintf "%s%s%s" (to_string a) sep (string_of_list sep to_string tl)



let error_msg loc file msg =
  let (_,line,first_char,last_char) = Stdpp.line_of_loc file loc in
    Printf.sprintf
      "%sFile \"%s\", line %d, characters %d-%d"
      msg
      file
      line
      first_char
      last_char


let parse_file entry file exn =
  let in_strm = Stream.of_channel (open_in file) in
    try
      Grammar.Entry.parse entry in_strm
    with
      | Stdpp.Exc_located (loc,e) ->
	  let (_,line,first_char,last_char) = Stdpp.line_of_loc file loc in
	  let msg = Printf.sprintf
		      "File \"%s\", line %d, characters %d-%d"
		      file
		      line
		      first_char
		      last_char in
	    match e with
	      | Stream.Error txt -> raise (exn (Printf.sprintf "%s:\n%s" msg txt))
	      | _ -> let () = Printf.fprintf stderr "%s\n" msg in
		  raise e


let new_parse_file file  parse_function exn =
  let in_strm = Stream.of_channel (open_in file) in
    try
      parse_function  in_strm
    with
      | Stdpp.Exc_located (loc,e) ->
	  let (_,line,first_char,last_char) = Stdpp.line_of_loc file loc in
	  let msg = Printf.sprintf
	    "File \"%s\", line %d, characters %d-%d"
	    file
	    line
	    first_char
	    last_char in
	    match e with
	      | Stream.Error txt -> raise (exn (Printf.sprintf "%s:\n%s" msg txt))
	      | _ -> let () = Printf.fprintf stderr "%s\n" msg in
		  raise e



let get_parse_errors file f a exn =
  try
    f a
  with
    | Stdpp.Exc_located (loc,e) ->
	let (_,line,first_char,last_char) = Stdpp.line_of_loc file loc in
	let msg = Printf.sprintf
	  "File \"%s\", line %d, characters %d-%d"
	  file
	  line
	  first_char
	  last_char in
	  match e with
	    | Stream.Error txt -> raise (exn (Printf.sprintf "%s:\n%s" msg txt))
	    | _ -> let () = Printf.fprintf stderr "%s\n" msg in
		raise e



exception No_file of (string * string )

(** [find_file f dirs msg] tries to find a file with the name [f] in
     the directories listed in [dirs]. If it finds it in [dir], it returns
     the full name [Filename.concat dir f]. To check in the current
     directory, add [""] to the list. It tries in the directories of [dirs]
     in this order and stops when it finds such a file. If it can't find
     any such file, raise the exception [No_file(f,msg)].*)
let find_file name dirs msg=
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
      rec_find_file dirs
  with
    | Sys_error("Is a directory") -> 
	failwith (Printf.sprintf "Failed while trying to trace file '%s'" name )
	  
