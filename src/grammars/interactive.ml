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

open Abstract_syntax

let interactive = ref false

let dirs = ref [""]

type status = Failure | Success

let status = ref Success

let output_name = ref None
    

let options =
  [
    ("-version", Arg.Unit (fun () -> Printf.printf "%s\n" Version.version;exit 0), Format.sprintf "@[Prints the version number@]");
    ("-o", Arg.String (fun name -> output_name:=Some name) , Format.sprintf "@[-o @[<hov 3>file_name sets the name of the ouput file to \"file_name\".@ The default is to use the base name@ (with no extension)@ of the first file argument with the suffix \".acgo\"@]@]");
    ("-i", Arg.Set interactive , " Enters the interaction loop to parse terms according to signatures");
    ("-I", Arg.String (fun dir -> dirs := (!dirs)@[dir]) , " -I dir sets dir as a directory in which file arguments can be looked for")
  ]
  
let usg_msg = Format.sprintf "@[usage:\n@[\t%s [options] file1 file2 ...@]@.@[This command parse the files which are supposed to be files containing acg signatures or lexicons.@ If all the parses are successful, a binary containing all the acg signatures and lexicons is created.@ Its default name is \"file1.acgo\"@ (see option -o).@ Files should have suffix \".acg\".@]" Sys.executable_name

module Make(Lex:Interface.Lexicon_sig) =
struct  
  module Actual_env = Environment.Make(Lex)
  module Sg=Actual_env.Signature1

  let env = ref Actual_env.empty

  module Actual_parser = Data_parser.Make(Actual_env)

  let parse_term sg =
    let t = ref None in
    let rec parse_rec = function
      | true ->
	let () = Printf.printf "Enter a term: " in
	let term_string = read_line () in
	(match Actual_parser.parse_term term_string sg with
	| None -> parse_rec true
	| Some ta -> let () = t:= (Some ta) in false )
      | false -> false in
    let () =
      while (parse_rec true) do
	()
      done in
    match !t with
    | Some u -> u
    | _ -> failwith "Strange..."
      
      
  let parse filename =
    if !status=Failure then
      ()
    else
      if not (Filename.check_suffix filename ".acg") then
	let () = Printf.fprintf stderr "File name's suffixes should be \".acg\". The name \"%s\" has not this suffix.\n" filename in
	status:=Failure
      else
	let () =
	  match !output_name with
	  | None -> 
	    let basename=Filename.basename filename in
	    let name_wo_suffix = Filename.chop_suffix basename ".acg" in
	    output_name:=Some (Printf.sprintf "%s.acgo" name_wo_suffix)
	  | Some _ -> () in
	match Actual_parser.parse_data filename !dirs !env with
	| None -> status:=Failure
	| Some e -> env:=e
	  
	  
  let term_parsing i env =
    if not i then
      ()
    else
      let n = Actual_env.sig_number env in
      let m = Actual_env.lex_number env in
      let available_data =
	Utils.string_of_list
	  "\n"
	  (fun x -> x)
	  (Actual_env.fold
	     (fun d a -> 
	       match d with
	       | Actual_env.Signature sg -> (Printf.sprintf "\tSignature\t%s" (fst (Actual_env.Signature1.name sg)))::a
	       | Actual_env.Lexicon lx -> (Printf.sprintf "\tLexicon\t\t%s" (fst (Actual_env.Lexicon.name lx)))::a)
	     []
	     env) in
      let chosen_sig=Actual_env.choose_signature env in
      let chosen_sig_name_loaded =
	match chosen_sig with
	| None -> ""
	| Some s -> Printf.sprintf "Signature \"%s\" loaded." (fst (Sg.name s))  in
      if (n+m=0) || (not i)
      then
	()
      else
	try
	  let () = if (n=1)&&(m=0) then Printf.printf "%s\n" chosen_sig_name_loaded else () in
	  while true do
	    try
	      let () = Printf.printf "Available data:\n%s\n" available_data in
	      let entry =
		match n,chosen_sig with
		| 1, Some s -> Actual_env.Signature s
		| _,_ -> 
		  let () = Printf.printf "Enter a name: " in
		  let sig_string = read_line () in 
		  Actual_env.get sig_string env in
	      match entry with
	      | Actual_env.Signature sg -> ignore (parse_term sg)
	      | Actual_env.Lexicon lex -> 
		let abs,obj=Actual_env.Lexicon.get_sig lex in
		let t,ty = parse_term abs in
		let t',ty'=Actual_env.Lexicon.interpret t ty lex in
		Printf.printf
		  "Interpreted as:\n%s : %s\n"
		  (Actual_env.Signature1.term_to_string t' obj)
		  (Actual_env.Signature1.type_to_string ty' obj)
	    with
	    | Actual_env.Signature_not_found sig_name -> Printf.printf "No such signature in %s\n" sig_name
	  done
	with
	| End_of_file -> let () = print_newline () in ()
						   
						   
  let output_env name env =
    let outch=open_out name in
    let () = output_value outch env in
    let () = close_out outch in
    Printf.printf "Output written on: \"%s\"\n%!" name
						  
							  
  let main () =
    let () = Arg.parse options parse usg_msg in
    if !status = Success then
      match !output_name with
      | None -> 
	let () = Printf.fprintf stderr "No ouput file is produced\n"
	in 0
      | Some n ->
	let () = output_env n !env in
	let () = term_parsing !interactive !env in
	0
    else
      1
end
