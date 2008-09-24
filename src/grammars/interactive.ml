open Abstract_syntax

let interactive = ref false



let options =
  [
  ("-i", Arg.Set interactive , "Enter the interaction loop to parse terms according to signatures")
  ]
  
let usg_msg = "./test [options] file1 file2 ...\n\nThis will parse the files which are supposed to be files containing acg signatures or lexicons."

module Make(Lex:Interface.Lexicon_sig) =
struct  
  module Actual_env = Environment.Make(Lex)
  module Sg=Actual_env.Signature1

  let env = ref Actual_env.empty

  module Actual_parser = Parser.Make(Actual_env)

    
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
    env := Actual_parser.parse_data filename !env
      
      
  let term_parsing i env =
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
      if (n+m=0) || (not !i)
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
							  
							  
  let main () =
    let () = Arg.parse options parse usg_msg in
      term_parsing interactive !env
end
