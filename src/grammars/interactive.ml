open Abstract_syntax

let interactive = ref false



let options =
  [
  ("-i", Arg.Set interactive , "Enter the interaction loop to parse terms according to signatures")
  ]
  
let usg_msg = "./test [options] file"

module Make(Sg:Interface.Signature_sig)(Lex:Interface.Lexicon_sig with type signature = Sg.t) =
struct  
  module Actual_env = Environment.Make(Sg)(Lex)

  let env = ref Actual_env.empty

  module Actual_parser = Parser.Make(Actual_env)

    
  let parse_term sg =
    let rec parse_rec = function
      | true ->
	  let () = Printf.printf "Enter a term: " in
	  let term_string = read_line () in
	    (match Actual_parser.parse_term term_string sg with
	       | None -> parse_rec true
	       | Some _ -> false )
      | false -> false in
      while (parse_rec true) do
	()
      done
	
	
  let parse filename =
    env := Actual_parser.parse_data filename !env
      
      
  let term_parsing i env =
    let n = Actual_env.sig_number env in
    let available_sig =
      Utils.string_of_list
	"\n"
	(fun x -> x)
	(Actual_env.fold
	   (fun d a -> 
	      match d with
		| Actual_env.Signature sg -> (fst (Actual_env.Signature.name sg))::a
		| _ -> a)
	   []
	   env) in
    let chosen_sig=Actual_env.choose_signature env in
    let chosen_sig_name_loaded =
      match chosen_sig with
	| None -> ""
	| Some s -> Printf.sprintf "Signature \"%s\" loaded." (fst (Sg.name s))  in
      if (n=0) || (not !i)
      then
	()
      else
	try
	  let () = if n=1 then Printf.printf "%s\n" chosen_sig_name_loaded else () in
	    while true do
	      try
		let () = Printf.printf "Available signatures:\n%s\n" available_sig in
		let sg =
		  match n,chosen_sig with
		    | 1, Some s -> s
		    | _,_ -> 
			let () = Printf.printf "Enter a signature: " in
			let sig_string = read_line () in 
			  Actual_env.get_signature sig_string env in
		  parse_term sg
	      with
		| Actual_env.Signature_not_found sig_name -> Printf.printf "No such signature in %s\n" sig_name
	    done
	with
	  | End_of_file -> let () = print_newline () in ()
							  
							  
  let main () =
    let () = Arg.parse options parse usg_msg in
      term_parsing interactive !env
end