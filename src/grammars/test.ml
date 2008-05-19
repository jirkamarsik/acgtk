open Abstract_syntax
open Data_parsing

let interactive = ref false

let env = ref Environment.empty

let options =
  [
  ("-i", Arg.Set interactive , "Enter the interaction loop to parse terms according to signatures")
  ]
  
let usg_msg = "./test [options] file"
  
  
let parse_term sg =
  let rec parse_rec = function
    | true ->
	let () = Printf.printf "Enter a term: " in
	let term_string = read_line () in
	  (match Data_parsing.term term_string sg with
	    | None -> parse_rec true
	    | Some _ -> false )
    | false -> false in
    while (parse_rec true) do
      ()
    done
	    

let parse filename =
  env := Data_parsing.data filename !env
    
    
let term_parsing i env =
  let n = Environment.sig_number env in
  let available_sig =
    Utils.string_of_list
      "\n"
      (fun x -> x)
      (Environment.fold
	 (fun d a -> 
	    match d with
	      | Environment.Signature sg -> (fst (Abstract_sig.name sg))::a)
	 []
	 env) in
  let chosen_sig=Environment.choose_signature env in
  let chosen_sig_name_loaded =
    match chosen_sig with
      | None -> ""
      | Some s -> Printf.sprintf "Signature \"%s\" loaded." (fst (Abstract_sig.name s))  in
    if (n=0) || (not !i)
    then
      ()
    else
      try
	let () = if n=1 then Printf.printf "%s\n" chosen_sig_name_loaded else () in
	  while true do
	    try
	      let () = Printf.printf "Availale signatures:\n%s\n" available_sig in
	      let sg =
		match n,chosen_sig with
		  | 1, Some s -> s
		  | _,_ -> 
		      let () = Printf.printf "Enter a signature: " in
		      let sig_string = read_line () in 
			Environment.get_signature sig_string env in
		parse_term sg
	    with
	      | Environment.Signature_not_found sig_name -> Printf.printf "No such signature in %s\n" sig_name
	  done
      with
	| End_of_file -> let () = print_newline () in ()
	
	
let () =
  let () = Arg.parse options parse usg_msg in
    term_parsing interactive !env
