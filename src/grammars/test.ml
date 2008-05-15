open Abstract_syntax
open Data_parsing

let interactive = ref false

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
	    

let parse i filename =
  let env = Data_parsing.signature filename in
  let n = Environment.sig_number env in
  let chosen_sig=Environment.choose_signature env in
  let chosen_sig_name_loaded =
    match chosen_sig with
      | None -> ""
      | Some s -> Printf.sprintf "Signature \"%s\" loaded." (fst (Abstract_sig.name s))  in
    if (n=0) || (not !i)
    then
      exit 0
    else
      try
	let () = if n=1 then Printf.printf "%s\n" chosen_sig_name_loaded else () in
	while true do
	  try
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
	| End_of_file -> let () = print_newline () in exit 0
	
	
let () = Arg.parse options (parse interactive) usg_msg
