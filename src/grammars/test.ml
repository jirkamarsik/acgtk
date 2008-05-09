open Abstract_syntax
open Data_parsing

let interactive = ref false

let options =
  [
  ("-i", Arg.Set interactive , "Enter the interaction loop to parse terms according to signatures")
  ]
  
let usg_msg = "./test [options] file"
  
  
let parse i filename =
  let env = Data_parsing.signature filename in
    if Environment.Env.is_empty env || (not !i)
    then
      exit 0
    else
      try
	while true do
	  try
	    let () = Printf.printf "Enter a signature: " in
	    let sig_string = read_line () in
	    let sg =  Environment.Env.find sig_string env in
	    let () = Printf.printf "Enter a term: " in
	    let term_string = read_line () in
	    let _ = Data_parsing.term term_string sg in
	      ()
	  with
	    | Not_found -> Printf.printf "No such signature in %s\n" filename
	done
      with
	| End_of_file -> let () = print_newline () in exit 0
	
	
let () = Arg.parse options (parse interactive) usg_msg
