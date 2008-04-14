open Abstract_syntax
open Data_parsing

let options = []
  
let usg_msg = ""
  
  
let parse filename =
  let env = Data_parsing.signature filename in
    if Environment.Env.is_empty env
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
	
	
let () = Arg.parse options parse usg_msg
