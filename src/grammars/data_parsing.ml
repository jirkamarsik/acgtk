open Abstract_syntax

module Data_parsing =
struct
  let data filename env =
    let in_ch = open_in filename in
    let lexbuf = Lexing.from_channel in_ch in
      try
	let () = Printf.printf "Parsing \"%s\"...\n%!" filename in
	let () = Lexer.set_to_data () in
	let sgs = 
	  try (fst (List.hd (Parser.signatures Lexer.lexer lexbuf))) env with
	    |  Dyp.Syntax_error -> raise (Error.dyp_error lexbuf filename) in
	let () = Printf.printf "Done.\n" in
	let () =
	  Environment.iter 
	    (fun (Environment.Signature sg) -> 
	       let () = Printf.printf "%s\n" (Abstract_sig.to_string sg) in
		 Printf.printf "%s\n" (Error.warnings_to_string filename (Abstract_sig.get_warnings sg)))
	    sgs in
	  sgs
      with
	| Error.Error e -> 
	    let () = Printf.fprintf stderr "Error: %s\n" (Error.error_msg e filename) in
	      env
	    
  let term t sg = 
    let lexbuf = Lexing.from_string t in
      try 
	let () = Lexer.set_to_term () in
	let abs_term = 
	  try fst (List.hd(Parser.term_alone ~local_data:(Some sg) Lexer.lexer lexbuf)) with
	    | Dyp.Syntax_error -> raise (Error.dyp_error lexbuf "stdin") in
	let () = Printf.printf "I read: %s\n" (Abstract_sig.term_to_string abs_term sg) in
	  Some abs_term
      with
	| Error.Error e -> 
	    let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e "stdin") in
	      None
	| End_of_file -> None
    

end
