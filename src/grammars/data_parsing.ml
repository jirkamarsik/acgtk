open Abstract_syntax

module Data_parsing =
struct
  let signature filename =
    let in_ch = open_in filename in
    let lexbuf = Lexing.from_channel in_ch in
      try
	let () = Printf.printf "Parsing \"%s\"...\n%!" filename in
	let () = Lexer.set_to_data () in
	let sgs = (fst (List.hd (Parser.signatures Lexer.lexer lexbuf))) (Environment.empty) in
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
	      Environment.empty
	| Dyp.Syntax_error -> 
	    let () = Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf filename) in
	      Environment.empty
	    
  let term t sg = 
    let lexbuf = Lexing.from_string t in
      try 
	let () = Lexer.set_to_term () in
	let abs_term = fst (List.hd(Parser.term_alone ~local_data:(Some sg) Lexer.lexer lexbuf)) in
	let () = Printf.printf "I read: %s\n" (Abstract_sig.term_to_string abs_term sg) in
	  Some abs_term
      with
	| Error.Error e -> 
	    let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg e "stdin") in
	      None
	| Dyp.Syntax_error -> 
	    let () = Printf.fprintf stderr "Dyp: %s\n%!" (Error.error lexbuf "stdin") in 
	      None
	| End_of_file -> None
    

end
