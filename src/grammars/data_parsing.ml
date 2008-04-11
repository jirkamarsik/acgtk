module Data_parsing =
struct
  let signature filename =
    let in_ch = open_in filename in
    let lexbuf = Lexing.from_channel in_ch in
      try
	let sgs = fst (List.hd (Parser.signatures Lexer.lexer lexbuf)) in
	  Parser.Environment.iter 
	    (fun _ sg -> 
	       let () = Printf.printf "%s\n" (Abstract_syntax.Abstract_sig.to_string sg) in
		 Printf.printf "%s\n" (Error.warnings_to_string filename (Abstract_syntax.Abstract_sig.get_warnings sg)))
	    sgs
      with
	| Error.Error e -> 
	    let () = Printf.fprintf stderr "Error: %s\n" (Error.error_msg e lexbuf filename) in
	      Parser.Environment.empty
	| Dyp.Syntax_error -> Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf filename) in
	      Parser.Environment.empty
	    
  let term t sg = 
    let lexbuf = Lexing.from_string t in
      try 
	let abs_term = fst (List.hd(Parser.term_alone ~local_data:(Some sg) Lexer.lexer lexbuf)) in
	  Printf.printf "I read: %s\n" (Abstract_syntax.Abstract_sig.term_to_string abs_term sg)
      with
	| Error.Error e -> Printf.fprintf stderr "Error: %s\n" (Error.error_msg e lexbuf "stdin")
	| Dyp.Syntax_error -> Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf "stdin")
	| End_of_file -> ()
    

end
