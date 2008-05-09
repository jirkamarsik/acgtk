{
  open Error
  open Entry
  open Parser

  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf

  let brackets = ref []

  let add_bracket loc = brackets:=loc::!brackets

  let remove_bracket l = match !brackets with
    | [] -> raise (Error (Lexer_error (Unstarted_bracket l)))
    | _::tl -> brackets := tl

  let check_brackets () =
    match !brackets with
      | [] -> ()
      | (p1,p2)::__ -> raise (Error (Lexer_error (Mismatch_parentheses (p1,p2))))
	
	  
  let data = ref (Entry.start_data ())


  let update_data v (p1,p2) =
    try
      data := Entry.transition !data v
    with
      | Entry.Expect l -> 
	  let s = Utils.string_of_list " or " Entry.valuation_to_string l in
	    raise (Error (Lexer_error (Expect (s,p1,p2))))
	  

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*

  let symbol = ['!' '"' '#' '$' '%' '&' '\'' '*' '+' '-' '/' '<' '>' '?' '@' '[' '\\' ']' '^' '`' '{' '}' '~' ]


    rule lexer =
    parse
      | [' ' '\t'] {lexer lexbuf}
      | newline {let () = Error.update_loc lexbuf None 1 false 0 in lexer lexbuf}
      | "(*" {comment [loc lexbuf] lexbuf}
      | "*)" {raise (Error (Lexer_error (Unstarted_comment (loc lexbuf))))}
      | eof {let () = update_data Entry.EOI (loc lexbuf) in
	       EOI}
      | ['='] {let () = update_data Entry.Equal (loc lexbuf) in
	       let () = check_brackets () in
		 EQUAL(loc lexbuf)}
      | [';'] {let () = update_data Entry.Semi_colon (loc lexbuf) in
	       let () = check_brackets () in
		 SEMICOLON(loc lexbuf)}
      | [':'] {let () = update_data Entry.Colon (loc lexbuf) in
	       let () = check_brackets () in
		 COLON(loc lexbuf)}
      | [','] {let () = update_data Entry.Comma (loc lexbuf) in
	       let () = check_brackets () in
		 COMMA(loc lexbuf)}
      | ['('] {let () = update_data Entry.Type_or_term (loc lexbuf) in
	       let l = loc lexbuf in
	       let () = add_bracket l in
		 LPAREN l}
      | [')'] {let () = update_data Entry.Type_or_term (loc lexbuf) in
	       let brac_loc = loc lexbuf in
	       let () = remove_bracket brac_loc in
		 RPAREN brac_loc}
      | ['.'] {let () = update_data Entry.Type_or_term (loc lexbuf) in
		 DOT(loc lexbuf)}
      | "signature" {let () = update_data Entry.Sig_kwd (loc lexbuf) in
		     let () = check_brackets () in
		       SIG_OPEN(loc lexbuf)}
      | "end" {let () = update_data Entry.End_kwd (loc lexbuf) in
	       let () = check_brackets () in
		 END_OF_DEC(loc lexbuf)}
      | "type" {let () = update_data Entry.Type_kwd (loc lexbuf) in
		let () = check_brackets () in
		  TYPE(loc lexbuf)}
      | "prefix" {let () = update_data Entry.Prefix_kwd (loc lexbuf) in
		  let () = check_brackets () in
		    PREFIX(loc lexbuf)}
      | "infix" {let () = update_data Entry.Infix_kwd (loc lexbuf) in
		 let () = check_brackets () in
		   INFIX(loc lexbuf)}
      | "binder" {let () = update_data Entry.Binder_kwd (loc lexbuf) in
		  let () = check_brackets () in
		    BINDER(loc lexbuf)}
      | "lambda" {let () = update_data Entry.Type_or_term (loc lexbuf) in
		    LAMBDA0(loc lexbuf)}
      | "Lambda" {let () = update_data Entry.Type_or_term (loc lexbuf) in
		    LAMBDA(loc lexbuf)}
      | "->" {let () = update_data Entry.Type_or_term (loc lexbuf) in
		LIN_ARROW(loc lexbuf)}
      | letter string {let () = update_data Entry.Id (loc lexbuf) in
			 IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
      | symbol {let () = update_data Entry.Sym (loc lexbuf) in
		  SYMBOL (Lexing.lexeme lexbuf,loc lexbuf)}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error (Lexer_error (Unstarted_comment (loc lexbuf))))}
      | "(*" {comment ((loc lexbuf)::depth) lexbuf}
      | eof {raise (Error (Lexer_error (Unclosed_comment (List.hd depth))))}
      | newline {let () = Error.update_loc lexbuf None 1 false 0 in comment depth lexbuf}
      | _ {comment depth lexbuf}

