{
  open Error
  open Parser

  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf

  let brackets = ref []

  let add_bracket loc = brackets:=loc::!brackets

  let remove_bracket () = match !brackets with
    | [] -> raise (Error (Lexer_error Unstarted_bracket))
    | _::tl -> brackets := tl

  let check_brackets () =
    match !brackets with
      | [] -> ()
      | (p1,p2)::__ -> raise (Error (Lexer_error (Mismatch_parentheses (p1,p2))))
	
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
      | "*)" {raise (Error (Lexer_error Unstarted_comment))}
      | eof {EOI}
      | ['='] {let () = check_brackets () in EQUAL(loc lexbuf)}
      | [';'] {let () = check_brackets () in SEMICOLON(loc lexbuf)}
      | [':'] {let () = check_brackets () in COLON(loc lexbuf)}
      | [','] {let () = check_brackets () in COMMA(loc lexbuf)}
      | ['('] {let l = loc lexbuf in
	       let () = add_bracket l in
	       LPAREN l}
      | [')'] {let () = remove_bracket ()
	       in RPAREN (loc lexbuf)}
      | ['.'] {DOT(loc lexbuf)}
      | "signature" {let () = check_brackets () in SIG_OPEN(loc lexbuf)}
      | "end" {let () = check_brackets () in END_OF_DEC(loc lexbuf)}
      | "type" {let () = check_brackets () in TYPE(loc lexbuf)}
      | "prefix" {let () = check_brackets () in PREFIX(loc lexbuf)}
      | "infix" {let () = check_brackets () in INFIX(loc lexbuf)}
      | "binder" {let () = check_brackets () in BINDER(loc lexbuf)}
      | "lambda" {LAMBDA0(loc lexbuf)}
      | "Lambda" {LAMBDA(loc lexbuf)}
      | "->" {LIN_ARROW(loc lexbuf)}
      | letter string {IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
      | symbol {SYMBOL (Lexing.lexeme lexbuf,loc lexbuf)}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error (Lexer_error Unstarted_comment))}
      | "(*" {comment ((loc lexbuf)::depth) lexbuf}
      | eof {raise (Error (Lexer_error (Unclosed_comment (List.hd depth))))}
      | newline {let () = Error.update_loc lexbuf None 1 false 0 in comment depth lexbuf}
      | _ {comment depth lexbuf}

