{
  open Error
  open Abs
  open Term_parser

  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)

  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
	
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
      | ['='] {EQUAL(loc lexbuf)}
      | [';'] {SEMICOLON(loc lexbuf)}
      | [':'] {COLON(loc lexbuf)}
      | [','] {COMMA(loc lexbuf)}
      | ['('] {LPAREN (loc lexbuf)}
      | [')'] {RPAREN (loc lexbuf)}
      | ['.'] {DOT(loc lexbuf)}
      | "signature" {SIG_OPEN(loc lexbuf)}
      | "end" {END_OF_DEC(loc lexbuf)}
      | "type" {TYPE(loc lexbuf)}
      | "prefix" {PREFIX(loc lexbuf)}
      | "infix" {INFIX(loc lexbuf)}
      | "binder" {BINDER(loc lexbuf)}
      | "lambda" {LAMBDA0(loc lexbuf)}
      | "Lambda" {LAMBDA(loc lexbuf)}
      | letter string {IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
      | symbol* {SYMBOL (Lexing.lexeme lexbuf,loc lexbuf)}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error (Lexer_error Unstarted_comment))}
      | "(*" {comment ((loc lexbuf)::depth) lexbuf}
      | eof {raise (Error (Lexer_error (Unclosed_comment (List.hd depth))))}
      | newline {let () = Error.update_loc lexbuf None 1 false 0 in comment depth lexbuf}
      | _ {comment depth lexbuf}

