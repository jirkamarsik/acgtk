{
  open Error
  open Abs
  open Term_parser

  let pr lexbuf = Printf.printf "%s\n%!" (Lexing.lexeme lexbuf)
	
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
      | "(*" {comment [Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf] lexbuf}
      | "*)" {raise (Error (Lexer_error Unstarted_comment))}
      | eof {EOI}
      | ['='] {EQUAL}
      | [';'] {SEMICOLON}
      | [':'] {COLON}
      | [','] {COMMA}
      | ['('] {LPAREN (Lexing.lexeme_start_p lexbuf)}
      | [')'] {RPAREN (Lexing.lexeme_start_p lexbuf)}
      | ['.'] {DOT}
      | "signature" {SIG_OPEN}
      | "end" {END_OF_DEC}
      | "type" {TYPE}
      | "prefix" {PREFIX}
      | "infix" {INFIX}
      | "binder" {BINDER}
      | "lambda" {LAMBDA0}
      | "Lambda" {LAMBDA}
      | letter string {IDENT (Lexing.lexeme lexbuf)}
      | symbol* {SYMBOL (Lexing.lexeme lexbuf)}
    and comment depth = parse
      | "*)" {match depth with
		| [a] -> lexer lexbuf
		| a::tl -> comment tl lexbuf
		| [] -> raise (Error (Lexer_error Unstarted_comment))}
      | "(*" {comment ((Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf)::depth) lexbuf}
      | eof {raise (Error (Lexer_error (Unclosed_comment (List.hd depth))))}
      | newline {let () = Error.update_loc lexbuf None 1 false 0 in comment depth lexbuf}
      | _ {comment depth lexbuf}

