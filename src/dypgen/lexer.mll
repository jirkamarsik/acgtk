{
  open Abs
  open Term_parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*

  let symbol = ['!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '/' '<' '>' '?' '@' '[' '\\' ']' '^' '`' '{' '}' '~' ]


    rule lexer =
    parse
      | [' ' '\t'] {lexer lexbuf}
      | "(*" {comment 1 lexbuf}
      | eof {EOI}
      | ['\n'] {EOI}
      | ['('] {LPAREN}
      | [')'] {RPAREN}
      | ['.'] {DOT}
      | "lambda" {LAMBDA0}
      | "Lambda" {LAMBDA}
      | letter string {IDENT (Lexing.lexeme lexbuf)}
      | symbol {lexer lexbuf}
    and comment depth = parse
      | "*)" {if depth=1 then lexer lexbuf else if depth >1 then comment (depth-1) lexbuf else failwith "BUG"}
      | "(*" {comment (depth+1) lexbuf}
      | eof {raise Parsing.Parse_error}
      | _ {comment depth lexbuf}

