{
  open Term_parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|['_'])*

rule lexer = parse
  | [' ' '\t'] {lexer lexbuf}
  | ['\n'] {EOI}
  | ['('] {LPAREN}
  | [')'] {RPAREN}
  | ['.'] {DOT}
  | ["{lambda}"] {LAMBDA0}
  | ["{Lambda}"] {LAMBDA}
  | letter string {

