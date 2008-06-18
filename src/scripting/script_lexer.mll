{
  open Abstract_syntax
  open Script_parser


  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
}


let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*


  rule lexer =
  parse
    | [' ' '\t'] {lexer lexbuf}
    | newline {let () = Error.update_loc lexbuf None in lexer lexbuf}
    | eof {EOI}
    | "#" {comment lexer}
    | [';'] {let () = check_brackets () in
	       SEMICOLON(loc lexbuf)}
    | "load" {}
    | "list" {}
    | "select" {}
    | "unselect" {}
    | "trace" {}
    | "print" {}
    | "analyse"
    | letter string {IDENT (Lexing.lexeme lexbuf,loc lexbuf)}
  and comment parser = parse
    | newline {parser lexbuf}
    | _ {comment lexbuf}
  and string f = parse
    | ";" {f (loc lexbuf) (Buffer.contents string_content),}
    | "#" {comment string}

