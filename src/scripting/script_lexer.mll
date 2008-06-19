{
  open Abstract_syntax
(*  open Error*)
(*  open Script_parser*)


	(*      | STRING of (string*Abstract_syntax.location)*)


type token =
  | EOII
  | LOAD_DATA of (string*Abstract_syntax.location)
  | LOAD_SCRIPT of (string*Abstract_syntax.location)
  | LIST
  | SELECT
  | UNSELECT
  | TRACE
  | PRINT
  | ANALYSE  of (string*Abstract_syntax.location)
  | COMPOSE
  | SEMICOLONN
  | AS
  | IDENTT of (string*Abstract_syntax.location)


  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
  let string_content = Buffer.create 16

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let string = (letter|digit|'_')*


  rule lexer =
  parse
    | [' ' '\t'] {lexer lexbuf}
    | newline {let () = Error.update_loc lexbuf None in lexer lexbuf}
    | eof {EOII}
    | "#" {comment lexer lexbuf}
    | [';'] {SEMICOLONN}
    | "load" {(*let () = Printf.fprintf stderr "Entering load commad\n%!" in*) let t = load_options lexbuf  in t}
    | "list" {LIST}
    | "select" {SELECT}
    | "unselect" {UNSELECT}
    | "trace" {TRACE}
    | "print" {PRINT}
    | "analyse" {let () = Buffer.reset string_content in
		string (fun x l -> ANALYSE (x,l)) lexbuf}
    | "compose" {COMPOSE}
    | "as" {AS}
    | letter string {IDENTT (Lexing.lexeme lexbuf,loc lexbuf)}
  and comment f_parser = parse
    | newline {f_parser lexbuf}
    | _ {comment f_parser lexbuf}
  and string f = parse
    | ";" {(*let () = Printf.fprintf stderr "Read the \"%s\" filename\n%!" (Buffer.contents string_content) in*) f (Buffer.contents string_content) (loc lexbuf)}
    | "#" {comment (string f) lexbuf}
    | _ as c {(*let () = Printf.fprintf stderr "Addind the \'%c\' char\n%!" c in *)
	      let () = Buffer.add_char string_content c in string f lexbuf}
  and string_wo_space f = parse
    | ";" {f (Buffer.contents string_content) (loc lexbuf)}
    | "#" {comment (string_wo_space f) lexbuf}
    | [' ' '\t'] {string_wo_space f lexbuf}
    | _ as c {let () = Buffer.add_char string_content c in string f lexbuf}
  and load_options = parse
    | [' ' '\t'] {load_options lexbuf}
    | newline {let () = Error.update_loc lexbuf None in load_options lexbuf}
    | eof {EOII}
    | "#" {comment load_options lexbuf}
    | "data" {let () = Buffer.reset string_content in
	      (*let () = Printf.fprintf stderr "Read data option\n%!" in*)
		string_wo_space (fun x l -> LOAD_DATA (x,l)) lexbuf}
    | "d" {let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_DATA (x,l)) lexbuf}
    | "script" {let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_SCRIPT (x,l)) lexbuf}
    | "s" {let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_SCRIPT (x,l)) lexbuf}

