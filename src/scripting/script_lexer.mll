{
  open Abstract_syntax
(*  open Error*)
(*  open Script_parser*)


	(*      | STRING of (string*Abstract_syntax.location)*)


type token =
  | EOII
  | LOAD_DATA of (string*Abstract_syntax.location*string)
  | LOAD_SCRIPT of (string*Abstract_syntax.location*string)
  | LOAD_HELP
  | LIST
  | SELECT
  | UNSELECT
  | TRACE
  | PRINT   of Abstract_syntax.location
  | ANALYSE  of (string*Abstract_syntax.location*string)
  | ADD of (string*Abstract_syntax.location*string)
  | COMPOSE
  | SEMICOLONN of string
  | AS
  | DONT
  | WAIT
  | IDENTT of (string*Abstract_syntax.location)
  | HELP
  | CREATE_SIG
  | CREATE_LEX
  | CREATE_HELP
  | SAVE  of (string*Abstract_syntax.location*string)


  let loc lexbuf = Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf
  let string_content = Buffer.create 16

  let echo_content = Buffer.create 32
    
  let space_added = ref true


  let add_space () =
    let () = if !space_added then () else Buffer.add_char echo_content ' ' in
      space_added := false
	
	
  let echo_str s = let () = add_space () in Buffer.add_string echo_content s
  let echo_chr s = let () = add_space () in Buffer.add_char echo_content s
  let reset_echo () = 
    let s = Buffer.contents echo_content in
    let () = Buffer.reset echo_content in
    let () = space_added := true in
      s
      
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
    | [';'] as c {let () = echo_chr c  in let s = reset_echo () in SEMICOLONN s}
    | "load" as c {let () = echo_str c in let t = load_options lexbuf in t}
    | "create" as c {let () = echo_str c in let t = create_options lexbuf in t}
    | "list"  as c {let () = echo_str c in LIST}
    | "select"  as c {let () = echo_str c in SELECT}
    | "unselect"  as c {let () = echo_str c in UNSELECT}
    | "trace"  as c {let () = echo_str c in TRACE}
    | "help"  as c {let () = echo_str c in HELP}
    | "print"  as c {let () = echo_str c in PRINT (loc lexbuf)}
    | "analyse"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		string (fun x l -> ANALYSE (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "add"  as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> ADD (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "compose"  as c {let () = echo_str c in COMPOSE}
    | "don't"  as c {let () = echo_str c in DONT}
    | "wait"  as c {let () = echo_str c in WAIT}
    | "as"  as c {let () = echo_str c in AS}
    | "save" as c {let () = echo_str c in let () = Buffer.reset string_content in
		     string_wo_space (fun x l -> SAVE (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | letter string  as c {let () = echo_str c in IDENTT (Lexing.lexeme lexbuf,loc lexbuf)}
    | _ {raise (Scripting_errors.Error (Scripting_errors.Command_expected,loc lexbuf))}
  and comment f_parser = parse
    | newline {f_parser lexbuf}
    | _ {comment f_parser lexbuf}
  and string f = parse
    | ";" {f (Buffer.contents string_content) (loc lexbuf)}
    | "#" {comment (string f) lexbuf}
    | newline {let () = Error.update_loc lexbuf None in string f lexbuf}
    | _ as c {let () = Buffer.add_char string_content c in string f lexbuf}
  and string_wo_space f = parse
    | ";" {f (Buffer.contents string_content) (loc lexbuf)}
    | "#" {comment (string_wo_space f) lexbuf}
    | [' ' '\t'] {string_wo_space f lexbuf}
    | newline {let () = Error.update_loc lexbuf None in string_wo_space f lexbuf}
    | _ as c {let () = Buffer.add_char string_content c in string f lexbuf}
  and load_options = parse
    | [' ' '\t'] {load_options lexbuf}
    | newline {let () = Error.update_loc lexbuf None in load_options lexbuf}
    | eof {EOII}
    | "help" {LOAD_HELP}
    | "#" {comment load_options lexbuf}
    | "data" as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_DATA (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "d" as c {let () = echo_chr c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_DATA (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "script" as c {let () = echo_str c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_SCRIPT (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | "s" as c {let () = echo_chr c in let () = Buffer.reset string_content in
		string_wo_space (fun x l -> LOAD_SCRIPT (x,l,let () = echo_str (x^";") in reset_echo ())) lexbuf}
    | _ {raise (Scripting_errors.Error (Scripting_errors.Missing_option Scripting_errors.Load,loc lexbuf))}
  and create_options = parse
    | [' ' '\t'] {create_options lexbuf}
    | newline {let () = Error.update_loc lexbuf None in create_options lexbuf}
    | eof {EOII}
    | "help" {CREATE_HELP}
    | "#" {comment create_options lexbuf}
    | "s" as c {let () = echo_chr c in let () = Buffer.reset string_content in CREATE_SIG}
    | "sig" as c {let () = echo_str c in let () = Buffer.reset string_content in CREATE_SIG}
    | "l" as c {let () = echo_chr c in let () = Buffer.reset string_content in CREATE_LEX}
    | "lex" as c {let () = echo_str c in let () = Buffer.reset string_content in CREATE_LEX}



