type position = Lexing.position
    
let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
    | None -> pos.Lexing.pos_fname
    | Some s -> s
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
			     Lexing.pos_fname = new_file;
			     Lexing.pos_lnum = if absolute then line else pos.Lexing.pos_lnum + line;
			     Lexing.pos_bol = pos.Lexing.pos_cnum - chars;
			 }

type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses of (position * position)
  | Unclosed_comment of (position * position)

type parse_error =
  | Illformed_term
  | Duplicated_term of (string * Lexing.position * Lexing.position)
  | Duplicated_type of (string * Lexing.position * Lexing.position)
  | Binder_expected of (string * Lexing.position * Lexing.position)
  | Unknown_constant of (string * Lexing.position * Lexing.position)
  | Unknown_type of (string * Lexing.position * Lexing.position)

type error = 
  | Parse_error of parse_error
  | Lexer_error of lex_error

exception Error of error

let lex_error_to_string = function
  | Unstarted_comment -> "No comment opened before this closing of comment"
  | Unstarted_bracket -> "No bracket opened before this right bracket"
  | Unclosed_comment (_,_) -> "Unclosed comment "
(*      let line2 = p2.Lexing.pos_lnum in
      let col2 = p2.Lexing.pos_cnum - p2.Lexing.pos_bol in
      let line1 = p1.Lexing.pos_lnum in
      let col1 = p1.Lexing.pos_cnum - p1.Lexing.pos_bol in
	if line1=line2 then
	  Printf.sprintf "Unclosed comment starting at line %d characters %d-%d\n"
             line2 col1 col2
	else
	  Printf.sprintf "Unclosed comment starting from l:%d, c:%d to l:%d,c:%d\n"
            line1 col1 line2 col2 *)
  | Mismatch_parentheses (_,_) -> "Unclosed parenthesis"
(*      let line2 = p2.Lexing.pos_lnum in
      let col2 = p2.Lexing.pos_cnum - p2.Lexing.pos_bol in
      let line1 = p1.Lexing.pos_lnum in
      let col1 = p1.Lexing.pos_cnum - p1.Lexing.pos_bol in
	if line1=line2 then
	  Printf.sprintf "Unclosed bracket starting at line %d characters %d-%d\n"
             line2 col1 col2
	else
	  Printf.sprintf "Unclosed bracket starting from l:%d, c:%d to l:%d,c:%d\n"
            line1 col1 line2 col2 *)

let parse_error_to_string = function
  | Illformed_term -> "Ill-formed term"
  | Duplicated_type (ty,_,_) ->  Printf.sprintf "Type \"%s\" has already been defined\n" ty
  | Duplicated_term (te,_,_) ->  Printf.sprintf "Term \"%s\" has already been defined\n" te
  | Binder_expected (id,_,_) -> Printf.sprintf "Unknown binder \"%s\"\n" id
  | Unknown_constant (id,_,_) -> Printf.sprintf "Unknown constant \"%s\"\n" id
  | Unknown_type (id,_,_) -> Printf.sprintf "Unknown atomic type \"%s\"\n" id

let error_to_string = function
  | Parse_error e -> parse_error_to_string e
  | Lexer_error e -> lex_error_to_string e

let error_msg e lexbuf input_file =
  let msg = error_to_string e in
  let pos1,pos2 = match e with
    | Parse_error Illformed_term -> Lexing.lexeme_start_p lexbuf,lexbuf.Lexing.lex_curr_p 
    | Parse_error (Duplicated_term (_,s,e)) -> s,e
    | Parse_error (Duplicated_type (_,s,e)) -> s,e
    | Parse_error (Binder_expected (_,s,e)) -> s,e
    | Parse_error (Unknown_constant (_,s,e)) -> s,e
    | Parse_error (Unknown_type (_,s,e)) -> s,e
    | Lexer_error (Unclosed_comment (s,e)) -> s,e
    | Lexer_error (Mismatch_parentheses (s,e)) -> s,e
    | Lexer_error Unstarted_bracket -> Lexing.lexeme_start_p lexbuf,lexbuf.Lexing.lex_curr_p 
    | Lexer_error Unstarted_comment -> Lexing.lexeme_start_p lexbuf,lexbuf.Lexing.lex_curr_p in
  let line2 = pos2.Lexing.pos_lnum in
  let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
  let pos1 = pos1 in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "File \"%s\", line %d, characters %d-%d\nSyntax error: %s\n"
        input_file line2 col1 col2 msg
    else
      Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error: %s\n"
        input_file line1 col1 line2 col2 msg

let error lexbuf input_file =
  let line2 = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let col2 = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
  let pos1 = Lexing.lexeme_start_p lexbuf in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "File \"%s\", line %d, characters %d-%d\nSyntax error\n"
        input_file line2 col1 col2
    else
      Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error\n"
        input_file line1 col1 line2 col2

