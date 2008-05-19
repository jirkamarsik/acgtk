type position = Lexing.position
    
let update_loc lexbuf file =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
    | None -> pos.Lexing.pos_fname
    | Some s -> s
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
			     Lexing.pos_fname = new_file;
			     Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
			     Lexing.pos_bol = pos.Lexing.pos_cnum;
			 }

type lex_error =
  | Unstarted_comment of (position * position)
  | Unstarted_bracket of (position * position)
  | Mismatch_parentheses of (position * position)
  | Unclosed_comment of (position * position)
  | Expect of (string * Lexing.position * Lexing.position)

type parse_error =
(*  | Illformed_term *)
  | Duplicated_term of (string * Lexing.position * Lexing.position)
  | Duplicated_type of (string * Lexing.position * Lexing.position)
  | Binder_expected of (string * Lexing.position * Lexing.position)
  | Unknown_constant of (string * Lexing.position * Lexing.position)
  | Unknown_type of (string * Lexing.position * Lexing.position)
  | Missing_arg_of_Infix of (string * Lexing.position * Lexing.position)
(*  | Infix_in_prefix_pos of (string * Lexing.position * Lexing.position) *)

type type_error =
  | Already_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_const of (string * Lexing.position * Lexing.position)
  | Not_well_typed_term of (string * Lexing.position * Lexing.position)
  | Not_well_kinded_type of (string * Lexing.position * Lexing.position)
  | Other of (Lexing.position * Lexing.position)


type env_error =
  | Duplicated_signature of (string * Lexing.position * Lexing.position)

type error = 
  | Parse_error of parse_error
  | Lexer_error of lex_error
  | Type_error of type_error
  | Env_error of env_error

type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)

exception Error of error

let lex_error_to_string = function
  | Unstarted_comment (_,_) -> "No comment opened before this closing of comment"
  | Unstarted_bracket (_,_) -> "No bracket opened before this right bracket"
  | Unclosed_comment (_,_) -> "Unclosed comment"
  | Mismatch_parentheses (_,_) -> "Unclosed parenthesis"
  | Expect (s,_,_) -> Printf.sprintf "%s expected" s

let parse_error_to_string = function
  | Duplicated_type (ty,_,_) ->  Printf.sprintf "Type \"%s\" has already been defined" ty
  | Duplicated_term (te,_,_) ->  Printf.sprintf "Term \"%s\" has already been defined" te
  | Binder_expected (id,_,_) -> Printf.sprintf "Unknown binder \"%s\"" id
  | Unknown_constant (id,_,_) -> Printf.sprintf "Unknown constant \"%s\"" id
  | Unknown_type (id,_,_) -> Printf.sprintf "Unknown atomic type \"%s\"" id
  | Missing_arg_of_Infix  (id,_,_) -> Printf.sprintf "\"%s\" is defined as infix but used here with less than two arguments" id

let type_error_to_string = function
  | Already_defined_var(s,_,_) ->
      Printf.sprintf "Var \"%s\" is already defined" s
  | Not_defined_var(s,_,_) -> 
      Printf.sprintf "Var \"%s\" is not defined" s
  | Not_defined_const(s,_,_) -> 
      Printf.sprintf "Const \"%s\" is not defined" s
  | Not_well_typed_term(s,_,_) ->
      Printf.sprintf "Term \"%s\" not well typed" s
  | Not_well_kinded_type(s,_,_) ->
      Printf.sprintf "Type \"%s\" not well kinded" s
  | Other(_,_) -> "Not yet implemented"

let env_error_to_string = function
  | Duplicated_signature (s,_,_) -> Printf.sprintf "Signature id \"%s\" is used twice" s

let error_to_string = function
  | Parse_error e -> parse_error_to_string e
  | Lexer_error e -> lex_error_to_string e
  | Type_error e -> type_error_to_string e
  | Env_error e -> env_error_to_string e

let warning_to_string w = 
  match w with
    | Variable_or_constant (s,pos1,pos2) -> Printf.sprintf "\"%s\" is a variable here, but is also declared as constant in the signature" s
	      
let error_msg e input_file =
  let msg = error_to_string e in
  let pos1,pos2 = match e with
  | Type_error (Already_defined_var(_,s,e)) -> s,e
  | Type_error (Not_defined_var(_,s,e)) -> s,e
  | Type_error (Not_defined_const(_,s,e)) -> s,e
  | Type_error (Not_well_typed_term(_,s,e)) -> s,e
  | Type_error (Not_well_kinded_type(_,s,e)) -> s,e
  | Type_error (Other(s,e)) -> s,e
  | Parse_error (Duplicated_term (_,s,e)) -> s,e
  | Parse_error (Duplicated_type (_,s,e)) -> s,e
  | Parse_error (Binder_expected (_,s,e)) -> s,e
  | Parse_error (Unknown_constant (_,s,e)) -> s,e
  | Parse_error (Unknown_type (_,s,e)) -> s,e
  | Parse_error (Missing_arg_of_Infix (_,s,e)) -> s,e
  | Lexer_error (Unclosed_comment (s,e)) -> s,e
  | Lexer_error (Mismatch_parentheses (s,e)) -> s,e
  | Lexer_error (Expect (_,s,e)) -> s,e
  | Lexer_error (Unstarted_bracket (s,e)) -> s,e
  | Lexer_error (Unstarted_comment (s,e)) -> s,e
  | Env_error (Duplicated_signature (_,s,e)) -> s,e in
  let line2 = pos2.Lexing.pos_lnum in
  let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
  let pos1 = pos1 in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "File \"%s\", line %d, characters %d-%d\nSyntax error: %s"
        input_file line2 col1 col2 msg
    else
      Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error: %s"
        input_file line1 col1 line2 col2 msg

let error lexbuf input_file =
  let line2 = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
  let col2 = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
  let pos1 = Lexing.lexeme_start_p lexbuf in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "File \"%s\", line %d, characters %d-%d\nSyntax error"
        input_file line2 col1 col2 
    else
      Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error"
        input_file line1 col1 line2 col2



(*  let emit_parse_error e = Error (Parse_error e) *)

  let emit_warning w input_file = 
    match w with
      | Variable_or_constant (s,pos1,pos2) -> 
	  let msg = warning_to_string w in
	  let line2 = pos2.Lexing.pos_lnum in
	  let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
	  let pos1 = pos1 in
	  let line1 = pos1.Lexing.pos_lnum in
	  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
	    if line1=line2 then
	      Printf.sprintf "File \"%s\", line %d, characters %d-%d\nWarning: %s"
		input_file line2 col1 col2 msg
	    else
	      Printf.sprintf "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nWarning: %s"
		input_file line1 col1 line2 col2 msg

  let warnings_to_string input_file ws = Utils.string_of_list "\n" (fun w -> emit_warning w input_file) ws

    
