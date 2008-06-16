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


let infix_as_prefix = ref None
  
  
let set_infix l = infix_as_prefix := Some l
  
let unset_infix () = infix_as_prefix := None
  
let bad_infix_usage () =  !infix_as_prefix 


type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses
  | Unclosed_comment
  | Expect of string

type parse_error =
  | Duplicated_term of string
  | Duplicated_type of string
  | Binder_expected of string
  | Unknown_constant of string
  | Unknown_type of string
  | Missing_arg_of_Infix of string
  | No_such_signature of string
  | Dyp_error

type type_error =
  | Already_defined_var of string
  | Not_defined_var of string
  | Not_defined_const of string
  | Not_well_typed_term of string * string
  | Not_well_typed_term_plus of string * string * string
  | Not_well_kinded_type of string
  | Not_linear_LAbs of string
  | Other
  | Is_Used of string * string
  | Two_occurrences_of_linear_variable of (Lexing.position * Lexing.position)
  | Non_empty_context of (string*(Lexing.position * Lexing.position))
  | Not_normal
  | Vacuous_abstraction of (string * (Lexing.position * Lexing.position))


type env_error =
  | Duplicated_signature of string
  | Duplicated_lexicon of string

type error = 
  | Parse_error of parse_error * (Lexing.position * Lexing.position)
  | Lexer_error of lex_error * (Lexing.position * Lexing.position)
  | Type_error of type_error * (Lexing.position * Lexing.position)
  | Env_error of env_error * (Lexing.position * Lexing.position)

type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)

exception Error of error


let compute_comment_for_position pos1 pos2 =
  let line2 = pos2.Lexing.pos_lnum in
  let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
  let pos1 = pos1 in
  let line1 = pos1.Lexing.pos_lnum in
  let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
    if line1=line2 then
      Printf.sprintf "line %d, characters %d-%d" line2 col1 col2
    else
      Printf.sprintf "from l:%d, c:%d to l:%d,c:%d" line1 col1 line2 col2


let lex_error_to_string = function
  | Unstarted_comment -> "Syntax error: No comment opened before this closing of comment"
  | Unstarted_bracket -> "Syntax error: No bracket opened before this right bracket"
  | Unclosed_comment -> "Syntax error: Unclosed comment"
  | Mismatch_parentheses -> "Syntax error: Unclosed parenthesis"
  | Expect s -> Printf.sprintf "Syntax error: %s expected" s

let parse_error_to_string = function
  | Duplicated_type ty ->  Printf.sprintf "Syntax error: Type \"%s\" has already been defined" ty
  | Duplicated_term te ->  Printf.sprintf "Syntax error: Term \"%s\" has already been defined" te
  | Binder_expected id -> Printf.sprintf "Syntax error: Unknown binder \"%s\"" id
  | Unknown_constant id -> Printf.sprintf "Syntax error: Unknown constant \"%s\"" id
  | Unknown_type id -> Printf.sprintf "Syntax error: Unknown atomic type \"%s\"" id
  | Missing_arg_of_Infix  id -> Printf.sprintf "Syntax error: \"%s\" is defined as infix but used here with less than two arguments" id
  | No_such_signature s -> Printf.sprintf "Syntax error: Signature id \"%s\" not in the current< environment" s
  | Dyp_error -> "Dyp: Syntax error"

let type_error_to_string = function
  | Already_defined_var s ->
      Printf.sprintf "Var \"%s\" is already defined" s
  | Not_defined_var s -> 
      Printf.sprintf "Var \"%s\" is not defined" s
  | Not_defined_const s -> 
      Printf.sprintf "Const \"%s\" is not defined" s
  | Not_well_typed_term (s,typ) ->
      Printf.sprintf "Term \"%s\" not well typed.\nType expected : %s\n" s typ
  | Not_well_typed_term_plus (s,typ,wrong_typ) ->
      Printf.sprintf "Term \"%s\" not well typed.\n \"%s\" is of type %s but is here used with type  %s\n" s s typ wrong_typ
  | Not_well_kinded_type s ->
      Printf.sprintf "Type \"%s\" not well kinded" s
  | Not_linear_LAbs s ->
      Printf.sprintf "Var \"%s\" is supposed to be linear" s
  | Other -> "Not yet implemented"
  | Is_Used (s1,s2) -> Printf.sprintf "The type of this expression is \"%s\" but is used with type \"%s\"" s1 s2
  | Two_occurrences_of_linear_variable (s,e) -> Printf.sprintf "This linear variable was already used: %s" (compute_comment_for_position s e)
  | Non_empty_context (x,(s,e)) -> Printf.sprintf "This term has a non empty linear context (variable \"%s\" at %s)" x (compute_comment_for_position s e)
  | Not_normal -> "This term is not in normal form"
  | Vacuous_abstraction (x,(s,e)) -> Printf.sprintf "The linear variable \"%s\" is not abstracted over in term %s" x (compute_comment_for_position s e)

let env_error_to_string = function
  | Duplicated_signature s -> Printf.sprintf "Syntax error: Signature id \"%s\" is used twice in this environment" s
  | Duplicated_lexicon s -> Printf.sprintf "Syntax error: Lexion id \"%s\" is used twice in this environment" s

let warning_to_string w = 
  match w with
    | Variable_or_constant (s,pos1,pos2) -> Printf.sprintf "\"%s\" is a variable here, but is also declared as constant in the signature" s
	      
let error_msg e input_file =
  let msg,location_msg =
    match e with
      | Parse_error (er,(s,e)) -> parse_error_to_string er,compute_comment_for_position s e
      | Lexer_error (er,(s,e))  -> lex_error_to_string er,compute_comment_for_position s e
      | Type_error (er,(s,e)) -> type_error_to_string er,compute_comment_for_position s e
      | Env_error (er,(s,e)) -> env_error_to_string er,compute_comment_for_position s e in
    Printf.sprintf "File \"%s\", %s\n%s" input_file location_msg msg

let dyp_error lexbuf input_file =
(*  let pos1=Lexing.lexeme_start_p lexbuf in
  let pos2=lexbuf.Lexing.lex_curr_p in *)
  let pos1=Lexing.lexeme_start_p lexbuf in
  let pos2=Lexing.lexeme_end_p lexbuf in
    match bad_infix_usage () with
      | None -> Error (Parse_error (Dyp_error,(pos1,pos2)))
      | Some (sym,(s,e)) -> Error (Parse_error (Missing_arg_of_Infix sym,(s,e)))


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

    
