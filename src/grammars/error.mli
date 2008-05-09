type lex_error =
  | Unstarted_comment of (Lexing.position * Lexing.position)
  | Unstarted_bracket of (Lexing.position * Lexing.position)
  | Mismatch_parentheses of (Lexing.position * Lexing.position)
  | Unclosed_comment  of (Lexing.position * Lexing.position)
  | Expect of (string * Lexing.position * Lexing.position)

type parse_error =
(*  | Illformed_term *)
  | Duplicated_term of (string * Lexing.position * Lexing.position)
  | Duplicated_type of (string * Lexing.position * Lexing.position)
  | Binder_expected of (string * Lexing.position * Lexing.position)
  | Unknown_constant of (string * Lexing.position * Lexing.position)
  | Unknown_type of (string * Lexing.position * Lexing.position)
  | Missing_arg_of_Infix of (string * Lexing.position * Lexing.position)

type type_error =
  | Already_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_const of (string * Lexing.position * Lexing.position)
  | Not_well_typed_term of (string * Lexing.position * Lexing.position)
  | Not_well_kinded_type of (string * Lexing.position * Lexing.position)
  | Other of (Lexing.position * Lexing.position)

type error = 
  | Parse_error of parse_error
  | Lexer_error of lex_error
  | Type_error of type_error

type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)


exception Error of error

val update_loc : Lexing.lexbuf -> string option -> int -> bool -> int -> unit

val error_msg : error -> string -> string

val error : Lexing.lexbuf -> string -> string

val emit_parse_error : parse_error -> exn

val emit_warning : warning -> string -> string

val warnings_to_string : string -> warning list -> string
