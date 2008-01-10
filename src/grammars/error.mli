type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses of (Lexing.position * Lexing.position)
  | Unclosed_comment  of (Lexing.position * Lexing.position)

type parse_error =
  | Illformed_term
  | Duplicated_term of (string * Lexing.position * Lexing.position)
  | Duplicated_type of (string * Lexing.position * Lexing.position)
  | Binder_expected of (string * Lexing.position * Lexing.position)
  | Unknown_constant of (string * Lexing.position * Lexing.position)
  | Unknown_type of (string * Lexing.position * Lexing.position)

type type_error =
  | Already_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_var of (string * Lexing.position * Lexing.position)
  | Other

type error = 
  | Parse_error of parse_error
  | Lexer_error of lex_error
  | Type_error of type_error


exception Error of error

val update_loc : Lexing.lexbuf -> string option -> int -> bool -> int -> unit

val error_msg : error -> Lexing.lexbuf -> string -> string

val error : Lexing.lexbuf -> string -> string
