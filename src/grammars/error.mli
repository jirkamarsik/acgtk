(** This module gives some types and some utilities to mange, emit and
    display messages *)


(** The type for errors raised by the lexer. Names should be explicit
*)
type lex_error =
  | Unstarted_comment of (Lexing.position * Lexing.position)
  | Unstarted_bracket of (Lexing.position * Lexing.position)
  | Mismatch_parentheses of (Lexing.position * Lexing.position)
  | Unclosed_comment  of (Lexing.position * Lexing.position)
  | Expect of (string * Lexing.position * Lexing.position)

(** The type for errors raised by the parser. Names should be explicit
*)
type parse_error =
  | Duplicated_term of (string * Lexing.position * Lexing.position)
  | Duplicated_type of (string * Lexing.position * Lexing.position)
  | Binder_expected of (string * Lexing.position * Lexing.position)
  | Unknown_constant of (string * Lexing.position * Lexing.position)
  | Unknown_type of (string * Lexing.position * Lexing.position)
  | Missing_arg_of_Infix of (string * Lexing.position * Lexing.position)

(** The types for errors raised by the typechecker. Names hould be
    explicit *)
type type_error =
  | Already_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_var of (string * Lexing.position * Lexing.position)
  | Not_defined_const of (string * Lexing.position * Lexing.position)
  | Not_well_typed_term of (string * Lexing.position * Lexing.position)
  | Not_well_kinded_type of (string * Lexing.position * Lexing.position)
  | Other of (Lexing.position * Lexing.position)


(** The types for errors raised by the environment. Names hould be
    explicit *)
type env_error =
  | Duplicated_signature of (string * Lexing.position * Lexing.position)

(** The type for errors *)
type error = 
  | Parse_error of parse_error
  | Lexer_error of lex_error
  | Type_error of type_error
  | Env_error of env_error

(** The type for warnings *)
type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)

(** The exception that should be raised when an error occur *)
exception Error of error

(** [update_loc lexbuf name] update the position informations for the
    lexer *)
val update_loc : Lexing.lexbuf -> string option -> unit

(** [error_msg e filename] returns a string describing the error [e]
    while the file [filename] is being processed *)
val error_msg : error -> string -> string

(** [error lexbuf filename] returns a string describing the location of
    the error while the file [filename] is being processed *)
val error : Lexing.lexbuf -> string -> string 

(** [warnings_to_string filname ws] returns a string describing the
    warnings anf their location for the file [filename] *)
val warnings_to_string : string -> warning list -> string
