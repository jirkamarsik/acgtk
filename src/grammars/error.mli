(** This module gives some types and some utilities to mange, emit and
    display messages *)


(** The type for errors raised by the lexer. Names should be explicit
*)
type lex_error =
  | Unstarted_comment
  | Unstarted_bracket
  | Mismatch_parentheses
  | Unclosed_comment
  | Expect of string

(** The type for errors raised by the parser. Names should be explicit
*)
type parse_error =
  | Duplicated_term of string
  | Duplicated_type of string
  | Binder_expected of string
  | Unknown_constant of string
  | Unknown_type of string
  | Missing_arg_of_Infix of string
  | Dyp_error

(** The types for errors raised by the typechecker. Names hould be
    explicit *)
type type_error =
  | Already_defined_var of string
  | Not_defined_var of string
  | Not_defined_const of string
  | Not_well_typed_term of string * string
  | Not_well_kinded_type of string
  | Other


(** The types for errors raised by the environment. Names hould be
    explicit *)
type env_error =
  | Duplicated_signature of string

(** The type for errors *)
type error = 
  | Parse_error of parse_error * (Lexing.position * Lexing.position)
  | Lexer_error of lex_error * (Lexing.position * Lexing.position)
  | Type_error of type_error * (Lexing.position * Lexing.position)
  | Env_error of env_error * (Lexing.position * Lexing.position)

(** The type for warnings *)
type warning =
  | Variable_or_constant of (string * Lexing.position * Lexing.position)

(** The exception that should be raised when an error occur *)
exception Error of error

(** [update_loc lexbuf name] update the position informations for the
    lexer *)
val update_loc : Lexing.lexbuf -> string option -> unit

(** [set_infix sym] declares sym as a prefix symbol used in an infix
    position *)
val set_infix : string * (Lexing.position * Lexing.position) -> unit

(** [unset_infix ()] unset the current use of a prefix symbol used in
    an infix position *)
val unset_infix : unit -> unit

(** [error_msg e filename] returns a string describing the error [e]
    while the file [filename] is being processed *)
val error_msg : error -> string -> string

(** [dyp_error lexbuf filename] returns an exception {!Error.Error} so
    that it can be caught in a uniform way. [lexbuf] and [filename] are
    used to set correctly the location information of the parse error *)
val dyp_error : Lexing.lexbuf -> string -> exn

(** [warnings_to_string filname ws] returns a string describing the
    warnings anf their location for the file [filename] *)
val warnings_to_string : string -> warning list -> string
