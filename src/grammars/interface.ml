open Abstract_syntax

module type Signature_sig =
sig
  exception Duplicate_type_definition
  exception Duplicate_term_definition

  type t
  type term
  val empty : (string*Abstract_syntax.location) -> t
  val name : t -> (string*Abstract_syntax.location)
  val add_entry : Abstract_syntax.sig_entry -> t -> t
  val is_type : string -> t -> bool
  val is_constant : string -> t -> bool*Abstract_syntax.syntactic_behavior option
  val add_warnings : Error.warning list -> t -> t
  val get_warnings : t -> Error.warning list
  val to_string : t -> string
  val term_to_string : term -> t -> string
  val convert : Abstract_syntax.term -> term
end

module type Lexicon_sig =
sig
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  type t
  type signature
  val empty : (string*Abstract_syntax.location) -> abs:signature -> obj:signature -> t
  val name : t -> (string*Abstract_syntax.location)
  val insert : Abstract_syntax.lex_entry -> t -> t
  val to_string : t -> string
end

