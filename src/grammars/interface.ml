open Abstract_syntax

module type Signature_sig =
sig
  exception Duplicate_type_definition
  exception Duplicate_term_definition


  type sig_entry = Abstract_sig.sig_entry
  type syntactic_behavior = Abstract_sig.term_kind


  type t
  val empty : t
  val add_entry : sig_entry -> t -> t
  val is_type : string -> t -> bool
  val is_constant : string -> t -> bool*syntactic_behavior
end

module type Lexicon_sig =
sig
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  type lex_entry = Abstract_lex.interpretation

  type t
  val empty : t
  val insert : lex_entry -> t -> t
end

module type Environment_sig = functor (Sig:Signature_sig) -> functor (Lex:Lexicon_sig) ->
sig
  exception Signature_not_found

  type t
  type env_entry = 
    | Signature of Sig.t
    | Lexicon of Lex.t
  val empty : t
  val insert : string -> env_entry -> t -> t
  val get_signature : string -> t -> Sig.t
end


