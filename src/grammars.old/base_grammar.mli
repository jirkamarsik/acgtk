(** This modules implements the basic grammar that is used for the
  parsong of signtures and lexicon *)

module Base_grammar :
sig
  (** The requirement for a grammar to be basic. *)
  module type Base_grammar =
  sig
    (** A special entry, useful when we don't want to distinguish upper
	and lower identiders *)
    include Grammar.S with type te = Token.t
    exception Parse_Error of string
    val luident : string Entry.e
    val luident_loc : (string*Token.flocation) Entry.e
    val symbol : string list Entry.e
  end
    
    
  (** A module that implements a basic grammar *)
  module Make (G:Grammar.S with type te = Token.t) : Base_grammar
end
