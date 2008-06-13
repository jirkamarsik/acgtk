(** This modules implements a functor that builds a testing module
when provided with an implementation for a signature module and a
lexicon module *)

module Make(Lex:Interface.Lexicon_sig) :
sig
  val main : unit -> unit
end
