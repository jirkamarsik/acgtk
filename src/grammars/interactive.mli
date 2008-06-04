(** This modules implements a functor that builds a testing module
when provided with an implementation for a signature module and a
lexicon module *)

module Make(Sg:Interface.Signature_sig)(Lex:Interface.Lexicon_sig with type signature = Sg.t) :
sig
  val main : unit -> unit
end
