open Diagram
open Environment

module Make (E : Environment_sig) : sig

  type lexicon = E.Lexicon.t
  type term = E.Signature1.term

  val realize_diagram : term -> lexicon list -> diagram

end
