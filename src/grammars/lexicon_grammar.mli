open Environment
open Abstract_syntax
open Signature_grammar

module Lexicon_grammar :
sig
  module type Lexicon_grammar =
  sig
    
    include Signature_grammar.Signature_grammar
      
    val lexicon : (Environment.t -> Abstract_lexicon.t) Entry.e
  end
    
  module Make (G:Signature_grammar.Signature_grammar) : Lexicon_grammar
end
