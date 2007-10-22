open Abstract_syntax
open Term_grammar


module Signature_grammar :
sig
  module type Signature_grammar =
  sig
    
    include Term_grammar.Term_grammar
      
    val signature : Abstract_sig.t Entry.e
      
  end
    
    
  module Make (G:Term_grammar.Term_grammar) : Signature_grammar
end
