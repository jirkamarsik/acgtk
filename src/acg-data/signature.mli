open Abstract_syntax
open Lambda


type sig_entry =
  | Type_declaration of string * int * Lambda.kind
  | Type_definition of string * int * Lambda.kind * Lambda.stype
  | Term_declaration of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype
  | Term_definition of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype * Lambda.term
      
      
module Sylvains_signature  : Interface.Signature_sig 
  with
    type term = Lambda.term
  and type stype = Lambda.stype 
  and type entry = sig_entry
