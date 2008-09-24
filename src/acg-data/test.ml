module Sg = Signature.Sylvains_signature
(*module Lex = Syntactic_data_structures.Abstract_lex*)
module Lex = Lexicon.Sylvain_lexicon

module Test = Interactive.Make(Lex)


let () = Test.main ()
