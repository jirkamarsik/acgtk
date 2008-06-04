open Abstract_syntax


(* First build actual implementations of signatures and lexicon. Here,
   the signature and the lexicon are just the ones with the syntactic
   trees of the terms and the types *)
module Actual_sig = Syntactic_data_structures.Abstract_sig
module Actual_lex = Syntactic_data_structures.Abstract_lex
  
  
(* Build accordingly a Test module *)
module Test=Interactive.Make(Actual_sig)(Actual_lex)
  
(* And run it *)
let () = Test.main()

