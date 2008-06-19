open Functions

(*module Sg = Signature.Sylvains_signature*)
module Lex = Lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module P=Script_parser.Make(E)

(*let _ = P.parse_file "../data/script" E.empty) *)
  
let _ =
  let continue = ref true in
  let env = ref E.empty in
    while !continue do
      try
	env := P.parse_entry !env
      with
	| End_of_file -> continue := false
    done
  
