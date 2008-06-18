open Functions

(*module Sg = Signature.Sylvains_signature*)
module Lex = Lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module F=Functions.Make(E)

open F

let e = ref E.empty

let () = e:= (load Data "../data/lex1.dat" !e)

let () = list !e

let () = e:= select "absobj" !e

let () = print !e

let () = print ~name:"abs" !e

let () = analyse !e "lambda x.t x:a->a"


