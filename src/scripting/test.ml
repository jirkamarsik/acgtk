open Functions

let () = Sys.catch_break true

module Lex = Lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module P = Script_parser.Make(E)



let welcome_msg = 
  "\n\t\t\tWelcome to the ACG toplevel\n\t\t\t\t©INRIA 2008\nPlease send your comments or bug reports or featrure requests to sylvain.pogodalla@loria.fr\n\n\nType\n\t\thelp ;\nto get help.\n\n\n\n"
  
let _ =
  let () = Printf.printf "%s%!" welcome_msg in
  let continue = ref true in
  let env = ref E.empty in
  let () = 
    while !continue do
      try
	env := P.parse_entry !env
      with
	| End_of_file
	| Sys.Break -> continue := false
    done in
    Printf.printf "\n%!"
  
