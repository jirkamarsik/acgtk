open Functions

let () = Sys.catch_break true

module Lex = Lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module P = Script_parser.Make(E)



let welcome_msg = 
  "\n\t\t\tWelcome to the ACG toplevel\n\t\t\t\t�INRIA 2008\nPlease send your comments or bug reports or featrure requests to sylvain.pogodalla@loria.fr\n\n\nType\n\t\thelp ;\nto get help.\n\n\n\n"


let env = ref E.empty

let anon_fun s = env := P.parse_file  s !env
  
let _ =
  let () = Printf.printf "%s%!" welcome_msg in
  let () = Arg.parse [] anon_fun "Expected arguments are script files" in
  let continue = ref true in
  let () = 
    while !continue do
      try
	env := P.parse_entry !env
      with
	| End_of_file
	| Sys.Break -> continue := false
    done in
    Printf.printf "\n%!"
  
