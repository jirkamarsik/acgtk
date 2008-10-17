open Functions

let () = Sys.catch_break true


let dirs = ref [""]


let options =
  [
    ("-I", Arg.String (fun dir -> dirs := (!dirs)@[dir]) , " -I dir sets dir as a directory in which file arguments can be looked for")
  ]

let usg_msg = Printf.sprintf "%s [options] file1 file2 ...\n\nThis will parse the files which are supposed to be files acripting commands and then run the ACG command interpreter." Sys.executable_name


module Lex = Lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module P = Script_parser.Make(E)



let welcome_msg = 
  "\n\t\t\tWelcome to the ACG toplevel\n\t\t\t\t©INRIA 2008\nPlease send your comments or bug reports or featrure requests to sylvain.pogodalla@loria.fr\n\n\nType\n\t\thelp ;\nto get help.\n\n\n\n"


let env = ref E.empty

let anon_fun s = env := P.parse_file  s !dirs !env
  
let _ =
  let () = Arg.parse options anon_fun usg_msg in
  let () = Printf.printf "%s%!" welcome_msg in
  let continue = ref true in
  let () = 
    while !continue do
      try
	env := P.parse_entry !dirs !env
      with
	| End_of_file
	| Sys.Break -> continue := false
    done in
    Printf.printf "\n%!"
  
