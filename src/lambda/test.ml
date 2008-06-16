let options = []



module Sg = Syntactic_data_structures.Abstract_sig
(*module Sg= Sign.Sign*)
module Lex = Syntactic_data_structures.Abstract_lex

module Actual_env = Environment.Make(Lex)

let env = ref Actual_env.empty

module Actual_parser = Parser.Make(Actual_env)



let env = ref Actual_env.empty

let usg_msg = ""

let parse filename =
  let () = env := Actual_parser.parse_data filename !env in
  Actual_env.iter 
    (fun content ->
       match content with
	 | Actual_env.Lexicon _ -> ()
	 | Actual_env.Signature sg ->
	     let () = Printf.printf "\n\nResultat typecheck : \n" in
	     let (name,loc) = Actual_env.Signature1.name sg in
	       try
		 let t = (Typechecker.typecheck (name,loc,sg)) in
		   Printf.printf "%s\n"  (Display.Display.to_string t)
	       with
		 | Error.Error e -> Printf.fprintf stderr "Error: %s\n" (Error.error_msg e filename))
(*Printf.fprintf stderr "Error: in signature\"%s\"\n%s\n" name (Error.error_msg e name))*)
    !env
    
	
let () =  Arg.parse options parse usg_msg
