let options = []

let env = ref Abstract_syntax.Environment.empty

let usg_msg = ""


let print_sign t =
  let Sign.Sign.Signature (name,size,_,trie) = t in
  print_string (Sign.Display.to_string t);
  print_newline()

	
let parse filename =
  let () = env := Data_parsing.Data_parsing.data filename !env in
  Abstract_syntax.Environment.iter 
    (fun content ->
       match content with
	 | Abstract_syntax.Environment.Lexicon _ -> ()
	 | Abstract_syntax.Environment.Signature sg ->
	     let () = Printf.printf "\n\nResultat typecheck : \n" in
	     let y = Abstract_syntax.Abstract_sig.get_content sg in
	     let (name,_) = Abstract_syntax.Abstract_sig.name sg in
(* 	     print_string (Abstract_syntax.Abstract_sig.to_string sg); *)
	       try
		 let t = (Typechecker.typecheck (name,y)) in
		   print_sign t
	       with
		 | Error.Error e -> Printf.fprintf stderr "Error: in signature\"%s\"\n%s\n" name (Error.error_msg e name))
    !env
    
	
let () =  Arg.parse options parse usg_msg
