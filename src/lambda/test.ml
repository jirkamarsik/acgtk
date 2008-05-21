let options = []

let env = ref Abstract_syntax.Environment.empty

let usg_msg = ""


let print_sign (t,l) =
  let Sign.Sign.Signature (name,size,_,trie,content) = t in
  let list_decl = Tries.Tries.content trie in
    Printf.printf "Sign \"%s\": [\n" name;
    let list_res =
      List.fold_left 
	(fun l x -> (
	   match x with
	     | Sign.Sign.Term_def(s,i,kd,wf,typ) -> 
		 let new_l = s::l in
		   print_string (s^" =PP ");
		   print_string (Sign.Sign.pretty_print new_l wf);
		   print_string " : ";
		   Typechecker.display_typ_tdef t typ;
		   print_string " ;\n ";
		   new_l
	     | Sign.Sign.Term_decl(s,i,kd,typ) -> 
		 let new_l = s::l in
		   print_string (s^" : ");
		   Typechecker.display_typ_tdef t typ;
		   print_string " ;\n ";
		   new_l
	     | Sign.Sign.Type_def(s,i,typ) -> 
		 let new_l = s::l in
		   print_string (s^" : ");
		   Typechecker.display_typ_tdef t typ;
		   print_string " ;\n ";
		   new_l
	     | Sign.Sign.Type_decl(s,i,Lambda.Lambda.K tdl) -> 
		 let new_l = s::l in
		   print_string (s^" : K[");
		   List.iter (Typechecker.display_typ_tdef t) tdl;
		   print_string "] ;\n ";
		   new_l))
	[] 
	list_decl in
      print_string"]\n"
	
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
	       try
		 let (t,l) = (Typechecker.typecheck (name,y)) in
		   print_sign (t,l)
	       with
		 | Error.Error e -> Printf.fprintf stderr "Error: in signature\"%s\"\n%s\n" name (Error.error_msg e name))
    !env
    
	
let () =  Arg.parse options parse usg_msg
