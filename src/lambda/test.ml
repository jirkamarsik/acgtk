let options = []

let env = ref Abstract_syntax.Environment.empty

let usg_msg = ""


let parse filename =
  env := Data_parsing.Data_parsing.data filename !env


let term_parsing env =
(*  let sgs = Data_parsing.Data_parsing.signature filename in*)
    Abstract_syntax.Environment.iter 
      (fun content ->
	   let () = Printf.printf "\n\nResultat typecheck : \n" in
	   let Abstract_syntax.Environment.Signature(sg) = 
	     content in
	 try
	   let Abstract_syntax.Abstract_sig.Signature((name,_),y) = sg in
	   let (t,l) = (Typechecker.typecheck (name,y)) in
	   let Sign.Sign.Signature (name,size,_,trie,content) = t in
	   let list_decl = Tries.Tries.content trie in
	     Printf.printf "Sign \"%s\": [\n" name;
	     List.iter
	       (fun x -> (match x with
			      Sign.Sign.Term_def(s,i,kd,wf,typ) -> 
				print_string (s^" =PP ");
				print_string (Sign.Sign.pretty_print wf);
				print_string " : ";
				Typechecker.display_typ_tdef t typ;
				print_string " ;\n ";
			    | Sign.Sign.Term_decl(s,i,kd,typ) -> 
				print_string (s^" : ");
				Typechecker.display_typ_tdef t typ;
				print_string " ;\n ";
			    | Sign.Sign.Type_def(s,i,typ) -> 
				print_string (s^" : ");
				Typechecker.display_typ_tdef t typ;
				print_string " ;\n ";
			    | Sign.Sign.Type_decl
				(s,i,Lambda.Lambda.K tdl) -> 
				print_string (s^" : K[");
				  List.iter (Typechecker.display_typ_tdef t) tdl;
				  print_string "] ;\n ";))
	       list_decl;
	     print_string"]\n"
	 with
	   | Error.Error e -> 
	       let name,_ = Abstract_syntax.Abstract_sig.name sg in
	       let () = Printf.fprintf stderr "Error: in signature\"%s\"\n" (Error.error_msg e name) in
		 ())
(*      sgs*) env
	  
	  
(* let () = Arg.parse options parse usg_msg *)
	
let () =
  let () = Arg.parse options parse usg_msg in
    term_parsing !env
