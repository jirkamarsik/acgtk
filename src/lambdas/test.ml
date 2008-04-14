let options = []

let usg_msg = ""


let parse filename =
  let sgs = Data_parsing.Data_parsing.signature filename in
    Abstract_syntax.Environment.Env.iter 
      (fun _ sg ->
	 let () = Printf.printf "\n\nResultat typecheck : \n" in
	 let Abstract_syntax.Abstract_sig.Signature(name,y) = sg in
	 let t = (Typechecker.typecheck (name,y)) in
	 let Signature.Abstract_typ.Signature (name,size,_,trie,content) = t in
	 let list_decl = Tries.Tries.content trie in
	   Printf.printf "Signature \"%s\": [\n" name;
	   List.iter
	     (fun x -> (match x with
			    Signature.Abstract_typ.Term_def(s,i,kd,wf,typ) -> 
			      print_string (s^" = ");
			      Typechecker.display_wfterm t wf;
			      print_string " : ";
			      Typechecker.display_typ_tdef t typ;
			      print_string " ;\n ";
			  | Signature.Abstract_typ.Term_decl(s,i,kd,typ) -> 
			      print_string (s^" : ");
			      Typechecker.display_typ_tdef t typ;
			      print_string " ;\n ";
			  | Signature.Abstract_typ.Type_def(s,i,typ) -> 
			      print_string (s^" : ");
			      Typechecker.display_typ_tdef t typ;
			      print_string " ;\n ";
			  | Signature.Abstract_typ.Type_decl
			      (s,i,Signature.Abstract_typ.K tdl) -> 
			      print_string (s^" : K[");
				List.iter (Typechecker.display_typ_tdef t) tdl;
				print_string "] ;\n ";))
	     list_decl;
	   print_string"]\n")
      sgs
	  
	  
let () = Arg.parse options parse usg_msg
