let options = []

let usg_msg = ""


let parse filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
    try(
      let sgs = fst (List.hd (Parser.signatures Lexer.lexer lexbuf)) in
	List.iter (fun sg -> 
	  let x = Printf.printf "%s\n" 
	      (Abstract_syntax.Abstract_sig.to_string sg) 
	  in 
	  Printf.printf "\n\n" ; 
	  print_string "\nResultat typecheck : \n";
	  let Abstract_syntax.Abstract_sig.Signature(_,y) = sg in
	  let (_,_,t) = 
	    (Typechecker.typecheck 
	       y.Abstract_syntax.Abstract_sig.entries) in
	  print_string "[";
	  List.iter
	    (fun x -> (match x with
	      Abstract_syntax.Abstract_typ.Term_def(s,kd,wf,typ) -> 
		print_string (s^" = ");
		Typechecker.display_wfterm wf;
		print_string " : ";
		Typechecker.display_typ_tdef typ;
		print_string " ;\n ";
	    | Abstract_syntax.Abstract_typ.Term_decl(s,kd,typ) -> 
		print_string (s^" : ");
		Typechecker.display_typ_tdef typ;
		print_string " ;\n ";
	    | Abstract_syntax.Abstract_typ.Type_def(s,typ) -> 
		print_string (s^" : ");
		Typechecker.display_typ_tdef typ;
		print_string " ;\n ";
	    | Abstract_syntax.Abstract_typ.Type_decl
		(s,Abstract_syntax.Abstract_typ.K tdl) -> 
		print_string (s^" : K[");
		List.iter (Typechecker.display_typ_tdef) tdl;
		print_string "] ;\n ";))
	    t;
	  print_string"]\n";
	  x) sgs)
    with
      | Error.Error e -> Printf.fprintf stderr "Error: %s\n" (Error.error_msg e lexbuf "test.dat")
      | Dyp.Syntax_error -> Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf "test.dat")



let () = Arg.parse options parse usg_msg
