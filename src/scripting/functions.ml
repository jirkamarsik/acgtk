open Environment


module Make (E:Environment_sig) =
struct

  exception Not_yet_implemented

  type file_type = | Data | Script of (string -> E.t -> E.t)

  module Term_parser = Parser.Make(E)
    
  let load t filename e =
    match t with
      | Data -> Term_parser.parse_data filename e
      | Script f  -> f filename e


  let list e =
    Printf.printf "Available data:\n%s\n"
      (Utils.string_of_list
	 "\n"
	 (fun x -> x)
	 (E.fold
	    (fun d a -> 
	       match d with
		 | E.Signature sg -> (Printf.sprintf "\tSignature\t%s" (fst (E.Signature1.name sg)))::a
		 | E.Lexicon lx -> (Printf.sprintf "\tLexicon\t\t%s" (fst (E.Lexicon.name lx)))::a)
	    []
	    e))

  let select = E.select 

  let unselect = E.unselect


  let trace () = raise Not_yet_implemented

  let print ?name e =
    let entry =
      match name with
	| None -> E.focus e
	| Some n -> E.get n e
    in
      match entry with
	| E.Signature sg -> Printf.printf "%s\n" (E.Signature1.to_string sg)
	| E.Lexicon lex -> Printf.printf "%s\n" (E.Lexicon.to_string lex)


    
  let analyse ?names e ?offset data=
    let entries =
	match names with
	  | None -> [E.focus e]
	  | Some ns -> List.map (fun (n,l) -> 
				   try 
				     E.get n e
				   with
				     | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l))) ns
in
    let _ = List.fold_left
      (fun first entry -> match entry with
	 | E.Signature sg -> 
	     let () = Printf.printf "In %s: \t" (fst (E.Signature1.name sg)) in
	     let () = ignore (Term_parser.parse_term ~output:true ?offset data sg) in
	       false
	 | E.Lexicon lex -> 
	     let abs,obj=E.Lexicon.get_sig lex in
	       match Term_parser.parse_term ~output:first ?offset data abs with
		 | None -> false
		 | Some (t,ty) -> 
		     let t',ty' = E.Lexicon.interpret t ty lex in
		     let () = Printf.printf
		       "Interpreted in %s as:\n\t%s : %s\n%!"
		       (fst (E.Lexicon.name lex))
		       (E.Signature1.term_to_string t' obj)
		       (E.Signature1.type_to_string ty' obj) in
		       false)
      true
      entries in
      Printf.printf "\n%!"
		
	
	   
  let compose n1 n2 n3 e =
    let get_lex (n,l) =
      try
	E.get_lexicon n e
      with
	  E.Lexicon_not_found s -> raise (Scripting_errors.Error (Scripting_errors.No_such_lexicon s,l)) in
    let lex1 = get_lex n1 in
    let lex2 = get_lex n2 in
      E.insert (E.Lexicon (E.Lexicon.compose lex1 lex2 n3)) e
    
    
end
    
