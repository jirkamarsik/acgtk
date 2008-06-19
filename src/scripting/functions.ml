open Environment


module Make (E:Environment_sig) =
struct

  exception Not_yet_implemented

  type file_type = | Data | Script

  module Term_parser = Parser.Make(E)
    
  let load t filename e =
    match t with
      | Data -> Term_parser.parse_data filename e
      | Script -> raise Not_yet_implemented


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


    
  let analyse ?name e data=
    let entry =
      match name with
	| None -> E.focus e
	| Some n -> E.get n e
    in
      match entry with
	| E.Signature sg -> ignore (Term_parser.parse_term ~output:true data sg)
	| E.Lexicon lex -> 
	    let abs,obj=E.Lexicon.get_sig lex in
	      match Term_parser.parse_term ~output:true data abs with
		| None -> ()
		| Some (t,ty) -> 
		    let t',ty' = E.Lexicon.interpret t ty lex in
		      Printf.printf
			"Interpreted as:\n\t%s : %s\n%!"
			(E.Signature1.term_to_string t' obj)
			(E.Signature1.type_to_string ty' obj)
			
   let compose _ _ _ _ = raise Not_yet_implemented

end
    
