open Environment



module Make (E:Environment_sig) =
struct

  exception Not_yet_implemented of string

  let interactive = ref false

  type file_type = | Data | Script of (string -> E.t -> E.t)

  module Data_parser = Parser.Make(E)
    
  let load t filename e =
    match t with
      | Data -> Data_parser.parse_data ~override:true filename e
      | Script f  -> f filename e


  let list e =
    Format.printf "Available data:\n%s\n%!"
      (Utils.string_of_list
	 "\n"
	 (fun x -> x)
	 (E.fold
	    (fun d a -> 
	       match d with
		 | E.Signature sg -> (Format.sprintf "\tSignature\t%s%!" (fst (E.Signature1.name sg)))::a
		 | E.Lexicon lx -> (Format.sprintf "\tLexicon\t\t%s%!" (fst (E.Lexicon.name lx)))::a)
	    []
	    e))


  let select n l e =
    try
	E.select n e
    with
      | E.Signature_not_found n
      | E.Lexicon_not_found n
      | E.Entry_not_found n -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment n,l))

  let unselect = E.unselect


  let trace () = raise (Not_yet_implemented "trace")
  let dont_trace () = raise (Not_yet_implemented "don't trace")

  let print ?name e l =
    try
      let entry =
	match name with
	  | None -> E.focus e
	  | Some n -> E.get n e
      in
	match entry with
	  | E.Signature sg -> Format.printf "%s\n%!" (E.Signature1.to_string sg)
	  | E.Lexicon lex -> Format.printf "%s\n%!" (E.Lexicon.to_string lex)
    with
      | E.Signature_not_found n
      | E.Lexicon_not_found n
      | E.Entry_not_found n -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment n,l))



  let in_sg sg = Format.fprintf Format.err_formatter "in signature %s\n%!" (fst (E.Signature1.name sg))

    
  let analyse ?names e ?offset data l =
    try
      let entries =
	match names with
	  | None -> [E.focus e]
	  | Some ns -> List.map (fun (n,l) -> 
				   try 
				     E.get n e
				   with
				     | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l))) ns in
    let _ = List.fold_left
      (fun (first,last_abs_sg) entry -> match entry with
	 | E.Signature sg -> 
	     (match last_abs_sg with
	       | Some previous_sg when (E.Signature1.name sg) = (E.Signature1.name previous_sg) -> (false,last_abs_sg)
	       | _ ->
		   let () = if first then Format.printf "In %s:\n\t%!" (fst (E.Signature1.name sg)) else () in
		   (match Data_parser.parse_term ~output:true ?offset data sg with
		      | None -> let () = in_sg sg in false, Some sg
		      | Some _ -> false,None))
	 | E.Lexicon lex -> 
	     let abs,obj=E.Lexicon.get_sig lex in
	       match last_abs_sg with
		 |  Some previous_sg when (E.Signature1.name abs) = (E.Signature1.name previous_sg) -> (false,last_abs_sg)
		 | _ -> let () = if first then Format.printf "In %s:\n\t%!" (fst (E.Signature1.name abs)) else () in
		     match Data_parser.parse_term ~output:first ?offset data abs with
		       | None -> false,Some abs
		       | Some (t,ty) -> 
			   let t',ty' = E.Lexicon.interpret t ty lex in
			   let () = Format.printf
			     "Interpreted by %s in %s as:\n\t%s : %s\n%!"
			     (fst (E.Lexicon.name lex))
			     (fst (E.Signature1.name obj))
			     (E.Signature1.term_to_string t' obj)
			     (E.Signature1.type_to_string ty' obj) in
			     false,None)
      (true,None)
      entries in
      Format.printf "\n%!"
    with
      | E.Signature_not_found n
      | E.Lexicon_not_found n
      | E.Entry_not_found n -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment n,l))
		
	
	   
  let compose n1 n2 n3 e =
    let get_lex (n,l) =
      try
	E.get_lexicon n e
      with
	  E.Lexicon_not_found s -> raise (Scripting_errors.Error (Scripting_errors.No_such_lexicon s,l)) in
    let lex1 = get_lex n1 in
    let lex2 = get_lex n2 in
    let () = Format.printf "%s = %s o %s\n%!" (fst n3) (fst n1) (fst n2) in
      E.insert ~override:true (E.Lexicon (E.Lexicon.compose lex1 lex2 n3)) e
    
  let wait () = interactive := true

  let dont_wait ()  = interactive := false

  let should_wait () = !interactive

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Compose
    | Dont_wait
    | Wait
    | Help of action option

  let rec action_to_string = function
    | Load -> "load"
    | List -> "list"
    | Select -> "select"
    | Unselect ->  "unselect"
    | Trace -> "trace"
    | Dont_trace -> "don't trace"
    | Print -> "print"
    | Analyse -> "analyse"
    | Compose -> "compose"
    | Dont_wait -> "don't wait"
    | Wait -> "wait"
    | Help None -> "help"
    | Help (Some (Help a)) -> action_to_string (Help a)
    | Help (Some a) -> Format.sprintf "%s help" (action_to_string a)




  let help_messages = [
    Load,Format.sprintf "\t%s d|s file;\n\t\tloads the file \"file\" as data (d option) or as a script (s option)" (action_to_string Load);
    List,Format.sprintf "\t%s;\n\t\tlists the signatures and the lexicons of the current environment" (action_to_string List);
    Select,Format.sprintf "\t%s name;\n\t\tselects the name signature or lexicon in the current environment and make it an implicit context for following commands" (action_to_string Select);
    Unselect,Format.sprintf "\t%s name;\n\t\tremoves any selected signature or lexicon from the context" (action_to_string Unselect);
    Trace,Format.sprintf "\t%s;\n\t\ttraces the interpretation (if a command is used in a context of a lexicon) and the beta-reduction process" (action_to_string Trace);
    Dont_trace,Format.sprintf "\t%s;\n\t\tstops tracing" (action_to_string Dont_trace);
    Wait,Format.sprintf "\t%s;\n\t\twaits a keyboard return event before going on in executing a script" (action_to_string Trace);
    Dont_wait,Format.sprintf "\t%s;\n\t\tstops waiting a keyboard return event before going on in executing a script" (action_to_string Trace);
    Print,Format.sprintf "\t[name] %s;\n\t\toutputs the content of the \"name\" signature or lexicon of the current environment. If no \"name\" is specified, check whether there is a selected data in the environment" (action_to_string Print);
    Analyse,Format.sprintf "\t[name1 name2 ...] %s term:type;\n\tanalyses the given \"term:type\" with respect to the given \"name1\" ... signatures or lexicons, or if no such name is given, with respect to the selected data in the environment. In the context of a signature, this command just typechecks the given entry. In the context of a lexicon, it typechecks it and interprets it with respect to this lexicon" (action_to_string Print);
    Compose,Format.sprintf "\t%s name1 name2 as name3;\n\t\tcreates a new lexicon with name \"name3\" by composing the \"name1\" and \"name2\" lexicons" (action_to_string Compose)
  ]




  let rec help = function
    | Help (Some (Help a)) -> help (Help a)
    | Help (Some a) -> Format.printf "Usage:\n%s\n" (List.assoc a help_messages)
    | Help None -> Format.printf "Commands: For any command, its usage can be reminded by running the following command:\n\tcommand help;\nThe following commands are available. \n%s\n" (Utils.string_of_list "\n" (fun (_,x) -> x) help_messages)
    | _ as a -> Format.printf "Usage:@\n%s@\n" (List.assoc a help_messages)

  let exit () = raise End_of_file
    
end
    
