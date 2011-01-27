(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open Environment

module type Action_sig = 
sig
  
  type env

  exception Not_yet_implemented of string
  exception Stop

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save


  type file_type = | Data | Script of (string -> string list -> env -> env)

  val load : file_type -> string -> string list -> env -> env

  val list : env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : unit -> unit
  val dont_trace : unit -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> unit

  val add : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> env

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> env -> env

  val wait : unit -> unit

  val dont_wait : unit -> unit

  val should_wait : unit -> bool

  val help : action -> unit

  val exit : unit -> unit


  val create_sig :  (string * (Lexing.position * Lexing.position)) -> env -> env


  val create_lex :  abs:(string * (Lexing.position * Lexing.position)) -> obj:(string * (Lexing.position * Lexing.position)) -> (string * (Lexing.position * Lexing.position)) -> env -> env

  val save : ?names:(string * (Lexing.position * Lexing.position)) list -> string -> env -> (Lexing.position * Lexing.position) -> unit 
end


module Make (E:Environment_sig) =
struct

  type env=E.t
  type entry=E.entry

  exception Not_yet_implemented of string
  exception Stop

  let interactive = ref false

  type file_type = | Data | Script of (string -> string list -> env -> env)

  module Data_parser = Data_parser.Make(E)
    
  let load t filename dirs e =
    try
      match t with
	| Data -> Data_parser.parse_data ~override:true filename dirs e
	| Script f  -> f filename dirs e
    with
      | Stop -> e


  let list e =
    Format.printf "Available data:\n%s\n%!"
      (Utils.string_of_list
	 "\n"
	 (fun x -> x)
	 (E.fold
	    (fun d a -> 
	       match d with
		 | E.Signature sg -> (Format.sprintf "\tSignature\t%s%!" (fst (E.Signature1.name sg)))::a
		 | E.Lexicon lx -> 
		     let abs_name,obj_name =
		       let abs,obj = E.Lexicon.get_sig lx in
			 fst (E.Signature1.name abs),fst (E.Signature1.name obj) in
		       (Format.sprintf
			  "\tLexicon\t\t%s\t(%s --> %s)%!"
			  (fst (E.Lexicon.name lx))
			  abs_name
			  obj_name)
		       ::a)
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
	match name,E.focus e with
	  | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
	  | None,Some en -> en
	  | Some n,_ -> E.get n e
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
      let additional_offset = "\t" in
      let actual_offset = Printf.sprintf "%s%s" (match offset with | None -> "" | Some s -> s) additional_offset in
      let entries =
	match names,E.focus e with
	  | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
	  | None,Some en -> [en]
	  | Some ns,_ -> List.map (fun (n,l) -> 
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
		   let () = if first then Format.printf "In %s:\n%s%!" (fst (E.Signature1.name sg)) additional_offset else () in
		   (match Data_parser.parse_term ~output:true ~offset:actual_offset data sg with
		      | None -> let () = in_sg sg in false, Some sg
		      | Some _ -> false,None))
	 | E.Lexicon lex -> 
	     let abs,obj=E.Lexicon.get_sig lex in
	       match last_abs_sg with
		 |  Some previous_sg when (E.Signature1.name abs) = (E.Signature1.name previous_sg) -> (false,last_abs_sg)
		 | _ -> let () = if first then Format.printf "In %s:\n%s%!" (fst (E.Signature1.name abs)) additional_offset else () in
		     match Data_parser.parse_term ~output:first ~offset:actual_offset data abs with
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


  let entry_name = function
    | E.Signature sg -> fst (E.Signature1.name sg)
    | E.Lexicon lex -> fst (E.Lexicon.name lex)

  let add ?names e ?offset data l =
    try
      let additional_offset = "" in
      let actual_offset = Printf.sprintf "%s%s" (match offset with | None -> "" | Some s -> s) additional_offset in
      let entries,update_focus,foc_name =
	match names,E.focus e with
	  | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
	  | None,Some en -> [en],true,entry_name en
	  | Some ns,None -> 
	      (List.map (fun (n,l) -> 
			  try 
			    E.get n e
			  with
			    | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l))) ns),false,""
	  | Some ns,Some foc -> 
	      let foc_name=entry_name foc in 
		List.fold_left
		  (fun (acc,b,name) (n,l) -> 
		     try 
		       (E.get n e)::acc,b or n=foc_name,name
		     with
		       | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l)))
		  ([],false,foc_name)
		  ns in
      let new_env =
	List.fold_left
	  (fun acc entry -> match entry with
	     | E.Signature sg -> 
		 (match Data_parser.parse_sig_entry ~offset:actual_offset data sg with
		    | None -> acc
		    | Some new_sg -> E.insert ~override:true (E.Signature new_sg) acc)
	     | E.Lexicon lex -> 
		 (match Data_parser.parse_lex_entry ~offset:actual_offset data lex with
		    | None -> acc
		    | Some new_lex -> E.insert ~override:true (E.Lexicon new_lex) acc))
	  e
	  entries in
	if update_focus then E.select foc_name new_env else new_env
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

  let create_sig (n,_) e =
    E.insert ~override:true (E.Signature (E.Signature1.empty (n,(Lexing.dummy_pos,Lexing.dummy_pos)))) e


  let get_sig (n,l) e =
    try 
      E.get_signature n e
    with
      | E.Signature_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l))
	  
  let create_lex ~abs ~obj (n,_)  e =
    let abs_sg=get_sig abs e in
    let obj_sg=get_sig obj e in
      E.insert ~override:true (E.Lexicon (E.Lexicon.empty (n,(Lexing.dummy_pos,Lexing.dummy_pos)) abs_sg obj_sg)) e


  let save ?names filename e l =
    let entries =
      match names,E.focus e with
	| None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
	| None,Some en -> [en]
	| Some ns,_ -> List.map (fun (n,l) -> 
				 try 
				   E.get n e
				 with
				   | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l))) ns in
      (* Prot�ger les acc�s avec de bons messages d'erreur *)
    let outch = open_out filename in
    let () = List.iter
      (fun entry ->
	 match entry with
	   | E.Signature sg -> Printf.fprintf outch "%s\n\n%!" (E.Signature1.to_string sg)
	   | E.Lexicon lex -> Printf.fprintf outch "%s\n\n%!" (E.Lexicon.to_string lex))
      entries in
      close_out outch
      

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
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save


  let actions = [Load;List;Select;Unselect;Trace;Dont_trace;Print;Analyse;Add;Compose;Dont_wait;Wait;Help None;Create;Save]



  let rec action_to_string = function
    | Load -> "load"
    | List -> "list"
    | Select -> "select"
    | Unselect ->  "unselect"
    | Trace -> "trace"
    | Dont_trace -> "don't trace"
    | Print -> "print"
    | Analyse -> "analyse"
    | Add -> "add"
    | Compose -> "compose"
    | Dont_wait -> "don't wait"
    | Wait -> "wait"
    | Help None -> "help"
    | Help (Some (Help a)) -> action_to_string (Help a)
    | Help (Some a) -> Format.sprintf "%s help" (action_to_string a)
    | Save -> "save"
    | Create -> "create"




  let messages = function
    | Load as command -> Format.sprintf "\t%s d|data|s|script file;\n\t\tloads the file \"file\" as data (d or data option) or as a script (script or s option)" (action_to_string command)
    | List as command -> Format.sprintf "\t%s;\n\t\tlists the signatures and the lexicons of the current environment" (action_to_string command)
    | Select as command -> Format.sprintf "\t%s name;\n\t\tselects the name signature or lexicon in the current environment and make it an implicit context for following commands" (action_to_string command)
    | Unselect as command -> Format.sprintf "\t%s name;\n\t\tremoves any selected signature or lexicon from the context" (action_to_string command)
    | Trace as command -> Format.sprintf "\t%s;\n\t\ttraces the interpretation (if a command is used in a context of a lexicon) and the beta-reduction process" (action_to_string command)
    | Dont_trace as command -> Format.sprintf "\t%s;\n\t\tstops tracing" (action_to_string command)
    | Wait as command -> Format.sprintf "\t%s;\n\t\twaits a keyboard return event before going on in executing a script" (action_to_string command)
    | Dont_wait as command -> Format.sprintf "\t%s;\n\t\tstops waiting a keyboard return event before going on in executing a script" (action_to_string command)
    | Print as command -> Format.sprintf "\t[name] %s;\n\t\toutputs the content of the \"name\" signature or lexicon of the current environment. If no \"name\" is specified, check whether there is a selected data in the environment" (action_to_string command)
    | Analyse as command -> Format.sprintf "\t[name1 name2 ...] %s term:type;\n\tanalyses the given \"term:type\" with respect to the given \"name1\" ... signatures or lexicons, or if no such name is given, with respect to the selected data in the environment. In the context of a signature, this command just typechecks the given entry. In the context of a lexicon, it typechecks it and interprets it with respect to this lexicon" (action_to_string command)
    | Add as command -> Format.sprintf "\t[name1 name2 ...] %s expression;\n\tadds the given \"expression\" with respect to the given \"name1\" ... signatures or lexicons to those signature or lexicons. \"expression\" must respect the syntax of signatures or lexicons" (action_to_string command)
    | Compose as command -> Format.sprintf "\t%s name1 name2 as name3;\n\t\tcreates a new lexicon with name \"name3\" by composing the \"name1\" and \"name2\" lexicons" (action_to_string command)
    | Help _ as command -> Format.sprintf "\t%s ;\n\t\tprints the help message" (action_to_string command)
    | Create as command -> Format.sprintf "\t%s s|sig|l|lex name [name1 name2];\n\t\tcreates a new empty signature or lexicon (according to the s or sig, or l or lex option) with name \"name\" in the current environment.\"name1\" and \"name2\" are mandatory in case of creating a lexicon: they are respectively the abstract and the object signature. They of course are forbidden in case of creating a signature" (action_to_string command)
    | Save as command -> Format.sprintf "\t[name1 name2 ...] %s filename;\n\t\toutputs the content of \"name1\", \"name2\"... into the same file \"filename\". If no \"name\" is specified, check whether there is a selected data in the environment" (action_to_string command)

  let rec help = function
    | Help (Some (Help a)) -> help (Help a)
    | Help (Some a) -> Format.printf "Usage:\n%s\n" (messages a)
    | Help None -> Format.printf "Commands: For any command, its usage can be reminded by running the following command:\n\tcommand help;\nThe following commands are available. \n%s\n" (Utils.string_of_list "\n" (fun x -> x) (List.map messages actions))
    | _ as a -> Format.printf "Usage:@\n%s@\n" (messages a)

  let exit () = raise End_of_file

end
    
