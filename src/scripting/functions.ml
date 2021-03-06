(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
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

let info fd name =
  let w,h =
    try
      let w,h=ANSITerminal.size () in
      Printf.sprintf "%i" w,Printf.sprintf "%i" h
    with
    | Failure "ANSITerminal.size" -> "Not set","Not set" in
  Printf.printf
    "The file descriptor %s refers to a terminal: %b and the size is (%s,%s)\n%!"
    name
    (Unix.isatty fd)
    w
    h



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
    | Check
    | Realize
    | RealizeShow
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save
    | Parse
    | Idb
    | Query


  type file_type =
  | Data
  | Object
  | Script of (string -> string list -> env -> env)

  val color_output : bool -> unit

  val load : file_type -> string -> string list -> env -> env

  val list : env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : unit -> unit
  val dont_trace : unit -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit
  val check : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit
  val realize : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit
  val realize_show : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val parse : ?name:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val idb : ?name:string -> env ->  (Lexing.position * Lexing.position) -> unit

  val query : ?name:string -> env -> string -> (Lexing.position * Lexing.position) -> unit

  val add : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> string -> (Lexing.position * Lexing.position) -> env

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

  let color = ref true

  let color_output b = color:=b

  type file_type =
  | Data
  | Object
  | Script of (string -> string list -> env -> env)

  module Data_parser = Data_parser.Make(E)
  module ShowI = Show.Make(E)
                          (Show_text_dejavu)
                          (Show_colors_solarized_dark)
                          (Show_embellish_examples.Make(Show_text_dejavu))

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Check
    | Realize
    | RealizeShow
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save
    | Parse
    | Idb
    | Query


  let actions = [Help None;Load;List;Print;Check;Realize;RealizeShow;Parse;Idb;Query;Compose;Analyse;Wait;Dont_wait;Select;Unselect;Create;Add;Save;Trace;Dont_trace;]


  let rec action_to_string = function
    | Load -> "load"
    | List -> "list"
    | Select -> "select"
    | Unselect ->  "unselect"
    | Trace -> "trace"
    | Dont_trace -> "don't trace"
    | Print -> "print"
    | Analyse -> "analyse"
    | Check -> "check"
    | Realize -> "realize"
    | RealizeShow -> "realize_show"
    | Add -> "add"
    | Compose -> "compose"
    | Dont_wait -> "don't wait"
    | Wait -> "wait"
    | Help None -> "help"
    | Help (Some (Help a)) -> action_to_string (Help a)
    | Help (Some a) -> Printf.sprintf "%s help" (action_to_string a)
    | Save -> "save"
    | Create -> "create"
    | Parse -> "parse"
    | Idb -> "idb"
    | Query -> "query"


  let colored_string f s =
    if !color then
      f s
    else
      s
	
  let red s = colored_string Utils.red s
  let green s = colored_string Utils.green s
  let bold s = 
    if !color then
      ANSITerminal.sprintf [ANSITerminal.Bold] "%s" s
    else
      s

  let action_to_string s =
    red (action_to_string s)


  let messages = function
    | Load as command ->
      let options = bold "d|data|s|script|o|object" in
      Utils.format "@[<v5>%s %s file;@ @[loads@ the@ file@ \"file\"@ as@ data@ (\"d\"@ or@ \"data\"@ option),@ as@ an@ object@ (compiled@ data,@ \"o\"@ or@ \"object\"@ option),@ or@ as@ a@ script@ (\"s\"@ or@ \"script\"@ option)@ @]@]@." (action_to_string command) options
    | List as command -> Utils.format "@[<v5>%s;@ @[lists@ the@ signatures@ and@ the@ lexicons@ of@ the@ current@ environment@ @]@]@." (action_to_string command)
    | Select as command -> Utils.format "@[<v5>%s name;@ @[selects@ the@ \"name\"@ signature@ or@ lexicon@ in@ the@ current@ environment@ and@ make@ it@ an@ implicit@ context@ for@ the@ following@ commands@ @]@]@." (action_to_string command)
    | Unselect as command -> Utils.format "@[<v5>%s name;@ @[removes@ any@ selected@ signature@ or@ lexicon@ from@ the@ context@ @]@]@." (action_to_string command)
    | Trace as command -> Utils.format "@[<v5>%s;@ @[traces@ the@ interpretation@ (if@ a@ command@ is@ used@ in@ a@ context@ of@ a@ lexicon)@ and@ the@ beta-reduction@ process@ @]@]@." (action_to_string command)
    | Dont_trace as command -> Utils.format "@[<v5>%s;@ @[stops@ tracing@ @]@]@." (action_to_string command)
    | Wait as command -> Utils.format "@[<v5>%s;@ @[waits@ a@ keyboard@ return@ event@ before@ going@ on@ in@ executing@ a@ script@ @]@]@." (action_to_string command)
    | Dont_wait as command -> Utils.format "@[<v5>%s;@ @[stops@ waiting@ a@ keyboard@ return@ event@ before@ going@ on@ in@ executing@ a@ script@ @]@]@." (action_to_string command)
    | Print as command -> Utils.format "@[<v5>[name] %s;@ @[outputs@ the@ content@ of@ the@ \"name\"@ signature@ or@ lexicon@ of@ the@ current@ environment.@ If@ no@ \"name\"@ is@ specified,@ check@ whether@ there@ is@ a@ selected@ data@ in@ the@ environment@ @]@]@." (action_to_string command)
    | Analyse as command -> Utils.format "@[<v5>[name1 name2 ...] %s term:type;@ @[*DEPRECATED*@]@ @[analyses@ the@ given@ \"term:type\"@ with@ respect@ to@ the@ given@ \"name1\"@ ...@ signatures@ or@ lexicons,@ or@ if@ no@ such@ name@ is@ given,@ with@ respect@ to@ the@ selected@ data@ in@ the@ environment.@ In@ the@ context@ of@ a@ signature,@ this@ command@ just@ typechecks@ the@ given@ entry.@ In@ the@ context@ of@ a@ lexicon,@ it@ typechecks@ it@ and@ interprets@ it@ with@ respect@ to@ this@ lexicon@ @]@]@." (action_to_string command)
    | Check as command -> Utils.format "@[<v5>[name1 name2 ...] %s term:type;@ @[check@ whether@ the@ given@ \"term:type\"@ typechecks@ with@ respect@ to@ the@ given@ \"name1\"@ ...@ signatures,@ or@ if@ no@ such@ name@ is@ given,@ with@ respect@ to@ the@ selected@ data@ in@ the@ environment,@ provided@ it@ is@ a@ signature.@]@]@." (action_to_string command)
    | Realize as command -> Utils.format "@[<v5>[name1 name2 ...] %s term:type;@ @[check@ whether@ the@ given@ \"term:type\"@ typechecks@ with@ respect@ to@ the@ abstract@ signatures@ of@ the@ \"name1\"@ ...@ lexicons,@ or@ if@ no@ such@ name@ is@ given,@ with@ respect@ to@ the@ selected@ data@ in@ the@ environment,@ provided@ it@ is@ a@ lexicon.@ Then@ the@ interrpretetion@ of@ the@ input@ term@ by@ each@ lexicon@ is@ computed.@]@]@." (action_to_string command)
    | RealizeShow as command -> Utils.format "@[<v5>[name1 name2 ...] %s term:type;@ @[same@ as@ realize,@ but@ with@ graphical@ output.@ A@ graph@ illustrating@ the@ images@ of@ the@ supplied@ term@ and@ its@ subterms@ with@ the@ given@ lexicons@ will@ be@ produced@ in@ a@ file@ named@ realize.svg.@]@]@." (action_to_string command)
    | Parse as command -> Utils.format "@[<v5>[name] %s term:type;@ @[parse@ the@ object@ term@ \"term\"@ as@ the@ image@ of@ some@ abstract@ term@ of@ type@ \"type\"@ according@ to@ the@ lexicon@ \"name\".@ If@ no@ \"name\"@ is@ specified,@ check@ whether@ there@ is@ a@ selected@ data@ in@ the@ environment@ @]@]@." (action_to_string command)
    | Idb as command -> Utils.format "@[<v5>[name] %s;@ @[outputs@ the@ datalog@ program@ (intensional@ database)@ corresponding@ to@ the@ lexicon@ \"name\".@ If@ no@ \"name\"@ is@ specified,@ check@ whether@ there@ is@ a@ selected@ data@ in@ the@ environment@ @]@]@." (action_to_string command)
    | Query as command -> Utils.format "@[<v5>[name] %s term:type;@ @[outputs@ the@ facts@ (extensional@ database)@ and@ the@ query@ associated@ to@ the@ term@ \"term\"@ of@ distinguished@ type@ \"type\"@ with@ respect@ to@ the@ lexicon@ \"name\".@ If@ no@ \"name\"@ is@ specified,@ check@ whether@ there@ is@ a@ selected@ data@ in@ the@ environment@ @]@]@." (action_to_string command)
    | Add as command -> Utils.format "@[<v5>[name1 name2 ...] %s expression;@ @[adds@ the@ given@ \"expression\"@ with@ respect@ to@ the@ given@ \"name1\"@ ...@ signatures@ or@ lexicons@ to@ those@ signature@ or@ lexicons.@ \"expression\"@ must@ respect@ the@ syntax@ of@ signatures@ or@ lexicons@ @]@]@." (action_to_string command)
    | Compose as command -> let as_str = red "as" in
			    Utils.format "@[<v5>%s name1 name2 %s name3; @ @[ creates@ a@ new@ lexicon@ with@ name@ \"name3\"@ by@ composing@ the@ \"name1\"@ and@ \"name2\"@ lexicons@ @]@]@." (action_to_string command) as_str
    | Help _ as command -> Utils.format "@[<v5>%s ;@ @[prints@ the@ help@ message@ @]@]@." (action_to_string command)
    | Create as command -> Utils.format "@[<v5>%s s|sig|l|lex name [name1 name2];@ @[creates@ a@ new@ empty@ signature@ or@ lexicon@ (according@ to@ the@ s@ or@ sig,@ or@ l@ or@ lex@ option)@ with@ name@ \"name\"@ in@ the@ current@ environment.\"name1\"@ and@ \"name2\"@ are@ mandatory@ in@ case@ of@ creating@ a@ lexicon:@ they@ are@ respectively@ the@ abstract@ and@ the@ object@ signature.@ They@ of@ course@ are@ forbidden@ in@ case@ of@ creating@ a@ signature@ @]@]@." (action_to_string command)
    | Save as command -> Utils.format "@[<v5>[name1 name2 ...] %s filename;@ @[outputs@ the@ content@ of@ \"name1\",@ \"name2\"...@ into@ the@ same@ file@ \"filename\".@ If@ no@ \"name\"@ is@ specified,@ check@ whether@ there@ is@ a@ selected@ data@ in@ the@ environment@ @]@]@." (action_to_string command)



  let rec help = function
    | Help (Some (Help a)) -> help (Help a)
    | Help (Some a) -> 
      let () = Utils.format "Usage:" in
      let () = print_newline () in
      let () = messages a in
      print_newline ()
    | Help None -> 
      let () = Utils.format "@[<v5>Commands:@[ For@ any@ command,@ its@ usage@ can@ be@ reminded@ by@ running@ the@ following@ command:@]@ @[command help;@]@ @[The@ following@ commands@ are@ available.@]@]@]@." in
      let () = print_newline () in
      List.iter
	(fun a -> 
	  let () = messages a in
	  print_newline ())
	actions
    | _ as a -> 
      let () = Utils.format "Usage:" in
      let () = print_newline () in
      let () = messages a in
      print_newline ()


    
  let load t filename dirs e =
    try
      match t with
	| Data -> 
	  (match Data_parser.parse_data ~override:true filename dirs e with
	  | None -> e
	  | Some e' -> e')
	| Object -> 
	  (try
	    let file =(Utils.find_file filename dirs) in
	    let in_ch = open_in file in
	    let () = Printf.printf "Loading \"%s\"...\n" file in
	    let new_env = input_value in_ch in
	    let () = Printf.printf "Done.\n%!" in
	    let () = close_in in_ch in	    
	    E.append e new_env
	   with
	   | Utils.No_file(f,msg) -> 
	     let err = Error.System_error (Printf.sprintf "No such file \"%s\" in %s" f msg) in
	     let () = Printf.fprintf stderr "Error: %s\n%!" (Error.error_msg err filename) in
	     e)
	| Script f  -> f filename dirs e
    with
    | Stop -> e
      
      
  let list e =
    let _ = Format.flush_str_formatter () in
    let () = Utils.sformat "@[<v3>Available data:@," in
    let () =
      E.iter
	(function 
	| E.Signature sg -> Utils.sformat "@[%9s @[%s@]@]@," "Signature" (green (fst (E.Signature1.name sg)))
	| E.Lexicon lx -> 
	  let abs_name,obj_name =
	    let abs,obj = E.Lexicon.get_sig lx in
	    fst (E.Signature1.name abs),fst (E.Signature1.name obj) in
	  Utils.sformat 
	    "@[<b>@[%9s @[<3>@[%s@]@ @[(@[%s -->@ %s@])@]@]@]@]@," "Lexicon"
	    (red (fst (E.Lexicon.name lx)))
	    (green abs_name)
	    (green obj_name))
	e in
    let () = Utils.sformat "@." in
    let s = Format.flush_str_formatter () in
    Utils.format "%s@?" s
   
  let select n l e =
    try
	E.select n e
    with
      | E.Signature_not_found n
      | E.Lexicon_not_found n
      | E.Entry_not_found n -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment n,l))

  let unselect = E.unselect


  let get_entry name e l =
    match name,E.focus e with
    | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
    | None,Some en -> en
    | Some n,_ ->
      (try 
	 E.get n e
       with
       | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l)))
	
  let get_lex name cmd e l =
    match get_entry name e l with
    | E.Signature sg -> raise (Scripting_errors.Error (Scripting_errors.Accept_only ((Scripting_errors.Lex (fst (E.Signature1.name sg))),cmd),l))
    | E.Lexicon lex ->  lex

  let get_sig name cmd e l =
    match get_entry name e l with
    | E.Lexicon lex -> raise (Scripting_errors.Error (Scripting_errors.Accept_only ((Scripting_errors.Sg (fst (E.Lexicon.name lex))),cmd),l))
    | E.Signature sg ->  sg
      
  let get_lexicons names cmd e l =
    match names,E.focus e with
    | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
    | None,Some (E.Lexicon lex) -> [lex]
    | None,Some (E.Signature sg) -> 
      raise (Scripting_errors.Error (
	Scripting_errors.Accept_only (
          Scripting_errors.Lex (
            fst (E.Signature1.name sg)),
          cmd),
        l)) 
    | Some ns,_ -> List.map (fun (n,l) -> get_lex (Some n) cmd e l) ns





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



  let in_sg sg = Printf.fprintf stderr "in signature %s\n%!" (green (fst (E.Signature1.name sg)))

    
  let analyse ?names e data l =
    try
      let () = Printf.printf "\n%!" in
      let _ = Format.flush_str_formatter () in
      let entries =
	match names,E.focus e with
	| None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
	| None,Some en -> [en]
	| Some ns,_ -> 
	  List.map
	    (fun (n,l) -> 
	      try 
		E.get n e
	      with
	      | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l)))
	    ns in
      let _ = List.fold_left
	(fun (first,last_abs_sg) entry -> 
	  match entry with
	  | E.Signature sg -> 
	    (match last_abs_sg with
	    | Some previous_sg when (E.Signature1.name sg) = (E.Signature1.name previous_sg) -> (false,last_abs_sg)
	    | _ ->
	      let () = 
		if first then
		  Utils.sformat "@[<v3>@[In@ %s:@]@,@[" (green (fst (E.Signature1.name sg)))
		else
		  Utils.sformat "@[@["
	      in
	      (match Data_parser.parse_term ~output:true  data sg with
	      | None -> let () = in_sg sg in false, Some sg
	      | Some _ -> 
		let () = Utils.sformat "@]@]@." in
		let s = Format.flush_str_formatter () in
		let () = Utils.format "%s@?" s in
		false,None))
	  | E.Lexicon lex -> 
	    let abs,obj=E.Lexicon.get_sig lex in
	    match last_abs_sg with
	    |  Some previous_sg when (E.Signature1.name abs) = (E.Signature1.name previous_sg) -> (false,last_abs_sg)
	    | _ ->
	      let () =
		if first then
		  Utils.sformat "@[<v3>@[In@ %s:@]@,@[" (green (fst (E.Signature1.name abs)))
		else
		  Utils.sformat "@[@["
	      in
	      match Data_parser.parse_term ~output:first  data abs with
	      | None -> false,Some abs
	      | Some (t,ty) -> 
		let () = Utils.sformat "@]@]@.@." in
		let s = Format.flush_str_formatter () in
		let () = Utils.format "%s" s in
		let t',ty' = E.Lexicon.interpret t ty lex in
		let () = Utils.sformat "@[<v3>@[Interpreted@ by@ %s@ in@ %s@ as:@]@,@[" 
		  (red (fst (E.Lexicon.name lex)))
		  (green (fst (E.Signature1.name obj))) in
		let () = Utils.sformat "@[" in
		let () = E.Signature1.term_to_formatted_string Format.str_formatter t' obj in
		let () = Utils.sformat "@] :@ @[" in
		let () = E.Signature1.type_to_formatted_string Format.str_formatter  ty' obj in
		let () = Utils.sformat "@]@]@]@." in
		false,None)
	(true,None)
	entries in
      let () = Utils.sformat "@." in
      let s = Format.flush_str_formatter () in
      Utils.format "%s@?" s
    with
    | E.Signature_not_found n
    | E.Lexicon_not_found n
    | E.Entry_not_found n -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment n,l))
      
	
      
  let check ?names e data l =
    let () = Printf.printf "\n%!" in
    let _ = Format.flush_str_formatter () in
    let signatures =
      match names,E.focus e with
      | None,None -> raise (Scripting_errors.Error (Scripting_errors.No_focus,l))
      | None,Some (E.Signature sg) -> [sg]
      | None,Some (E.Lexicon lex) -> 
	raise (Scripting_errors.Error (
	  Scripting_errors.Accept_only (
	    Scripting_errors.Sg (
	      fst (E.Lexicon.name lex)),
	    "check"),
	  l))
      | Some ns,_ -> List.map (fun (n,l) -> get_sig (Some n) "check" e l) ns in
    List.iter
      (fun sg -> 
	let () = Utils.sformat "@[<v3>@[In@ %s:@]@,@[" (green (fst (E.Signature1.name sg))) in
	let _ = Data_parser.parse_term ~output:true data sg in
	let () = Utils.sformat "@]@]@." in
	let s = Format.flush_str_formatter () in
	Utils.format "%s" s)
      signatures
      

  let toplevel_newline () =
    let () = Printf.printf "\n%!" in
    let _ = Format.flush_str_formatter () in
    ()

  let toplevel_flush_newline () =
    let () = Utils.sformat "@." in
    let s = Format.flush_str_formatter () in
    Utils.format "%s@?" s
 
  let realize ?names e data l =
    toplevel_newline ();
    let lexicons = get_lexicons names "realize" e l in
    let _ = List.fold_left
      (fun (first,last_abs_sg) lex -> 
	let abs,obj=E.Lexicon.get_sig lex in
	let () =
	  match last_abs_sg with
	  | None  -> 
	    Utils.sformat "@[<v3>@[In@ %s:@]@,@[" (green (fst (E.Signature1.name abs)))
	  | Some previous_sg when (E.Signature1.name abs) <> (E.Signature1.name previous_sg)  ->
	    Utils.sformat "@[<v3>@[In@ %s:@]@,@[" (green (fst (E.Signature1.name abs)))
	  | _ -> Utils.sformat "@[@[" in
	match Data_parser.parse_term ~output:first data abs with
	| None -> false,Some abs
	| Some (t,ty) -> 
	  let () = Utils.sformat "@]@]@." in
	  let s = Format.flush_str_formatter () in
	  let () = Utils.format "%s" s in
	  let t',ty' = E.Lexicon.interpret t ty lex in
	  let () = Utils.sformat "@[<v3>@[Interpreted@ by@ %s@ in@ %s@ as:@]@,@[" 
	    (red (fst (E.Lexicon.name lex)))
	    (green (fst (E.Signature1.name obj))) in
	let () = Utils.sformat "@[" in
	  let () = E.Signature1.term_to_formatted_string Format.str_formatter t' obj in
	  let () = Utils.sformat "@] :@ @[" in
	  let () = E.Signature1.type_to_formatted_string Format.str_formatter  ty' obj in
	  let () = Utils.sformat "@]@]@]@." in
	  false,Some abs)
      (true,None)
      lexicons in
    toplevel_flush_newline ()

  let realize_show ?names e data l =
    toplevel_newline ();
    let lexicons = get_lexicons names "realize_show" e l in
    let abs, _ = E.Lexicon.get_sig (List.hd lexicons) in
    match Data_parser.parse_term data abs with
    | None -> ()
    | Some (abs_term, abs_type) ->
      let d = ShowI.realize_diagram abs_term lexicons in
      Diagram.to_svg "realize.svg" d
 
  type inputs =
  | Stop
  | Next
  | All

  let return_input s =
    let () = print_newline () in
    match String.lowercase (String.trim s) with
    | "y" | "yes"-> Some Next
    | "n" | "no" -> Some Stop
    | "a" | "all" -> Some All
    | "" -> Some Next
    | _ -> None


  let rec interact message get_input =
    let () = Printf.printf "%s%!" message in
    match get_input (read_line ()) with
    | Some v -> v
    | None -> interact message get_input
      
  let rec ask_for_next_parse f param =
    let rec no_interaction f p =
      match f p with
      | None -> Printf.printf "No other possible value\n"
      | Some new_param -> no_interaction f new_param in
    let msg = Printf.sprintf "Do you want to look for another solution?\n\ty/yes\n\tn/no\n\ta/all\n(Default: yes):" in
    match interact msg return_input with
    | Next -> 
      (match f param with
      | None -> Printf.printf "No other possible value\n"
      | Some new_param -> ask_for_next_parse f new_param)
    | All -> no_interaction f param
    | Stop -> ()
      

  let get_parse_tree resume abs_ty lex =
    let _ = Format.flush_str_formatter () in
    let abs_sig,_=E.Lexicon.get_sig lex in
    match E.Lexicon.get_analysis resume lex with
    | Some t,resume -> 
      let () = Utils.sformat "@[<v>@[An@ antecedent@ by@ %s@ in@ %s@ is:@]@,@[" (red (fst (E.Lexicon.name lex))) (green (fst (E.Signature1.name abs_sig))) in
      let () = Utils.sformat "@[@[" in
      let () = E.Signature1.term_to_formatted_string  Format.str_formatter t abs_sig in
      let () = Utils.sformat "@] :@ @[" in
      let () = E.Signature1.type_to_formatted_string  Format.str_formatter abs_ty abs_sig in
      let () = Utils.sformat "@]@]@]@]@." in
      let s = Format.flush_str_formatter () in
      let () = Utils.format "%s@?" s in
      Some resume
    | None,_ -> None
      
  let parse ?name e data l =
    let lex = get_lex name "parse" e l in
    let abs,obj=E.Lexicon.get_sig lex in
    match Data_parser.parse_heterogenous_term ~output:false data lex with
    | None -> ()
    | Some (obj_t,abs_ty) -> 
      let resume = get_parse_tree (E.Lexicon.parse obj_t abs_ty lex) abs_ty lex in
      match resume with
      | None -> Printf.printf "No solution.\n%!"
      | Some resume ->
	ask_for_next_parse (fun res -> get_parse_tree res abs_ty lex) resume
	  
	  
  let idb ?name e l =
    let lex = get_lex name "query" e l in
    let buff=E.Lexicon.program_to_buffer lex in
    Printf.printf
      "The datalog program (intensional database) corresponding to the lexicon \"%s\" is:\n%s\n%!"
      (fst (E.Lexicon.name lex))
      (Buffer.contents buff)      
	
      
  let query ?name e data l =
    let lex = get_lex name "idb" e l in
    let abs,obj=E.Lexicon.get_sig lex in
    match Data_parser.parse_heterogenous_term ~output:false data lex with
    | None -> ()
    | Some (obj_t,abs_ty) -> 
      let buff=E.Lexicon.query_to_buffer obj_t abs_ty lex in
      Printf.printf
	"The datalog program (intensional database) corresponding to the lexicon \"%s\" is:\n%s\n%!"
	(fst (E.Lexicon.name lex))
	(Buffer.contents buff)      
	  

      
      
  let entry_name = function
    | E.Signature sg -> fst (E.Signature1.name sg)
    | E.Lexicon lex -> fst (E.Lexicon.name lex)

  let add ?names e data l =
    try
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
		       (E.get n e)::acc,b || n=foc_name,name
		     with
		       | E.Entry_not_found s -> raise (Scripting_errors.Error (Scripting_errors.Not_in_environment s,l)))
		  ([],false,foc_name)
		  ns in
      let new_env =
	List.fold_left
	  (fun acc entry -> match entry with
	     | E.Signature sg -> 
		 (match Data_parser.parse_sig_entry data sg with
		    | None -> acc
		    | Some new_sg -> E.insert ~override:true (E.Signature new_sg) acc)
	     | E.Lexicon lex -> 
		 (match Data_parser.parse_lex_entry data lex with
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


  let exit () = raise End_of_file

end
    
