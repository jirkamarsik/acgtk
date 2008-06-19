open Utils
open Printf
open Tries
open Table
open Abstract_syntax
	
module Abstract_sig =
struct
      

  exception Duplicate_type_definition
  exception Duplicate_term_definition

  type location = Lexing.position*Lexing.position

  type type_of_definition =
    | Declared
    | Defined

  type abs =
    | Linear
(*    | Non_linear *)

  type term = Abstract_syntax.term
  type stype = Abstract_syntax.type_def

  type entry =Abstract_syntax.sig_entry

  (** The type of the signature as abstract object *)
  type t = Signature of (string * location) * sig_content (** The first string is the name of the signature *)
  and sig_content = {entries:entry list;
		     type_definitions: type_of_definition StringMap.t;
		     term_definitions: (type_of_definition*Abstract_syntax.syntactic_behavior) StringMap.t;
		     warnings: Error.warning list}
  let name (Signature(n,_)) = n
    
  let get_content (Signature(_,c)) = c
    
  let is_constant id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	true,Some t
    with
      | Not_found -> false,None

  let is_type id (Signature (_,{entries=_;type_definitions=defs;term_definitions=_})) =
    try
      let _ =  StringMap.find id defs in
	true
    with
      | Not_found -> false

  let is_infix id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
      (t=Abstract_syntax.Infix)
    with
      | Not_found -> false


  let is_prefix id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	(t = Abstract_syntax.Prefix)||(t=Abstract_syntax.Default)
    with
      | Not_found -> false



  let is_binder id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	t = Abstract_syntax.Binder
    with
      | Not_found -> false

  let rec term_to_string t sg = 
    match t with
      | Abstract_syntax.Var(s,_) -> s
      | Abstract_syntax.Const (s,_) -> s
      | Abstract_syntax.LAbs (s,t,_) -> 
	  let vars,u=unfold_labs [s] t in
	    sprintf
	      "(lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (term_to_string u sg)
      | Abstract_syntax.Abs (s,t,_) -> 
	  let vars,u=unfold_abs [s] t in
	    sprintf
	      "(Lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (term_to_string u sg)
      | Abstract_syntax.App (Abstract_syntax.Const (s,_),(Abstract_syntax.LAbs(x,u,_) as t),l) when is_binder s sg ->
	  let vars,u= unfold_binder s sg [x] u in
	  sprintf
	    "(%s %s. %s)"
	    s
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u sg) 
      | Abstract_syntax.App ((Abstract_syntax.App (Abstract_syntax.Const (s,_),t1,_)),t2,_) when is_infix s sg ->
	  sprintf
	    "(%s %s %s)"
	    (term_to_string t1 sg)
	    s
	    (term_to_string t2 sg)
      | Abstract_syntax.App (t1,t2,_) ->
	  sprintf
	    "(%s %s)"
	    (term_to_string t1 sg)
	    (term_to_string t2 sg)
  and unfold_abs acc = function
    | Abstract_syntax.Abs (s,t,_) -> unfold_abs (s::acc) t
    | t -> acc,t
  and unfold_labs acc = function
    | Abstract_syntax.LAbs (s,t,_) -> unfold_labs (s::acc) t
    | t -> acc,t
  and unfold_app acc = function
    | Abstract_syntax.App (t1,t2,_) -> unfold_app (t2::acc) t1
    | t -> acc,t
  and unfold_binder binder sg acc = function
    | Abstract_syntax.App (Abstract_syntax.Const (s,_),(Abstract_syntax.LAbs(x,u,_) as t),l) when (is_binder s sg)&&(s=binder) -> unfold_binder binder sg (x::acc) u
    | t -> acc,t
	
  let rec is_atomic_type = function
    | Abstract_syntax.Type_atom _ -> true
(*    | Dep (_,t,_) -> is_atomic_type t  *)
    | _ -> false
      
  let is_arrow = function
    | Abstract_syntax.Arrow _ -> true
    | Abstract_syntax.Linear_arrow _ -> true
    | _ -> false
	
  let rec type_def_to_string def sg = match def with
    | Abstract_syntax.Type_atom (s,_,terms) ->
	(match terms with
             [] -> s
           | _  -> sprintf "%s %s" s (Utils.string_of_list " " (fun x -> sprintf "(%s)" (term_to_string x sg )) terms))
    | Abstract_syntax.Linear_arrow (t1,t2,_) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg  else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s -> %s"
	    (Utils.string_of_list
	       " -> "
	       (fun x -> if is_atomic_type x then type_def_to_string x sg  else sprintf "(%s)" (type_def_to_string x sg ))
	       (List.rev arrows))
	    u_string
    | Abstract_syntax.Arrow (t1,t2,_) ->
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg  else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if is_atomic_type x then type_def_to_string x sg  else sprintf "(%s)" (type_def_to_string x sg ))
	       (List.rev arrows))
	    u_string
  and unfold_linear_arrow acc = function
    | Abstract_syntax.Linear_arrow (t1,t2,_) -> unfold_linear_arrow (t1::acc) t2
    | t -> acc,t
  and unfold_arrow acc = function
    | Abstract_syntax.Arrow (t1,t2,_) -> unfold_arrow (t1::acc) t2
    | t -> acc,t
(*	    and unfold_dep acc = function
	    | Dep ((s,_,t),t2,_) -> unfold_dep ((s,t)::acc) t2
	    | t -> acc,t
	    and unfold_type_abs acc = function
	    | Type_Abs ((s,_,t),_) -> unfold_type_abs (s::acc) t
	    | t -> acc,t *)
	
  let type_to_string = type_def_to_string

  let empty (s,l) = Signature ((s,l),{entries=[];type_definitions=StringMap.empty;term_definitions=StringMap.empty;warnings=[]})
    
  let add_type_decl id loc k (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = (Abstract_syntax.Type_decl (id,loc,k))::content.entries;
				      type_definitions = StringMap.add id Declared content.type_definitions;
				      term_definitions = content.term_definitions})
      
  let add_type_def id def loc k (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = Abstract_syntax.Type_def (id,loc,def,k)::content.entries;
				      type_definitions = StringMap.add id Defined content.type_definitions;
				      term_definitions = content.term_definitions})
      
  let add_term_decl id k ty loc  (Signature (name,content)) =
    try 
      let _ = StringMap.find id content.term_definitions in
	raise Duplicate_term_definition
    with
      | Not_found -> 
	  Signature (name,{content with entries = (Abstract_syntax.Term_decl (id,k,loc,ty))::content.entries;
			   type_definitions = content.type_definitions;
			   term_definitions = StringMap.add id (Defined,k) content.term_definitions})

  let add_term_def id kind_of (t,type_of_t) loc (Signature (name,content)) =
    try 
      let _ = StringMap.find id content.term_definitions in
	raise Duplicate_term_definition
    with
      | Not_found -> 
	  Signature (name,{content with entries = Abstract_syntax.Term_def (id,kind_of,loc,t,type_of_t)::content.entries;
			   type_definitions = content.type_definitions;
			   term_definitions = StringMap.add id (Defined,kind_of) content.term_definitions})

  let add_entry e ((Signature (_,{term_definitions=defs})) as sg) = match e with
    | Abstract_syntax.Type_decl (id,l,k) -> add_type_decl id l k sg
    | Abstract_syntax.Type_def (id,l,def,k) -> add_type_def id def l k sg
    | Abstract_syntax.Term_decl (id,k,l,type_of) -> add_term_decl id k type_of l sg
    | Abstract_syntax.Term_def (id,k,l,t,type_of) -> add_term_def id k (t,type_of) l sg

  let add_warnings ws (Signature (name,content)) = 
      Signature(name,{content with warnings=ws@content.warnings})

  let get_warnings (Signature (_,{warnings=ws})) = ws
      
  let to_string ((Signature ((name,_),dec)) as sg) =
    sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list_rev
	 "\n"
	 (function
	    | Abstract_syntax.Type_decl (id,_,_) -> sprintf "\t%s: type;" id
	    | Abstract_syntax.Type_def (id,_,value,k) -> sprintf "\t%s = %s: type;" id (type_def_to_string value sg)
	    | Abstract_syntax.Term_decl (id,_,_,ty) -> 
		let t = match snd (StringMap.find id dec.term_definitions) with
		  | Abstract_syntax.Default -> ""
		  | Abstract_syntax.Infix -> "infix "
		  | Abstract_syntax.Prefix -> "prefix "
		  | Abstract_syntax.Binder -> "binder " in
		  sprintf "\t%s%s: %s;" t id (type_def_to_string ty sg)
	    | Abstract_syntax.Term_def (id,_,_,value,type_of) -> 
		let t = match snd (StringMap.find id dec.term_definitions) with
		  | Abstract_syntax.Default -> ""
		  | Abstract_syntax.Infix -> "infix "
		  | Abstract_syntax.Prefix -> "prefix "
		  | Abstract_syntax.Binder -> "binder " in
		  sprintf "\t%s%s = %s: %s;" t id (term_to_string value sg) (type_def_to_string type_of sg))
	 dec.entries)
      
  let new_loc (s,_) (_,e) = (s,e)

    let convert_term x y _ = (x,y)

    let convert_type y _ = y



    let fold f a (Signature (_,{entries=entries})) =
      let rec fold_aux lst k =
	match lst with
	  | [] -> k a
	  | hd::tl ->
	      fold_aux tl (fun r -> k (f hd r)) in
	fold_aux entries (fun x -> x)


    let id_to_string _ _ = Abstract_syntax.Default, "TOTO"

    let unfold_type_definition _ _ = failwith "Not implemented: useless"
    let unfold_term_definition _ _ = failwith "Not implemented: useless"

    let type_of_constant _ _ = failwith "Not implemented: useless"

    let typecheck t _ _ = t

    let get_binder_argument_functional_type _ _ = Some Abstract_syntax.Linear

    let is_declared _ _ = None
end
  
module Abstract_lex =
struct

  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  module Dico = Utils.StringMap

  type signature = Abstract_sig.t

  module Signature = Abstract_sig

  type interpretation =
    | Type of (Abstract_syntax.location * Abstract_syntax.type_def )
    | Constant of (Abstract_syntax.location * Abstract_sig.term )

  let interpretation_to_string i sg = match i with
    | Type (_,t) -> Printf.sprintf "(* type *)\t%s" (Abstract_sig.type_def_to_string t sg)
    | Constant (_,c) -> Printf.sprintf "(* cst *)\t%s" (Abstract_sig.term_to_string c sg)

  type t = {name:string*Abstract_sig.location;
	    dico:interpretation Dico.t;
	    abstract_sig:Abstract_sig.t;
	    object_sig:Abstract_sig.t;}

  let get_sig {abstract_sig=abs;object_sig=obj} = abs,obj
  let interpret _ _ _ = failwith "toto"

  let name {name=n}=n

  let empty name ~abs ~obj = {name=name;dico=Dico.empty;abstract_sig=abs;object_sig=obj}

  let insert e ({dico=d} as lex) = match e with
    | Abstract_syntax.Type (id,loc,ty) -> {lex with dico=Dico.add id (Type (loc,ty)) d}
    | Abstract_syntax.Constant (id,loc,t) -> {lex with dico=Dico.add id (Constant (loc,t)) d}

  let to_string {name=n,_;dico=d;abstract_sig=abs_sg;object_sig=obj_sg} =
    Printf.sprintf
      "lexicon %s(%s): %s =\n%send"
      n
      (fst (Abstract_sig.name abs_sg))
      (fst (Abstract_sig.name obj_sg))
      (match 
	 Dico.fold
	   (fun k i -> function
	      | None -> Some (Printf.sprintf "\t%s := %s;" k (interpretation_to_string i obj_sg))
	      | Some a -> Some (Printf.sprintf "%s\n\t%s := %s;" a k (interpretation_to_string i obj_sg)))
	   d
	   None with
	     | None -> ""
	     | Some s -> Printf.sprintf "%s\n" s)

  let check _ = Printf.printf "No checking of interpretations\n%!" 
end


