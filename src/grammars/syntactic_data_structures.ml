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
(*    | Var of string * location
	(** If the term is variable (bound by a binder)*)
    | Const of string * location
	(** If the term is a constant (not bound by a binder) *)
(*    | Abs of string * term * location
	(** If the term is a intuitionistic abstraction *) *)
    | LAbs of string * term * location
	(** If the term is a linear abstraction *)
    | App of term * term * location
	(** If the term is an application *)	

  type type_def =
    | Type_atom of string * location * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def * location
	(** If the type is described with a linear abstraction *)
(*    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *) *)

*)


  (** The type of the signature as abstract object *)
  type t = Signature of (string * location) * sig_content (** The first string is the name of the signature *)
  and sig_content = {entries:Abstract_syntax.sig_entry list;
		     type_definitions: type_of_definition StringMap.t;
		     term_definitions: (type_of_definition*Abstract_syntax.syntactic_behavior) StringMap.t;
		     warnings: Error.warning list}
(*  and sig_entry = 
    | Type_decl of (string * location * kind)
	(** The first parameter ([string]) is the name of the type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its kind *)
    | Type_def of (string * location * type_def)
	(** Tthe first parameter ([string]) is the name of the defined type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
    | Term_decl of (string * term_kind * location * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its type *)
    | Term_def of (string * term_kind * location * term * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
*)  

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
	  (*    | Abs (s,t,_) -> 
		let vars,u=unfold_abs [s] t in
		sprintf
		"Lambda %s. %s"
		(Utils.string_of_list " " (fun x -> x) (List.rev vars))
		(term_to_string u) *)
      | Abstract_syntax.LAbs (s,t,_) -> 
	  let vars,u=unfold_labs [s] t in
	    sprintf
	      "(lambda %s. %s)"
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
	    (*  and unfold_abs acc = function
		| Abs (s,t,_) -> unfold_abs (s::acc) t
		| t -> acc,t *)
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
(*    | Arrow _ -> true *)
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
  and unfold_linear_arrow acc = function
    | Abstract_syntax.Linear_arrow (t1,t2,_) -> unfold_linear_arrow (t1::acc) t2
    | t -> acc,t
	(*  and unfold_arrow acc = function
	    | Arrow (t1,t2,_) -> unfold_arrow (t1::acc) t2
	    | t -> acc,t
	    and unfold_dep acc = function
	    | Dep ((s,_,t),t2,_) -> unfold_dep ((s,t)::acc) t2
	    | t -> acc,t
	    and unfold_type_abs acc = function
	    | Type_Abs ((s,_,t),_) -> unfold_type_abs (s::acc) t
	    | t -> acc,t *)
	
  let empty (s,l) = Signature ((s,l),{entries=[];type_definitions=StringMap.empty;term_definitions=StringMap.empty;warnings=[]})
    
  let add_type_decl id loc k (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = (Abstract_syntax.Type_decl (id,loc,k))::content.entries;
				      type_definitions = StringMap.add id Declared content.type_definitions;
				      term_definitions = content.term_definitions})
      
  let add_type_def id def loc (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = Abstract_syntax.Type_def (id,loc,def)::content.entries;
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
    | Abstract_syntax.Type_def (id,l,def) -> add_type_def id def l sg
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
	    | Abstract_syntax.Type_def (id,_,value) -> sprintf "\t%s = %s: type;" id (type_def_to_string value sg)
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

(*
  let rec type_def_display s sg = 
      match s with
	Type_atom(s,_,tl) -> print_string "Type_atom(";print_string s; 
	  print_string ", "; let _ = List.map term_display tl in ();
	  print_string ")"
      | Linear_arrow(td1,td2,_) -> print_string "Linear_arrow(";
	  type_def_display td1 sg; print_string ", ";
	  type_def_display td2 sg; print_string ")"

    and term_display t sg = 
      match t with
	Var(s,_) -> print_string "Var(";print_string s; print_string ")"
      | Const(s,_) -> print_string "Const(";print_string s; print_string ")"
      | LAbs(s,t1,_) -> print_string "LAbs(";print_string s;print_string ". ";
	  term_display t1 sg; print_string ")"
      | App(t1,t2,_) -> print_string "App("; term_display t1 sg;print_string " ";
	  term_display t2 sg; print_string ")"
	  


    let display ((Signature (name,dec)) as sg) =
      let rec dec_display d =
	(match d with 
	| Type_decl (id,_,K types) -> printf "Type_decl("; print_string  id;
	    print_string ",K of ";
          (match types 
          with [] -> print_string "[])"
          | _  -> 
	      let rec type_defl t =
		match t with
		  [] -> ()
		| s::ty -> type_def_display s sg; type_defl ty in
		type_defl types);
	| Type_def (id,_,value) -> printf "Type_def(";print_string id; printf ", ";
	    (type_def_display value sg);print_string ")"
	| Term_decl (id,_,_,ty) -> printf "Term_decl(";print_string id; printf ", ";
	    let t = match snd (StringMap.find id dec.term_definitions) with
	      | Default -> "default "
	      | Infix -> "infix "
	      | Prefix -> "prefix "
	      | Binder -> "binder " in
	      printf "\t%s, " t ;type_def_display ty sg;print_string ")"
	| Term_def (id,_,_,value,type_of) -> printf "Term_def( ";print_string id; printf ", ";
	    (let t = match snd (StringMap.find id dec.term_definitions) with
	       | Default -> "default "
	       | Infix -> "infix "
	       | Prefix -> "prefix "
	       | Binder -> "binder " in
	       printf "\t%s, " t ); term_display value sg;print_string ", "; type_def_display type_of sg ; print_string ")")
	  
      and decl_display dec = 
	match dec with
	    [] -> ()
	  | d::de -> dec_display d; print_newline(); decl_display de
      in
	decl_display dec.entries
*)
    let convert x = x	  



    let fold f a (Signature (_,{entries=entries})) =
      let rec fold_aux lst k =
	match lst with
	  | [] -> k a
	  | hd::tl ->
	      fold_aux tl (fun r -> k (f hd r)) in
	fold_aux entries (fun x -> x)

end
  
module Abstract_lex =
struct

  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  module Dico = Utils.StringMap

  type signature = Abstract_sig.t

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
end


(*
module Environment =
struct

  exception Signature_not_found of string
    
  module Env = Map.Make(String)
  type content = 
    | Signature of Abstract_sig.t
    | Lexicon of Abstract_lex.t

  type t = {map:content Env.t;sig_number:int;lex_number:int}

  let empty = {map=Env.empty;sig_number=0;lex_number=0}

  let insert d e = match d with
    | Signature s -> let name,(p1,p2) = Abstract_sig.name s in
	if not (Env.mem name e.map)
	then
	  {e with map=Env.add name d e.map ;sig_number=e.sig_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_signature name,(p1,p2))))
    | Lexicon l -> let name,(p1,p2) = Abstract_lex.name l in
	if not (Env.mem name e.map)
	then
	  {e with map=Env.add name d e.map ;lex_number=e.lex_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_lexicon name,(p1,p2))))

  let iter f {map=e} =  Env.iter (fun _ d -> f d) e

  let fold f a {map=e} = Env.fold (fun _ d acc -> f d acc) e a

  let sig_number {sig_number=n} = n

  let get_signature s {map=e} =
    try
      match Env.find s e with
	| Signature sg -> sg
	| Lexicon _ -> raise (Signature_not_found s)
    with
      | Not_found -> raise (Signature_not_found s)


  exception Sig of Abstract_sig.t    

  let choose_signature {map=e} =
    try
      let () = Env.fold
	(fun _ c a -> 
	   match c with
	     | Signature s -> raise (Sig s)
	     | Lexicon _ -> a )
	e
	() in
	None
    with
      | Sig s -> Some s
    
end
*)
