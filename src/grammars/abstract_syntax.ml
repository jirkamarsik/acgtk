open Utils
open Printf
open Tries
open Table
	
module Abstract_sig =
struct
      

  exception Duplicate_type_definition
  exception Duplicate_term_definition

  type location = Lexing.position*Lexing.position

  type term_kind =
    | Default
    | Prefix
    | Infix
    | Binder

  type type_of_definition =
    | Declared
    | Defined

  type abs =
    | Linear
(*    | Non_linear *)

  type term =
    | Var of string * location
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



  (** The type of kinds as found in the signature files *)
  type kind = K of type_def list

  (** The type of the signature as abstract object *)
  type t = Signature of string * sig_content (** The first string is the name of the signature *)
  and sig_content = {entries:sig_entry list;
		     type_definitions: type_of_definition StringMap.t;
		     term_definitions: (type_of_definition*term_kind) StringMap.t;
		     warnings: Error.warning list}
  and sig_entry = 
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
    

  let name (Signature(n,_)) = n
    
    
  let is_atomic_term = function
    | Var _ -> true
    | Const _ -> true
    | _ -> false
      

  let get_term_location = function
    | Var (_,l) -> l
    | Const (_,l) -> l
(*    | Abs (_,_,l) -> l *)
    | LAbs (_,_,l) -> l
    | App (_,_,l) -> l

  let get_type_location = function
    | Type_atom (_,l,_) -> l
    | Linear_arrow (_,_,l) -> l
(*    | Arrow (_,_,l) -> l
    | Dep (_,_,l) -> l
    | Type_Abs (_,l) -> l *)

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
      (t=Infix)
    with
      | Not_found -> false


  let is_prefix id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	(t = Prefix)||(t=Default)
    with
      | Not_found -> false



  let is_binder id (Signature (_,{entries=_;type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	t = Binder
    with
      | Not_found -> false

  let rec term_to_string t sg = 
    match t with
      | Var(s,_) -> s
      | Const (s,_) -> s
	  (*    | Abs (s,t,_) -> 
		let vars,u=unfold_abs [s] t in
		sprintf
		"Lambda %s. %s"
		(Utils.string_of_list " " (fun x -> x) (List.rev vars))
		(term_to_string u) *)
      | LAbs (s,t,_) -> 
	  let vars,u=unfold_labs [s] t in
	    sprintf
	      "(lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (term_to_string u sg)
      | App (Const (s,_),(LAbs(x,u,_) as t),l) when is_binder s sg ->
	  let vars,u= unfold_binder s sg [x] u in
	  sprintf
	    "(%s %s. %s)"
	    s
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u sg) 
      | App ((App (Const (s,_),t1,_)),t2,_) when is_infix s sg ->
	  sprintf
	    "(%s %s %s)"
	    (term_to_string t1 sg)
	    s
	    (term_to_string t2 sg)
      | App (t1,t2,_) ->
	  sprintf
	    "(%s %s)"
	    (term_to_string t1 sg)
	    (term_to_string t2 sg)
	    (*  and unfold_abs acc = function
		| Abs (s,t,_) -> unfold_abs (s::acc) t
		| t -> acc,t *)
  and unfold_labs acc = function
    | LAbs (s,t,_) -> unfold_labs (s::acc) t
    | t -> acc,t
  and unfold_app acc = function
    | App (t1,t2,_) -> unfold_app (t2::acc) t1
    | t -> acc,t
  and unfold_binder binder sg acc = function
    | App (Const (s,_),(LAbs(x,u,_) as t),l) when (is_binder s sg)&&(s=binder) -> unfold_binder binder sg (x::acc) u
    | t -> acc,t
	
  let rec is_atomic_type = function
    | Type_atom _ -> true
(*    | Dep (_,t,_) -> is_atomic_type t  *)
    | _ -> false
      
  let is_arrow = function
(*    | Arrow _ -> true *)
    | Linear_arrow _ -> true
    | _ -> false
	
  let rec type_def_to_string def sg = match def with
    | Type_atom (s,_,terms) ->
	(match terms with
             [] -> s
           | _  -> sprintf "%s %s" s (Utils.string_of_list " " (fun x -> sprintf "(%s)" (term_to_string x sg )) terms))
    | Linear_arrow (t1,t2,_) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg  else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s -> %s"
	    (Utils.string_of_list
	       " -> "
	       (fun x -> if is_atomic_type x then type_def_to_string x sg  else sprintf "(%s)" (type_def_to_string x sg ))
	       (List.rev arrows))
	    u_string
(*    | Arrow (t1,t2,_) -> 
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if (is_atomic_type x) then type_def_to_string x else sprintf "(%s)" (type_def_to_string x))
	       (List.rev arrows))
	    u_string
    | Dep ((s,_,ty1),ty2,_) ->
	let deps,u = unfold_dep [(s,ty1)] ty2 in
	sprintf 
	  "(%s) %s"
	  (Utils.string_of_list ", " (fun (id,t) -> sprintf "%s:%s" id (type_def_to_string t)) (List.rev deps))
          (if is_atomic_type u
	   then
	     type_def_to_string u
           else
	     sprintf "(%s)" (type_def_to_string u))
    | Type_Abs ((s,_,ty),_) -> 
	let abs,u = unfold_type_abs [s] ty in
	  sprintf
	    "lambda %s.(%s)"
	    (Utils.string_of_list " " (fun x -> x) (List.rev abs))
	    (type_def_to_string ty) *)
  and unfold_linear_arrow acc = function
    | Linear_arrow (t1,t2,_) -> unfold_linear_arrow (t1::acc) t2
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
	
  let empty s = Signature (s,{entries=[];type_definitions=StringMap.empty;term_definitions=StringMap.empty;warnings=[]})
    
  let add_type_decl id types loc (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = (Type_decl (id,loc,K types))::content.entries;
				      type_definitions = StringMap.add id Declared content.type_definitions;
				      term_definitions = content.term_definitions})
      
  let add_type_def id def loc (Signature (name,content)) =
    try
      let _ = StringMap.find id content.type_definitions in
	raise Duplicate_type_definition
    with
      | Not_found -> Signature (name,{content with entries = Type_def (id,loc,def)::content.entries;
				      type_definitions = StringMap.add id Defined content.type_definitions;
				      term_definitions = content.term_definitions})
      
  let add_term_decl id k ty loc  (Signature (name,content)) =
    try 
      let _ = StringMap.find id content.term_definitions in
	raise Duplicate_term_definition
    with
      | Not_found -> 
	  Signature (name,{content with entries = (Term_decl (id,k,loc,ty))::content.entries;
			   type_definitions = content.type_definitions;
			   term_definitions = StringMap.add id (Defined,k) content.term_definitions})

  let add_term_def id kind_of (t,type_of_t) loc (Signature (name,content)) =
    try 
      let _ = StringMap.find id content.term_definitions in
	raise Duplicate_term_definition
    with
      | Not_found -> 
	  Signature (name,{content with entries = Term_def (id,kind_of,loc,t,type_of_t)::content.entries;
			   type_definitions = content.type_definitions;
			   term_definitions = StringMap.add id (Defined,kind_of) content.term_definitions})

  let add_entry e ((Signature (_,{term_definitions=defs})) as sg) = match e with
    | Type_decl (id,l,K types) -> add_type_decl id types l sg
    | Type_def (id,l,def) -> add_type_def id def l sg
    | Term_decl (id,k,l,type_of) -> add_term_decl id k type_of l sg
    | Term_def (id,k,l,t,type_of) -> add_term_def id k (t,type_of) l sg

  let add_warnings ws (Signature (name,content)) = 
      Signature(name,{content with warnings=ws@content.warnings})

  let get_warnings (Signature (_,{warnings=ws})) = ws
      
  let to_string ((Signature (name,dec)) as sg) =
    sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list_rev
	 "\n"
	 (function
	    | Type_decl (id,_,K types) -> 
                (match types 
                 with [] -> sprintf "\t%s: type;" id
                   | _  -> sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," (fun s -> type_def_to_string s sg) types))
	    | Type_def (id,_,value) -> sprintf "\t%s = %s: type;" id (type_def_to_string value sg)
	    | Term_decl (id,_,_,ty) -> 
		let t = match snd (StringMap.find id dec.term_definitions) with
		  | Default -> ""
		  | Infix -> "infix "
		  | Prefix -> "prefix "
		  | Binder -> "binder " in
		  sprintf "\t%s%s: %s;" t id (type_def_to_string ty sg)
	    | Term_def (id,_,_,value,type_of) -> 
		let t = match snd (StringMap.find id dec.term_definitions) with
		  | Default -> ""
		  | Infix -> "infix "
		  | Prefix -> "prefix "
		  | Binder -> "binder " in
		  sprintf "\t%s%s = %s: %s;" t id (term_to_string value sg) (type_def_to_string type_of sg))
	 dec.entries)
      
  let new_loc (s,_) (_,e) = (s,e)
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
      (*print_string "signature "; print_string name; print_string " = \n{"; print_string " ENTRIES : \n";*)
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
	  (*print_string (type_def_display s sg))(*Utils.string_of_list "," (fun s -> type_def_display s sg) types*)*)
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
      decl_display dec.entries;
	  
end
  
  
module rec Abstract_typ : 
  
sig
      

  exception Duplicate_type_definition
  exception Duplicate_term_definition

 module Table : TABLE


(*   type term_kind = *)
(*     | Default *)
(*     | Prefix *)
(*     | Infix *)
(*     | Binder *)

  type type_of_definition =
    | Declared
    | Defined

  type abs =
    | Linear
(*    | Non_linear *)

  type term =
    | Var of int
	(** If the term is variable (bound by a binder)*)
    | Const of int
	(** If the term is a constant (not bound by a binder) *)
(*    | Abs of string * term
	(** If the term is a intuitionistic abstraction *) *)
    | LAbs of string * term
	(** If the term is a linear abstraction *)
    | App of term * term
	(** If the term is an application *)	

  type type_def = 
    | Type_atom of int * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def
	(** If the type is described with a linear abstraction *)
(*    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *) *)

  (** The type of kinds as found in the signature files *)
  type kind = K of type_def list

  type sig_entry = 
    | Type_decl of (string * int * kind)
	(** The first parameter ([string]) is the name of the type,
	    the second parameter its indexd and the last parameter is
	    its kind *)
    | Type_def of (string * int * type_def)
	(** The first parameter ([string]) is the name of the defined
	    type, the second parameter its index and the last
	    parameter is its value *)
    | Term_decl of (string * int * Abstract_sig.term_kind * type_def)
	(** The first parameter ([string]) is the name of the
	    constant, the second parameter is its index and the last
	    parameter is its type *)
    | Term_def of (string * int * Abstract_sig.term_kind * term * type_def)
	(** The first parameter ([string]) is the name of the
	    constant, the second parameter is its index and the last
	    parameter is its value *)


  type sig_content = 
      {type_definitions: Abstract_sig.type_of_definition StringMap.t;
       term_definitions: (Abstract_sig.type_of_definition*Abstract_sig.term_kind) StringMap.t}

  val content2content : Abstract_sig.sig_content -> sig_content

  type t = Signature of string * int * sig_entry Table.t * sig_entry
      Tries.t * sig_content (** The first string is the name of the
      signature and the int is its size *)

 
  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string
    
  (** [term_to_string t sg] returns a string describing the term [t]. *)
  val term_to_string : term -> t -> string

  (** [type_def_to_string t sg ] returns a string describing the type definition [t] *)
  val type_def_to_string : type_def -> t -> string


  end
=
struct
      

  exception Duplicate_type_definition
  exception Duplicate_term_definition

(*   type term_kind = *)
(*     | Default *)
(*     | Prefix *)
(*     | Infix *)
(*     | Binder *)

  type type_of_definition =
    | Declared
    | Defined

  type abs =
    | Linear
(*    | Non_linear *)

  type term =
    | Var of int
	(** If the term is variable (bound by a binder)*)
    | Const of int
	(** If the term is a constant (not bound by a binder) *)
(*    | Abs of string * term
	(** If the term is a intuitionistic abstraction *) *)
    | LAbs of string * term
	(** If the term is a linear abstraction *)
    | App of term * term
	(** If the term is an application *)	

  type type_def = 
    | Type_atom of int * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def
	(** If the type is described with a linear abstraction *)
(*    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *) *)

  type kind = K of type_def list

  type sig_entry = 
    | Type_decl of (string * int * kind)
	(** The first parameter ([string]) is the name of the type,
	    the second parameter its indexd and the last parameter is
	    its kind *)
    | Type_def of (string * int * type_def)
	(** The first parameter ([string]) is the name of the defined
	    type, the second parameter its index and the last
	    parameter is its value *)
    | Term_decl of (string * int * Abstract_sig.term_kind * type_def)
	(** The first parameter ([string]) is the name of the
	    constant, the second parameter is its index and the last
	    parameter is its type *)
    | Term_def of (string * int * Abstract_sig.term_kind * term * type_def)
	(** The first parameter ([string]) is the name of the
	    constant, the second parameter is its index and the last
	    parameter is its value *)


  module Table = Make_table (struct let b = 10 end)


  type sig_content = 
      {type_definitions: Abstract_sig.type_of_definition StringMap.t;
       term_definitions: (Abstract_sig.type_of_definition*Abstract_sig.term_kind) StringMap.t}

  let content2content (content : Abstract_sig.sig_content) =

    {type_definitions = content.Abstract_sig.type_definitions;
     term_definitions = content.Abstract_sig.term_definitions}
 

  type t = Signature of string * int * sig_entry Table.t * sig_entry
      Tries.t * sig_content (** The first string is the name of the
      signature and the int is its size *)

  let is_infix id (Signature (_, _, _, _, {type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
      (t=Abstract_sig.Infix)
    with
      | Not_found -> false

  let is_binder id (Signature (_, _, _, _, {type_definitions=_;term_definitions=defs})) =
    try
      let _,t = StringMap.find id defs in
	t = Abstract_sig.Binder
    with
      | Not_found -> false

  let rec term_to_string t (sg:t) = 
    match t with
    | Var(i) -> Signature.string_of_const i sg
      | Const (i) -> Signature.string_of_const i sg
	  (*    | Abs (s,t,_) -> 
		let vars,u=unfold_abs [s] t in
		sprintf
		"Lambda %s. %s"
		(Utils.string_of_list " " (fun x -> x) (List.rev vars))
		(term_to_string u) *)
      | LAbs (s,t) -> 
	  let vars,u=unfold_labs [s] t in
	    sprintf
	      "(lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (term_to_string u sg)
      | App (Const (i),(LAbs(x,u) as t)) when is_binder (Signature.string_of_const i sg) sg ->
	  let s = string_of_int i in
	  let vars,u= unfold_binder s sg [x] u in
	  sprintf
	    "(%s %s. %s)"
	    s
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u sg) 
      | App ((App (Const (i),t1)),t2) when is_infix (Signature.string_of_const i sg) sg ->
	  sprintf
	    "(%s %s %s)"
	    (term_to_string t1 sg)
	    (string_of_int i)
	    (term_to_string t2 sg)
      | App (t1,t2) ->
	  sprintf
	    "(%s %s)"
	    (term_to_string t1 sg)
	    (term_to_string t2 sg)
	    (*  and unfold_abs acc = function
		| Abs (s,t,_) -> unfold_abs (s::acc) t
		| t -> acc,t *)
  and unfold_labs acc = function
    | LAbs (s,t) -> unfold_labs (s::acc) t
    | t -> acc,t
  and unfold_app acc = function
    | App (t1,t2) -> unfold_app (t2::acc) t1
    | t -> acc,t
  and unfold_binder binder sg acc = function
    | App (Const (i),(LAbs(x,u) as t)) when let s = (Signature.string_of_const i sg) in (is_binder s sg)&&(s=binder) -> unfold_binder binder sg (x::acc) u
    | t -> acc,t
	
  let rec is_atomic_type = function
    | Type_atom _ -> true
(*    | Dep (_,t,_) -> is_atomic_type t  *)
    | _ -> false
      
  let is_arrow = function
(*    | Arrow _ -> true *)
    | Linear_arrow _ -> true
    | _ -> false

  let rec type_def_to_string def sg = match def with
    | Type_atom (i,terms) ->
	(match terms with
             [] -> (string_of_int i)
           | _  -> sprintf "%i %s" i (Utils.string_of_list " " (fun x -> sprintf "(%s)" (term_to_string x sg )) terms))
    | Linear_arrow (t1,t2) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg  else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s -> %s"
	    (Utils.string_of_list
	       " -> "
	       (fun x -> if is_atomic_type x then type_def_to_string x sg  else sprintf "(%s)" (type_def_to_string x sg ))
	       (List.rev arrows))
	    u_string
(*    | Arrow (t1,t2,_) -> 
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg else sprintf "(%s)" (type_def_to_string u sg ) in
	  sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if (is_atomic_type x) then type_def_to_string x else sprintf "(%s)" (type_def_to_string x))
	       (List.rev arrows))
	    u_string
    | Dep ((s,_,ty1),ty2,_) ->
	let deps,u = unfold_dep [(s,ty1)] ty2 in
	sprintf 
	  "(%s) %s"
	  (Utils.string_of_list ", " (fun (id,t) -> sprintf "%s:%s" id (type_def_to_string t)) (List.rev deps))
          (if is_atomic_type u
	   then
	     type_def_to_string u
           else
	     sprintf "(%s)" (type_def_to_string u))
    | Type_Abs ((s,_,ty),_) -> 
	let abs,u = unfold_type_abs [s] ty in
	  sprintf
	    "lambda %s.(%s)"
	    (Utils.string_of_list " " (fun x -> x) (List.rev abs))
	    (type_def_to_string ty) *)
  and unfold_linear_arrow acc = function
    | Linear_arrow (t1,t2) -> unfold_linear_arrow (t1::acc) t2
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
	
(*  let empty s = Abstract_sig.Signature (s,{entries=[];type_definitions=StringMap.empty;term_definitions=StringMap.empty})
    *)
(*  type t = Signature of string * int * sig_entry Table.t * sig_entry
      Tries.t * sig_content*)

  let to_string ((Signature (name,_,_,trie,content)) as sg) =
    sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list_rev
	 "\n"
	 (function
	   | Type_decl (id,_,K types) -> 
                (match types 
                 with [] -> sprintf "\t%s: type;" id
                   | _  -> sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," (fun s -> type_def_to_string s sg) types))
	    | Type_def (id,_,value) -> sprintf "\t%s = %s: type;" id (type_def_to_string value sg)
	    | Term_decl (id,_,_,ty) -> 
		let t = match snd (StringMap.find id content.term_definitions) with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder " in
		  sprintf "\t%s%s: %s;" t id (type_def_to_string ty sg)
	    | Term_def (id,_,_,value,type_of) -> 
		let t = match snd (StringMap.find id content.term_definitions) with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder " in
		  sprintf "\t%s%s = %s: %s;" t id (term_to_string value sg) (type_def_to_string type_of sg))
	 (Tries.content trie))

end



and Signature :     sig
      
      val create : string * Abstract_typ.sig_content -> Abstract_typ.t
       
    val size : Abstract_typ.t -> int

    val insert_type_dcl : string -> Abstract_typ.kind ->
      Abstract_typ.t -> Abstract_typ.t

    val insert_term_dcl : string -> Abstract_sig.term_kind ->
      Abstract_typ.type_def -> Abstract_typ.t -> Abstract_typ.t

    val insert_var : string -> Abstract_sig.term_kind ->
      Abstract_typ.type_def -> Abstract_typ.t -> Abstract_typ.t

    val insert_term_def : string -> Abstract_sig.term_kind ->
      Abstract_typ.term ->
      Abstract_typ.type_def -> Abstract_typ.t -> Abstract_typ.t

    val lookup : int -> Abstract_typ.t -> Abstract_typ.sig_entry

    val get_const : Abstract_typ.t -> string -> int *
	Abstract_sig.term_kind * Abstract_typ.type_def

    val get_const_ind : Abstract_typ.t -> string -> int 

    val get_atom : Abstract_typ.t -> string -> int *
        Abstract_typ.kind
	  
    val get_atom_ind : Abstract_typ.t -> string -> int
	  
    val string_of_const : int -> Abstract_typ.t -> string 

    val string_of_atom : int -> Abstract_typ.t -> string 

(*
    let kind_of_atom i (Signature (_, tb)) =
      match Table.lookup i tb with
        Type_declaration (_, ki) -> ki
      | Term_declaration _       -> raise (Failure "Signature.kind_of_atom")
  
*)  

(*     let is_a_cst id (Signature (_, _, _, tr, _)) = *)
(*       try *)
(* 	match (Tries.lookup id tr) with *)
(*           | Term_decl _ -> true *)
(* 	  | _ -> false *)
(*       with *)
(* 	| Tries.Not_found -> false *)


(*     let is_a_type id (Signature (_, _, _, tr, _)) = *)
(*       try *)
(* 	match (Tries.lookup id tr) with *)
(*           | Type_decl _ -> true *)
(* 	  | _ -> false *)
(*       with *)
(* 	| Tries.Not_found -> false *)

  end
= 

  struct
open Abstract_typ 
   let create(name, content) = Signature (name,0, Abstract_typ.Table.create(), Tries.empty,content)

    let size (Signature (_, n, _, _, _)) = n

    let insert_type_dcl id ki (Signature (name,size, tb, tr,content)) =
      let e = Type_decl (id, size , ki)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr, content)

    let insert_term_dcl id tk ty (Signature (name,size, tb, tr,content)) =
      let e = Term_decl (id, size, tk, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let insert_term_def id tk te ty 
	(Signature (name,size, tb, tr,content)) =
      let e = Term_def (id, size, tk, te, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let insert_var id tk ty 
	(Signature (name,size, tb, tr,content)) =
      let e = Term_def (id, size, tk, Var(size), ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let lookup i (Signature (_, _, tb, _, _)) = Table.lookup i tb

    let get_const (Signature (_, _, _, tr, _)) id =
      try
      (match (Tries.lookup id tr) with
        Term_decl (_, i, tk, ty) -> (i, tk, ty)
      | Term_def (_, i, tk, _, ty) -> (i, tk, ty)
      | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_const_ind (Signature (_, _, _, tr, _)) id =
      try
      (match (Tries.lookup id tr) with
        Term_decl (_, i, _, _) -> i
      | Term_def (_, i, _, _, _) -> i
      | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_atom (Signature (_, _ , _, tr,_)) id =
      try
      (match (Tries.lookup id tr) with
        Type_decl (_, i, ki) -> (i, ki)
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let get_atom_ind (Signature (_, _ , _, tr,_)) id =
      try
      (match (Tries.lookup id tr) with
        Type_decl (_, i, _) -> i
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let string_of_const i (Signature (_, _, tb, _, _)) =
      match Table.lookup i tb with
        Type_decl _ -> raise (Failure "Signature.string_of_const")
      | Term_decl (x, _, _, _) -> x
      | Term_def (x, _, _, _, _) -> x
      | _ -> raise (Failure "string_of_const Not yet implemented")

    let string_of_atom i (Signature (_, _, tb, _, _)) =
      match Table.lookup i tb with
        Type_decl (x, _, _) -> x 
      | Term_decl _ -> raise (Failure "Signature.string_of_atom")
      | Term_def _ -> raise (Failure "Signature.string_of_atom")
      | _ -> raise (Failure "string_of_atom Not yet implemented")

(*
    let kind_of_atom i (Signature (_, tb)) =
      match Table.lookup i tb with
        Type_declaration (_, ki) -> ki
      | Term_declaration _       -> raise (Failure "Signature.kind_of_atom")
  
*)  

    let is_a_cst id (Signature (_, _, _, tr, _)) =
      try
	match (Tries.lookup id tr) with
	| Term_decl _ -> true
	| Term_def _ -> true
	| _ -> false
      with
	| Tries.Not_found -> false


    let is_a_type id (Signature (_, _, _, tr, _)) =
      try
	match (Tries.lookup id tr) with
          | Type_decl _ -> true
	  | _ -> false
      with
	| Tries.Not_found -> false

	    

  end


