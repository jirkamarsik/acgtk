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

open Table
open Tries
open Abstract_syntax
  
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
      {type_definitions: Abstract_sig.type_of_definition Utils.StringMap.t;
       term_definitions: (Abstract_sig.type_of_definition*Abstract_sig.term_kind) Utils.StringMap.t}

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


  module Table : TABLE = Make_table (struct let b = 10 end)


  type sig_content = 
      {type_definitions: Abstract_sig.type_of_definition Utils.StringMap.t;
       term_definitions: (Abstract_sig.type_of_definition*Abstract_sig.term_kind) Utils.StringMap.t}

  let content2content (content : Abstract_sig.sig_content) =

    {type_definitions = content.Abstract_sig.type_definitions;
     term_definitions = content.Abstract_sig.term_definitions}
 

  type t = Signature of string * int * sig_entry Table.t * sig_entry
      Tries.t * sig_content (** The first string is the name of the
      signature and the int is its size *)

  let is_infix id (Signature (_, _, _, _, {type_definitions=_;term_definitions=defs})) =
    try
      let _,t = Utils.StringMap.find id defs in
      (t=Abstract_sig.Infix)
    with
      | Not_found -> false

  let is_binder id (Signature (_, _, _, _, {type_definitions=_;term_definitions=defs})) =
    try
      let _,t = Utils.StringMap.find id defs in
	t = Abstract_sig.Binder
    with
      | Not_found -> false

  let rec term_to_string t (sg:t) = 
    match t with
    | Var(i) -> Signature.string_of_const i sg
      | Const (i) -> Signature.string_of_const i sg
	  (*    | Abs (s,t,_) -> 
		let vars,u=unfold_abs [s] t in
		Printf.sprintf
		"Lambda %s. %s"
		(Utils.string_of_list " " (fun x -> x) (List.rev vars))
		(term_to_string u) *)
      | LAbs (s,t) -> 
	  let vars,u=unfold_labs [s] t in
	    Printf.sprintf
	      "(lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (term_to_string u sg)
      | App (Const (i),(LAbs(x,u) as t)) when is_binder (Signature.string_of_const i sg) sg ->
	  let s = string_of_int i in
	  let vars,u= unfold_binder s sg [x] u in
	  Printf.sprintf
	    "(%s %s. %s)"
	    s
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u sg) 
      | App ((App (Const (i),t1)),t2) when is_infix (Signature.string_of_const i sg) sg ->
	  Printf.sprintf
	    "(%s %s %s)"
	    (term_to_string t1 sg)
	    (string_of_int i)
	    (term_to_string t2 sg)
      | App (t1,t2) ->
	  Printf.sprintf
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
           | _  -> Printf.sprintf "%i %s" i (Utils.string_of_list " " (fun x -> Printf.sprintf "(%s)" (term_to_string x sg )) terms))
    | Linear_arrow (t1,t2) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg  else Printf.sprintf "(%s)" (type_def_to_string u sg ) in
	  Printf.sprintf
	    "%s -> %s"
	    (Utils.string_of_list
	       " -> "
	       (fun x -> if is_atomic_type x then type_def_to_string x sg  else Printf.sprintf "(%s)" (type_def_to_string x sg ))
	       (List.rev arrows))
	    u_string
(*    | Arrow (t1,t2,_) -> 
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u sg else Printf.sprintf "(%s)" (type_def_to_string u sg ) in
	  Printf.sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if (is_atomic_type x) then type_def_to_string x else Printf.sprintf "(%s)" (type_def_to_string x))
	       (List.rev arrows))
	    u_string
    | Dep ((s,_,ty1),ty2,_) ->
	let deps,u = unfold_dep [(s,ty1)] ty2 in
	Printf.sprintf 
	  "(%s) %s"
	  (Utils.string_of_list ", " (fun (id,t) -> Printf.sprintf "%s:%s" id (type_def_to_string t)) (List.rev deps))
          (if is_atomic_type u
	   then
	     type_def_to_string u
           else
	     Printf.sprintf "(%s)" (type_def_to_string u))
    | Type_Abs ((s,_,ty),_) -> 
	let abs,u = unfold_type_abs [s] ty in
	  Printf.sprintf
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
    Printf.sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list_rev
	 "\n"
	 (function
	   | Type_decl (id,_,K types) -> 
                (match types 
                 with [] -> Printf.sprintf "\t%s: type;" id
                   | _  -> Printf.sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," (fun s -> type_def_to_string s sg) types))
	    | Type_def (id,_,value) -> Printf.sprintf "\t%s = %s: type;" id (type_def_to_string value sg)
	    | Term_decl (id,_,_,ty) -> 
		let t = match snd (Utils.StringMap.find id content.term_definitions) with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder " in
		  Printf.sprintf "\t%s%s: %s;" t id (type_def_to_string ty sg)
	    | Term_def (id,_,_,value,type_of) -> 
		let t = match snd (Utils.StringMap.find id content.term_definitions) with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder " in
		  Printf.sprintf "\t%s%s = %s: %s;" t id (term_to_string value sg) (type_def_to_string type_of sg))
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

  val get_size : Abstract_typ.t -> int

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

    let get_size (Signature (_,s,_,_,_)) =
      s

  end


