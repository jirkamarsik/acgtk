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


and Signature :

    sig
      
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

