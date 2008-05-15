open Utils
open Tries
open Table

(** This modules implements the abstract syntax and the build function for the signatures *)

module Abstract_sig : 
sig
  
  (** The type of location in the signature files *)
  type location = Lexing.position*Lexing.position
      
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  exception Duplicate_type_definition
  exception Duplicate_term_definition
    
  (** The type of the syntactic behaviour of constants defined in
      the signature *)    
  type term_kind =
    | Default 
    | Prefix 
    | Infix 
    | Binder
	
  (** The type corresponding to the fact that a type (resp. a
      term) comes from a declaration or a definition *)
  type type_of_definition =
    | Declared
    | Defined
	
  (** The type of the different possible abstractions *)
  type abs =
    | Linear
	(*    | Non_linear *)
	
  (** The type of terms provided by the parser. *)
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
	
	
  (** The type of types as found in the signature files *)
	
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
  type t
(*
  type t  = Signature of string * sig_content  (** The first string is the name of the signature *)
  and sig_content = {entries:sig_entry list;
    (** the list of entries comes in the reverse order of declaration *)		     		     type_definitions: type_of_definition StringMap.t;
    (** the map from type definitions to their value *)
	    term_definitions: (type_of_definition*term_kind) StringMap.t;
    (** the map from term definitions to their value and their
	    syntactic behaviour *)
	    warnings: Error.warning list
    (** the list of warnings emitted during parsing *)}
	 
    
and *)
type sig_entry = 
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
	
	
  (** [empty name] returns the empty signature of name [name] *)
  val empty : string * location -> t
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
  val add_entry : sig_entry -> t -> t

  (** [add_warnings w s ] resturns a signature where the warning [w] have been added *)
  val add_warnings : Error.warning list -> t -> t
    
  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string
    
  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val term_to_string : term -> t -> string
    
  (** [type_def_to_string t sg ] returns a string describing the type definition [t]
      wrt the signature [sg]. *)
  val type_def_to_string : type_def -> t -> string

  (** [get_term_location t] returns the location of the term [t]. *)
  val get_term_location : term -> Token.flocation
    
  (** [get_type_location t] returns the location of the type definiton [t]. *)
  val get_type_location : type_def -> Token.flocation
    
  (** [new_loc start end] returns a new location from the beginning of
      [start] to the end of [end]*)
  val new_loc : location -> location -> location
    
  (** [is_atomic_ype id s ] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s ] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * term_kind option

  (** [display sg] prints the signature [sg] on stdout. *)
  val display : t -> unit
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string * location)

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list
    
end


module Environment :
sig
  type t
  type content = 
    | Signature of Abstract_sig.t

  exception Signature_not_found of string

  val empty : t
  val insert : content -> t -> t
  val iter : (content -> unit) -> t -> unit
  val sig_number : t -> int
  val get_signature : string -> t -> Abstract_sig.t
  val choose_signature : t -> Abstract_sig.t option

end
