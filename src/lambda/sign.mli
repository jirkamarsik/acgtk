open Table
open Tries
open Abstract_syntax
open Lambda

(** This module signature describes the interface for modules implementing signatures *)
module Sign :
sig
  
  module Table : TABLE

  (** The type of the signature as abstract object *)
  type t

  (** The (ocaml) type for the terms of the signature *)
  type term
    
  (** [empty name] returns the empty signature of name [name] *)
  val empty : (string*Abstract_syntax.location) -> t
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string*Abstract_syntax.location)
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
(*   val add_entry : Abstract_syntax.sig_entry -> t -> t *)
    
  (** [is_atomic_ype id s ] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s ] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option
    
  (** [add_warnings w s ] resturns a signature where the warning [w] have been added *)
  val add_warnings : Error.warning list -> t -> t

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list

(*   (\** [to_string sg] returns a string describing the signature *)
(*       [sg]. Should be parsable *\) *)
(*   val to_string : t -> string *)

(*   (\** [term_to_string t sg] returns a string describing the term [t] *)
(*       wrt the signature [sg]. *\) *)
(*   val term_to_string : term -> t -> string *)
    
  (** [convert t sg] returns a the term corresponding to the parsed term
      [t] wrt to the signature [sg] *)
  val convert : Abstract_syntax.term -> t -> term

(*****************************)

  val find : int -> 'a list -> 'a

  val string_of_const : int -> t -> string

  val add_assoc : (string * int) list -> string -> (string * int) list

  val is_binder : string -> t -> bool

  val is_infix : string -> t -> bool

  val string_of_atom : int -> t -> string
      
  type sig_entry =
    | Type_decl of (string * int * Lambda.kind)
	  (** The first parameter ([string]) is the name of the type,
	      the second parameter its indexd and the last parameter is
	      its kind *)
    | Type_def of (string * int * Lambda.stype)
	  (** The first parameter ([string]) is the name of the defined
	      type, the second parameter its index and the last
	      parameter is its value *)
    | Term_decl of (string * int * Abstract_syntax.syntactic_behavior * Lambda.stype)
	  (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
	      parameter is its type *)
    | Term_def of (string * int * Abstract_syntax.syntactic_behavior * Lambda.term * Lambda.stype)
	  (** The first parameter ([string]) is the name of the
	      constant, the second parameter is its index and the last
	      parameter is its value *)

  val get_trie : t -> sig_entry Tries.t

  val size : t -> int

  val lookup : int -> t -> sig_entry

  val insert_type_decl : string -> Lambda.kind -> t -> t

  val insert_type_def : string -> Lambda.stype -> t -> t

  val insert_term_decl : 
      string -> Abstract_syntax.syntactic_behavior -> Lambda.stype -> t -> t
	
  val insert_term_def : 
      string -> Abstract_syntax.syntactic_behavior -> Lambda.term -> Lambda.stype -> t -> t
	
  val insert_var : 
      string -> Abstract_syntax.syntactic_behavior -> Lambda.stype -> t -> t

  val get_atom : t -> string -> int * Lambda.kind

  val get_const : 
      t -> string -> int * Abstract_syntax.syntactic_behavior * Lambda.stype * bool

  val get_ind : ('a * 'b) list -> 'a -> 'b

  val cut_assoc : ('a * 'b) list -> 'a -> ('a * 'b) list

  val cut : 'a list -> Lambda.term -> 'a list

end

