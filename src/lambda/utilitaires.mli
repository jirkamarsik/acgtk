open Table
open Tries
open Abstract_syntax
open Lambda

(** This module signature describes the interface for modules implementing signatures *)
module Utilitaires :
sig
  
  module Table : TABLE

  (** The type of the signature as abstract object *)
  type t

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

  val string_of_atom_is : int -> t -> sig_entry

  val empty : (string*Abstract_syntax.location) -> t

  val get_trie : t -> sig_entry Tries.t

  val get_table : t -> sig_entry Table.t

  val name : t -> (string*Abstract_syntax.location)

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

  val get_atom : t -> string -> int * bool

  val get_type_def : int -> t -> Lambda.stype

  val get_const : 
      t -> string -> int * Abstract_syntax.syntactic_behavior * Lambda.stype * bool

  val get_ind : ('a * 'b) list -> 'a -> 'b

  val cut_assoc : ('a * 'b) list -> 'a -> ('a * 'b) list

  val cut : 'a list -> Lambda.term -> 'a list

end
