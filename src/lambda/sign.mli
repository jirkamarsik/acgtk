open Table
open Tries
open Abstract_syntax
open Lambda

(** This module signature describes the interface for modules implementing signatures *)
module Sign :
sig
  
  (** The type of the signature as abstract object *)
  type t

  (** The (ocaml) type for the terms of the signature *)
  type term
    
  (** The type of the entries of the signature *)
  type entry

  (** The (ocaml) type for the types of the signature *)
  type stype

  (** [empty name] returns the empty signature of name [name] *)
  val empty : (string*Abstract_syntax.location) -> t
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string*Abstract_syntax.location)
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
  val add_entry : Abstract_syntax.sig_entry -> t -> t 
      
  (** [is_atomic_ype id s ] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s ] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option
    
  (** [id_to_string id sg] returns the a syntactic behaviour together
      with a string describing the term or the type of [sg] whose
      identifier in (as [i] in [Lambda.Atom i] of type
      {!Lambda.Lambda.stype}) is [id]. If [id] corresponds to a type,
      the syntactic behaviour is meaningless and should not be relied on *)
  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string

  (** [unfold_type_definition id t] returns the actual type for the
      type defined by [Lambda.DAtom id]. Fails with "Bug" if [id] does
      not correspond to a type definition *)

  val unfold_type_definition : int -> t -> Lambda.stype

  (** [unfold_term_definition id t] returns the actual term for the
      term defined by [Lambda.DConst id]. Fails with "Bug" if [id]
      does not correspond to a term definition *)

  val unfold_term_definition : int -> t -> Lambda.term
    
  (** [add_warnings w s ] resturns a signature where the warning [w] have been added *)
  val add_warnings : Error.warning list -> t -> t

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list

  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string

  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val term_to_string : term -> t -> string

  (** [type_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val type_to_string : stype -> t -> string
    
  (** [convert_term t ty sg] returns a the term corresponding to the
      parsed term [t] with parsed type [ty] wrt to the signature [sg]
  *)
  val convert_term : Abstract_syntax.term -> Abstract_syntax.type_def -> t -> term * stype

  (** [convert_type ty sg] returns a type to the parsed type [ty] wrt
      to the signature [sg] *)
  val convert_type : Abstract_syntax.type_def -> t -> stype

  (** [type_of_constant n sg] returns the type of the constant of name
      [n] as defined in the signature [sg] *)
  val type_of_constant : string -> t -> stype

  (** [typecheck t ty sg] returns a term if, according to [sg], it has
      type [ty] *)
  val typecheck : Abstract_syntax.term -> stype -> t -> term

  (** [fold f a sg] returns [f e_n (f e_n-1 ( ... ( f e_1 a) ... ))]
      where the [e_i] are the entries of the signature [sg]. It is
      ensured that the [e_i] are provided in the same order as they
      have been inserted. *)
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a

  (** [get_functionnal_type s sg] returns [None] if the constant [s] is
      not defined in [sg] with a functionnal type, and returns [Some abs]
      where [abs] is {!Abstract_syntax.Abstract_syntax.Linear} or
      {!Abstract_syntax.Abstract_syntax.Non_linear} otherwise *)
  val get_binder_argument_functional_type : string -> t -> Abstract_syntax.abstraction option

end

