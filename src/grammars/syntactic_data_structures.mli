open Abstract_syntax

(** This modules implements the abstract syntax and the build function for the signatures *)

module Abstract_sig : 
sig
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  exception Duplicate_type_definition
  exception Duplicate_term_definition
    
  (** The type of terms provided by the parser. *)
  type term 

  (** The type of the signature as abstract object *)
  type t

  (** [empty name] returns the empty signature of name [name] *)
  val empty : string * Abstract_syntax.location -> t
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
  val add_entry : Abstract_syntax.sig_entry -> t -> t

  (** [add_warnings w s ] resturns a signature where the warning [w] have been added *)
  val add_warnings : Error.warning list -> t -> t
    
  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string
    
  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val term_to_string : term -> t -> string
    
  (** [is_atomic_ype id s ] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s ] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option
    
  (** [display sg] prints the signature [sg] on stdout. *)
(*  val display : t -> unit*)
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string * Abstract_syntax.location)

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list

  (** [convert t] returns a term of type
      {!Abstract_syntax.Abstract_syntax.term}, such as the ones stored in the
      signature *)
  val convert : Abstract_syntax.term -> term 
    
  (** To be removed: only for compatibility *)
  type type_of_definition =
    | Declared
    | Defined
	
 (** [fold f a sg] returns [f e_n (f e_n-1 ( ... ( f e_1 a) ... ))]
     where the [e_i] are the entries of the signature [sg]. It is
     ensured that the [e_i] are provided in the same order as they
     have been inserted. *)
  val fold : (Abstract_syntax.sig_entry -> 'a -> 'a) -> 'a -> t -> 'a

end

module Abstract_lex :
sig
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  (** The type of the lexicon *)
  type t
    
  (** We constrain the type of signature [signature] provided by the
      lexicon to correspond to a type [t] provided by an actual signature *)
  type signature=Abstract_sig.t

  (** [name l] returns the name of the lexicon [l] and the location of its definition *)
  val name : t -> string * Abstract_syntax.location

  (** [empty (n,l) abs:a obj:o] returns the empty lexicon from the
      abstract signature [a] to the object signature [o] *)
  val empty : string*Abstract_syntax.location -> abs:Abstract_sig.t -> obj:Abstract_sig.t -> t

  (** [insert e l] insert the lexicon entry [e] to the lexicon [l] **)
  val insert : Abstract_syntax.lex_entry -> t -> t

  (** [to_string l] returns a string describing the lexicon
      [l]. Should be parsable *)
  val to_string : t -> string
end

