open Abstract_syntax

(** This module signature describes the interface for modules implementing signatures *)
module type Signature_sig =
sig
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  
  exception Duplicate_type_definition
  exception Duplicate_term_definition

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
  val add_entry : Abstract_syntax.sig_entry -> t -> t
    
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

  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string

  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val term_to_string : term -> t -> string
    
  (** [convert t sg] returns a the term corresponding to the parsed term
      [t] wrt to the signature [sg] *)
  val convert : Abstract_syntax.term -> term
end

(** This module signature describes the interface for modules implementing lexicons *)
module type Lexicon_sig =
sig
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  type t
  type signature
  val empty : (string*Abstract_syntax.location) -> abs:signature -> obj:signature -> t
  val name : t -> (string*Abstract_syntax.location)
  val insert : Abstract_syntax.lex_entry -> t -> t
  val to_string : t -> string
end
