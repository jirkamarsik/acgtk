(*open Interface*)

open Abstract_syntax

(* A simple interface for environmnent *)
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
  val get_binder_argument_functional_type : string -> t -> Abstract_syntax.abstraction option


end



  
(** This modules implements a functor that build an environment
    containing signatures and lexicons when provided with to actual
    implementations of a signature and a lexicon *)


(** This module signature describes the interface for modules implementing environments *)
module type Environment_sig =
sig

  (** This exception can be raised when a signature is not found in
      the environmnent *)
  exception Signature_not_found of string

  (** The modules implementing the signatures and the lexicons managed
      by the environment *)
  module Signature1:Signature_sig
  module Lexicon:Interface.Lexicon_sig with type Signature.t=Signature1.t and type Signature.term=Signature1.term and type Signature.stype=Signature1.stype

  (** The type of the environment *)
  type t

  (** The type of what an environment can contain *)
  type entry = 
    | Signature of Signature1.t
    | Lexicon of Lexicon.t

  (** [empty] is the empty environmnent *)
  val empty : t

  (** [insert c e] adds the content [c] into the environment [e] and
      returns the resulting environmnent *)
  val insert : entry -> t -> t

  (** [get_signature name e] returns the signature of name [name] in
      the environment [e]. Raise
      {!Environment.Environment_sig.Signature_not_found} if such a
      signature does not exist *)
  val get_signature : string -> t -> Signature1.t

  val get : string -> t -> entry

  (** [iter f e] applies f to every data contained in the environment
  *)
  val iter : (entry -> unit) -> t -> unit

  (** [fold f a e] returns [f a_n (f a_n-1 (... (f a1 (f a0 a))
      ... ))] where the [a_0 ... a_n] are the [n+1] elements of the
      environmnent *)
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a

  (** [sig_number e] returns the number of signatures an environment
      contains *)
  val sig_number : t -> int

  (** [sig_number e] returns the number of lexicons an environment
      contains *)
  val lex_number : t -> int

  (** [choose_signature e] returns a randomly chosen signature in the
      environment [e] *)
  val choose_signature : t -> Signature1.t option
end


(** The functor that builds the environment *)
module Make  (Lex:Interface.Lexicon_sig) : Environment_sig 
  with type Signature1.t=Lex.Signature.t and type Lexicon.t = Lex.t 
