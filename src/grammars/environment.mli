open Interface

  
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
  module Signature:Signature_sig
  module Lexicon:Lexicon_sig with type signature=Signature.t

  (** The type of the environment *)
  type t

  (** The type of what an environment can contain *)
  type entry = 
    | Signature of Signature.t
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
  val get_signature : string -> t -> Signature.t

  (** [iter f e] applies f to every data contained in the environment
  *)
  val iter : (entry -> unit) -> t -> unit

  (** [fold f a e] returns [f a_n (f a_n-1 (... (f a1 (f a0 a))
      ... ))] where the [a_0 ... a_n] are the [n+1] elements of the
      environmnent *)
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a

  (** [sig_number e] returns the number of signature an environment
      contains *)
  val sig_number : t -> int

  (** [choose_signature e] returns a randomly chosen signature in the
      environment [e] *)
  val choose_signature : t -> Signature.t option
end


(** The functor that builds the environment *)
module Make  (Sg:Signature_sig)(Lex:Lexicon_sig with type signature = Sg.t) : Environment_sig 
  with type Signature.t=Sg.t and type Lexicon.t = Lex.t
