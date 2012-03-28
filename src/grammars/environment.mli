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

open Interface

open Abstract_syntax

(** This modules implements a functor that build an environment
    containing signatures and lexicons when provided with to actual
    implementations of a signature and a lexicon *)


(** This module signature describes the interface for modules implementing environments *)
module type Environment_sig =
sig

  (** This exception can be raised when a signature or an entry is not
      found in the environmnent *)
  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string

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
  val insert : ?override:bool -> entry -> t -> t

  (** [get_signature name e] returns the signature of name [name] in
      the environment [e]. Raise
      {!Environment.Environment_sig.Signature_not_found} if such a
      signature does not exist *)
  val get_signature : string -> t -> Signature1.t

  (** [get_lexicon name e] returns the signature of name [name] in
      the environment [e]. Raise
      {!Environment.Environment_sig.Lexicon_not_found} if such a
      signature does not exist *)
  val get_lexicon : string -> t -> Lexicon.t

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

  val select : string -> t -> t

  val unselect : t -> t

  val focus : t -> entry option
end


(** The functor that builds the environment *)
module Make  (Lex:Interface.Lexicon_sig) : Environment_sig 
  with type Signature1.t=Lex.Signature.t and type Lexicon.t = Lex.t and type Signature1.term = Lex.Signature.term and type Signature1.stype = Lex.Signature.stype
