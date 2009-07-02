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

open Abstract_syntax
open Lambda

module type Signature_sig =
sig
  exception Duplicate_type_definition
  exception Duplicate_term_definition

  type t
  type entry
  type term
  type stype
  val empty : (string*Abstract_syntax.location) -> t
  val name : t -> (string*Abstract_syntax.location)
  val add_entry : Abstract_syntax.sig_entry -> t -> t
  val is_type : string -> t -> bool
  val is_constant : string -> t -> bool*Abstract_syntax.syntactic_behavior option
  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string
  val unfold_type_definition : int -> t -> Lambda.stype
  val unfold_term_definition : int -> t -> Lambda.term
  val add_warnings : Error.warning list -> t -> t
  val get_warnings : t -> Error.warning list
  val to_string : t -> string
  val term_to_string : term -> t -> string
(*  val raw_to_string : term -> string*)
  val type_to_string : stype -> t -> string
  val convert_term : Abstract_syntax.term  -> Abstract_syntax.type_def -> t -> term*stype
  val convert_type : Abstract_syntax.type_def -> t -> stype
  val type_of_constant : string -> t -> stype
  val typecheck : Abstract_syntax.term -> stype -> t -> term
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a
  val get_binder_argument_functional_type : string -> t -> Abstract_syntax.abstraction option
  val is_declared : entry -> t -> string option
  val eta_long_form : term -> stype -> t -> term
end

module type Lexicon_sig =
sig
  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation

  type t
  module Signature:Signature_sig
  type signature = Signature.t
  val empty : (string*Abstract_syntax.location) -> abs:signature -> obj:signature -> t
  val name : t -> (string*Abstract_syntax.location)
  val insert : Abstract_syntax.lex_entry -> t -> t
  val to_string : t -> string
  val interpret : Signature.term -> Signature.stype -> t -> (Signature.term*Signature.stype)
  val get_sig : t -> (signature*signature)
  val check : t -> unit
  val compose: t -> t -> (string*Abstract_syntax.location) -> t
end
