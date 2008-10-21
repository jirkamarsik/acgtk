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

(** This module implements a grammar for lamda terms *)

open Signature
open Abstract_syntax
open Base_grammar


module Term_grammar :
sig
  (** The module type to parse lambda terms *)
  
  module type Term_grammar =
  sig
    include Base_grammar.Base_grammar
      
    type type_or_cst =
      | Type of Abstract_lexicon.type_def
      | Const of Abstract_lexicon.term
	  
    (** The entry that parses lambda terms *)
    val term : Abstract_sig.term Entry.e
    val term_as_parameter : Abstract_sig.term  Entry.e
    val entry : (Signature.t -> type_or_cst) Entry.e
    val add_binder : string -> unit
    val add_infix : string -> unit
    val add_prefix : string -> unit
    val add_outfix: (string*string) -> unit
  end
    
    
  (** This module builds a module that parse lambda terms when provided
      with a grammar *)
  module Make (G:Base_grammar.Base_grammar) : Term_grammar
end
