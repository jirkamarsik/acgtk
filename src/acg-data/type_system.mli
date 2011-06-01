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

module type SIG_ACCESS =
sig
  exception Not_found
  type t
  type entry
  type stype

  val unfold_type_definition :  int -> t -> stype
  val find_term : string -> t -> entry
  val id_to_string : t -> int -> Abstract_syntax.syntactic_behavior*string
end

module Type_System :
sig
  module Make(Signature:SIG_ACCESS
	      with
	      (*		type term = Lambda.term
				and *)
		type stype = Lambda.stype 
	      and type entry = Interface.sig_entry) : 
  sig
    val typecheck : Abstract_syntax.term -> Lambda.stype -> Signature.t -> Lambda.term
  end
end

      
