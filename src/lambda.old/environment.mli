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

open Signature
open Lexicon

module Environment :
sig
  type t
  exception Error of string
  val empty : t
  val insert_signature : string -> Signature.t -> t -> t
  val insert_lexicon : string -> Lexicon.t -> t -> t
  val get_signature : string -> t -> Signature.t
  val get_lexicon : string -> t -> Lexicon.t
end
