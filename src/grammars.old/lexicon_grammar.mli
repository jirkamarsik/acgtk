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

open Environment
open Abstract_syntax
open Signature_grammar

module Lexicon_grammar :
sig
  module type Lexicon_grammar =
  sig
    
    include Signature_grammar.Signature_grammar
      
    val lexicon : (Environment.t -> Abstract_lexicon.t) Entry.e
  end
    
  module Make (G:Signature_grammar.Signature_grammar) : Lexicon_grammar
end
