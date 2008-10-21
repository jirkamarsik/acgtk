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
open Signature

module Lexicon_grammar =
struct
  module type Lexicon_grammar =
  sig
    
    include Signature_grammar.Signature_grammar
      
    val lexicon : (Environment.t -> Abstract_lexicon.t) Entry.e
      
  end
    
    
  module Make(Gram:Signature_grammar.Signature_grammar) =
  struct
    
    include Gram
      
      
    let lexicons = Gram.Entry.create "lexicon list"
    let lexicon = Gram.Entry.create "lexicon" 
      
    let insert_id id assgt sg loc =
      try
	match assgt with
	  | Gram.Const t when Signature.is_a_cst id sg ->
	      (fun lex -> Abstract_lexicon.insert_const_assgt id t loc lex),assgt
	  | Gram.Type t when Signature.is_a_type id sg ->
	      (fun lex -> Abstract_lexicon.insert_type_assgt id t loc lex), assgt
	  | _ -> Stdpp.raise_with_loc loc (Parse_Error "Mixing type and constants or neither type or constant")
      with
	| Signature.Not_found -> Stdpp.raise_with_loc loc (Parse_Error (Printf.sprintf "No constant or type \"%s\" in abstract signature" id))
	    
	    
	    GEXTEND Gram
	      GLOBAL: lexicons lexicon ;
	    lexicons :
	      [ [ lexicons=LIST1 lexicon ; EOI -> lexicons ]];
	    
	    lexicon :
	      [ [ "lexicon" ; name = UIDENT ; "(" ; abs_sg_name = UIDENT ; ")" ; ":" ; obj_sg_name = UIDENT ; "=" ; dec=lexicon_declaration ->
		    (fun env ->
		       let abs_sg = 
			 try
			   Environment.get_signature abs_sg_name env
			 with
			   | Environment.Error s -> Stdpp.raise_with_loc loc (Parse_Error s) in
		       let obj_sg =
			 try Environment.get_signature obj_sg_name env 
			 with
			   | Environment.Error s -> Stdpp.raise_with_loc loc  (Parse_Error s) in
			 (dec (abs_sg,obj_sg)) (Abstract_lexicon.empty name abs_sg_name obj_sg_name))]];
	    lexicon_declaration :
	      [ [ "end" -> (fun sgs lex -> lex)
		| e=lexicon_entry ; ";" ; "end" -> (fun sgs -> fst (e sgs))
		| e=lexicon_entry ;  "end" -> (fun sgs -> fst (e sgs))
		| e=lexicon_entry ; ";" ; dec=lexicon_declaration -> (fun sgs -> (fun lex -> (dec sgs) ( (fst (e sgs)) lex))) ] ];
	    lexicon_entry :
	      [ [
		  x = Gram.luident ; ":=" ; e=Gram.entry -> (fun (a_sg,o_sg) -> insert_id x (e o_sg) a_sg loc)
		| x = Gram.luident ; "," ; e= lexicon_entry ->
		    (fun (a_sg,o_sg) ->
		       let new_lex_f,assgt = e (a_sg,o_sg) in
			 (fun lex -> 
			    let new_lex,_ = insert_id x assgt a_sg loc in
			      new_lex_f (new_lex lex)),assgt) ] ];
	    END
	      
	      
    let parse file env = 
      List.map
	(fun lex_f -> lex_f env)
	(Utils.new_parse_file file (fun x -> Gram.Entry.parse lexicons (Gram.parsable x)) (fun x -> Parse_Error x))
	
	
  end
    
end
