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
open Environment
open Syntax
open Base_grammar
open Term_grammar
open Signature_grammar
open Lexicon_grammar

module Parser =
struct
  
  exception Parse_Error of string
    
  type t =
    | Signature of Abstract_sig.t
    | Lexicon of Abstract_lexicon.t
	
	
  module Make (G:Grammar.S with type te = Token.t) =
  struct
    module Gram = Lexicon_grammar.Make(Signature_grammar.Make(Term_grammar.Make(Base_grammar.Make(G))))
	
      include Gram
	
      let data = Gram.Entry.create  "data" 
	
	GEXTEND Gram
	GLOBAL : data ;
	data :
	  [ [ d = LIST1 signature_or_lexicon ; EOI -> d ] ];
	
      signature_or_lexicon :
	[ [ s = Gram.signature -> (fun env -> Signature s)
	  | l = Gram.lexicon -> (fun env -> Lexicon (l env)) ] ];
      END
	
      let parse file = 
	Utils.get_parse_errors
	  file
	  (fun x -> Gram.Entry.parse data (Gram.parsable (Stream.of_channel (open_in x))))
	  file
	  (fun x -> Parse_Error x)

    end
  
  let parse file env = 
    let module Gram = Make(Grammar.GMake(
			     struct
			       type te = Token.t
			       let lexer = Plexer.gmake()
			     end)) in
      try
	let new_env,data_lst =
	  Utils.get_parse_errors
	    file
	    (fun x ->
	       List.fold_left
		 (fun (l_env,acc) data ->
		    let res = data l_env in
		      match res with
			| Signature (Abstract_sig.Signature (name,content)) -> 
			    let new_sg = Abstract_sig.Signature (name,List.rev content) in
			      (Environment.insert_signature name (Syntax.generate_signature new_sg) l_env,(Signature new_sg)::acc)
			| Lexicon _ as lex -> (l_env,lex::acc))
		 x
		 (let result = Gram.parse file in
		  let () = Printf.printf "entry is defined as follows:\n" in
		  let () = Gram.Entry.print Gram.entry in
		  let () = Printf.printf "term is defined as follows:\n" in
		  let () = Gram.Entry.print Gram.term in
		    result))
	    (env,[])
	    (fun x ->  Parse_Error x) in
	  new_env,List.rev data_lst
      with
	| Gram.Parse_Error x -> raise (Parse_Error x)
	| Syntax.Typing_error (t,_,_) ->
            raise (Parse_Error (Utils.error_msg
                                  (Abstract_sig.extract_term_location t)
                                  file
                                  (Printf.sprintf "Typing error for term \"%s\":\n" (Abstract_sig.term_to_string t))))
	| Syntax.Kinding_error (t,_,_) ->
            raise (Parse_Error (Utils.error_msg
                                  (Abstract_sig.extract_type_location t)
                                  file
                                  (Printf.sprintf "Kinding error for type definition \"%s\":\n" (Abstract_sig.type_def_to_string t))))
	| Syntax.Binding_error (s,loc) ->
            raise (Parse_Error (Utils.error_msg loc file (Printf.sprintf "Binding error for \"%s\":\n" s)))
	| Syntax.Conflict (s,loc) ->
            raise (Parse_Error (Utils.error_msg loc file (Printf.sprintf "Conflict for \"%s\":\n" s)))
	
  let to_string = function
    | Signature s -> Abstract_sig.to_string s
    | Lexicon l -> Abstract_lexicon.to_string l
	
end
