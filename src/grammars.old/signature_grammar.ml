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
open Signature
open Environment
open Syntax
open Term_grammar

module Signature_grammar =
struct
  module type Signature_grammar =
  sig
    
    include Term_grammar.Term_grammar
      
    val signature : Abstract_sig.t Entry.e
      
  end
    
    
    
  module Make(Gram:Term_grammar.Term_grammar) = 
  struct
    
    include Gram
      
    let signature_list = Gram.Entry.create "signature list" 
    let signature = Gram.Entry.create "signature" 
      
    let stream_peek_nth n strm =
      try
	Some(List.nth (Stream.npeek n strm) (n-1))
      with
	| Failure "nth" -> None
  
    let type_declaration =
      let rec test lev strm =
	match stream_peek_nth lev strm with             
	  | Some ("", "type") -> true
          | Some ("",";") -> raise Stream.Failure
	  | Some ("","end") -> raise Stream.Failure
	  | Some  _ -> test (lev + 1) strm
	  | None -> raise Stream.Failure in
	Gram.Entry.of_parser  "type_declaration" (test 1)
      
    
	  

  
    let dependent_type =
      let rec test nothing_but_left_par lev strm =
	match nothing_but_left_par,stream_peek_nth lev strm with
	  | true,Some ("","(") -> test true (lev + 1) strm
	  | true, Some (("UIDENT",_)|("LIDENT",_)) -> test false (lev + 1) strm
	  | _ , Some ("", ":") -> true
	  | _ -> raise Stream.Failure in
	Gram.Entry.of_parser "dependent_type" (test true 1)

    let compact lst_syms loc =
      let buff = Buffer.create 5 in
      let rec compact_rec = function
      | [] -> ()
      | s::tl ->
	  let () = compact_rec tl in
	    Buffer.add_string buff s in
      let () = compact_rec lst_syms in
	match Buffer.contents buff with
	  | "" -> Stdpp.raise_with_loc loc (Parse_Error "Bad symbol list: empty")
	  | s -> s
    ;;
  

    
  
    GEXTEND Gram
      GLOBAL: signature_list  signature;
    signature_list :
      [ [ l = LIST1 signature ; EOI -> l ] ];
    signature :
      [ [ "signature" ; s = UIDENT ; "=" ; sig_decl = signature_decl ->
	    sig_decl (Abstract_sig.empty s)    ] ];
    signature_decl :
      [ [ OPT ";" ; "end"   -> (fun sg -> sg)
	| d = entry ; OPT ";" ; "end"  -> d 
	| d = entry ; ";" ; s_decl = signature_decl -> (fun sg -> s_decl (d sg))
	] ] ;
    entry :
      [ [  type_declaration ; l = LIST1 Gram.luident SEP "," ; ":" ;  k=kind ->
	     (fun sg -> List.fold_left (fun sign x -> Abstract_sig.insert_type_decl x k loc sign) sg l)
	| "prefix" ; y = Gram.symbol ; ":" ; t = type_expression ->
	    let x = compact y loc in
	    let () = Gram.add_prefix x in
	      (fun sg -> Abstract_sig.insert_term_decl x t loc sg)
	| "infix" ; y = Gram.symbol ; ":" ; t = type_expression ->
	    let x = compact y loc in
	    let () = Gram.add_infix x in
	      (fun sg -> Abstract_sig.insert_term_decl x t loc sg)
	| "outfix" ; y1 = Gram.symbol ; "." ; y2 = Gram.symbol ; ":" ; t = type_expression ->
	    let x1 = compact y1 loc in
	    let x2 = compact y2 loc in
	    let () = Gram.add_outfix (x1,x2) in
	      (fun sg -> Abstract_sig.insert_term_decl (x1^x2) t loc sg)
	| "binder" ; x = Gram.luident ; ":" ; t = type_expression ->
	    let () = Gram.add_binder x in
	      (fun sg -> Abstract_sig.insert_term_decl x t loc sg)
	| "binder" ; y = Gram.symbol ; ":" ; t = type_expression ->
	    let x = compact y loc in
	    let () = Gram.add_binder x in
	      (fun sg -> Abstract_sig.insert_term_decl x t loc sg)
	| l = LIST1 Gram.luident SEP "," ; ":" ; t = type_expression ->
	    (fun sg -> List.fold_left (fun sign -> fun x -> Abstract_sig.insert_term_decl x t loc sign) sg l) ] ];
    kind : 
      [ [ "type" -> []
	| "(" ; l = LIST1 type_expression SEP "," ; ")" ; "type" -> l  
	] ];
    type_expression :
      [ [ dependent_type ; "(" ;  l = LIST1 depend SEP "," ; ")" ; t = type_expression -> 
	    List.fold_right
	      (fun abs acc ->
		 List.fold_right (fun var acc' -> Abstract_sig.Dep (var,acc',loc)) abs acc)
	      l
	      t
	| t = functional_type -> t   
	] ];
    
    depend :
      [ [ l = LIST1 Gram.luident_loc ; ":" ; t = type_expression ->
	    List.map (fun (id,loc_id) -> (id,loc_id,t)) l
	] ];
    
    functional_type : 
      [ [ a_t = atomic_type -> a_t
	| a_t = atomic_type ; "->" ; f = functional_type -> Abstract_sig.Linear_arrow(a_t,f,loc)  
	| a_t = atomic_type ; "=>" ; f = functional_type -> Abstract_sig.Arrow(a_t,f,loc)
	] ];
    
    atomic_type :
      [ [ "(" ; t = type_expression ; ")" -> t
	| x = Gram.luident ; l = LIST0 Gram.term_as_parameter  -> Abstract_sig.Type_atom (x,loc,l) ] ]; 
    
    
    END;;
  
  
    let parse file = 
      List.map
	(fun (Abstract_sig.Signature (name,content)) -> Abstract_sig.Signature(name,List.rev content))
	(Utils.new_parse_file file  (fun s -> Gram.Entry.parse signature_list (Gram.parsable s)) (fun x -> Parse_Error x))

  end
end

