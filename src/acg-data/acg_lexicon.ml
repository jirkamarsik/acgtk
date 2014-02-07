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
open Signature 

module Make (Sg:Interface.Signature_sig with type  term = Lambda.term and type stype = Lambda.stype) =
struct

  exception Duplicate_type_interpretation
  exception Duplicate_constant_interpretation
(*  exception Not_yet_interpreted_value*)
  exception Missing_interpretation of string

  module Dico = Utils.StringMap
  module Signature=Sg

  type signature = Sg.t

  type resume = int AlterTrees.AlternTrees.resumption

  let resume_info (actual,delayed) =
    let length = List.length actual in
    let length = Utils.IntMap.fold (fun _ v acc -> acc+(List.length v)) delayed length in
    Printf.sprintf "%d entries." length

  type interpretation =
    | Type of (Abstract_syntax.location * Lambda.stype )
    | Constant of (Abstract_syntax.location * Lambda.term )


  let interpretation_to_string abstract_type_or_cst_id fun_type_from_id i sg = match i with
    | Type (_,t) -> Printf.sprintf "\t%s" (Signature.type_to_string t sg)
    | Constant (_,c) -> 
	let eta_long = Sg.eta_long_form c (fun_type_from_id abstract_type_or_cst_id) sg  in
	  Printf.sprintf "\t%s [eta-long form: %s {%s}]" (Sg.term_to_string c sg) (Sg.term_to_string eta_long sg ) (Lambda.raw_to_string eta_long)

  module Datalog=Datalog.Datalog

  module RuleToCstMap=Utils.IntMap

  type t = {name:string*Abstract_syntax.location;
	    dico:interpretation Dico.t;
	    abstract_sig:Sg.t;
	    object_sig:Sg.t;
	    datalog_prog:(Datalog.Program.program * Lambda.term RuleToCstMap.t) option}


  let name {name=n}=n

  let get_sig {abstract_sig=abs;object_sig=obj} = abs,obj

  let empty name ~abs ~obj = 
    let prog = if Sg.is_2nd_order abs then Some (Datalog.Program.empty,RuleToCstMap.empty) else None in
    {name=name;dico=Dico.empty;abstract_sig=abs;object_sig=obj;datalog_prog=prog}

  let emit_missing_inter lex lst =
    let lex_name,loc = name lex in
    let abs_name,_ = Sg.name lex.abstract_sig in
      raise (Error.Error (Error.Lexicon_error (Error.Missing_interpretations(lex_name,abs_name,lst),loc)))

  let rec interpret_type abs_ty ({abstract_sig=abs_sg;dico=dico} as lex) =
    match abs_ty with
      | Lambda.Atom i -> 
	  (let abs_ty_as_str = Sg.type_to_string abs_ty abs_sg in
	     try
	       match Dico.find abs_ty_as_str dico with
		 | Type (_,obj_ty) -> obj_ty
		 | Constant _ -> failwith "Bug"
	     with
	       | Not_found -> emit_missing_inter lex [abs_ty_as_str])
      | Lambda.DAtom i -> interpret_type (Sg.unfold_type_definition i abs_sg) lex
      | Lambda.LFun (ty1,ty2) -> Lambda.LFun (interpret_type ty1 lex,interpret_type ty2 lex)
      | Lambda.Fun (ty1,ty2) -> Lambda.Fun (interpret_type ty1 lex,interpret_type ty2 lex)
      | _ -> failwith "Not yet implemented"

  let rec interpret_term abs_t ({abstract_sig=abs_sg;dico=dico} as lex) =
    match abs_t with
      | (Lambda.Var i| Lambda.LVar i) -> abs_t
      | Lambda.Const i -> 
	  (let abs_term_as_str = Sg.term_to_string abs_t abs_sg in
	     try
	       match Dico.find abs_term_as_str dico with
		 | Constant (_,obj_t) -> obj_t
		 | Type _ -> failwith "Bug"
	     with
	       | Not_found -> emit_missing_inter lex [abs_term_as_str] )
      | Lambda.DConst i -> 
	 interpret_term (Sg.unfold_term_definition i abs_sg) lex
      | Lambda.Abs(x,t) -> Lambda.Abs(x,interpret_term t lex)
      | Lambda.LAbs(x,t) -> Lambda.LAbs(x,interpret_term t lex)
      | Lambda.App(t,u) -> Lambda.App(interpret_term t lex,interpret_term u lex)
      | _ -> failwith "Not yet implemented"
    
  let interpret t ty lex = 
    let t_interpretation = (interpret_term t lex) in
(*    let () = Printf.printf "Going_to_normalize:\t%s\n%!" (Lambda.term_to_string  t_interpretation (Sg.id_to_string lex.object_sig)) in*)
      Lambda.normalize ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig) t_interpretation,interpret_type ty lex

  module Reduction=Reduction.Make(Sg)


  let add_rule_for_cst_in_prog name abs_type interpreted_term lex (prog,rule_to_cst) =
    let interpreted_type = (interpret_type abs_type lex) in
    let eta_long_term = 
      Sg.eta_long_form 
	interpreted_term
	interpreted_type
	lex.object_sig in
    LOG "term: %s:%s" (Sg.term_to_string interpreted_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig) LEVEL TRACE;
    LOG "eta-long form: %s" (Sg.term_to_string eta_long_term lex.object_sig) LEVEL TRACE;
    LOG "eta-long form (as caml term): %s" (Lambda.raw_to_caml eta_long_term) LEVEL TRACE;
    LOG "Datalog rule addition: lexicon \"%s\", constant \"%s:%s\" mapped to \"%s:%s\"" (fst lex.name) name (Sg.type_to_string abs_type lex.abstract_sig) (Sg.term_to_string eta_long_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig) LEVEL TRACE;
    let obj_princ_type,obj_typing_env = TypeInference.Type.inference eta_long_term in
    LOG "Interpreting \"%s\" as \"%s=%s\" with principle type: \"%s\"" name (Sg.term_to_string eta_long_term lex.object_sig) (Lambda.raw_to_caml eta_long_term) (Lambda.raw_type_to_string obj_princ_type) LEVEL DEBUG;
    LOG "In the context of:" LEVEL DEBUG ;
    (Utils.IntMap.iter
       (fun k (t,ty) -> LOG "%d --> %s : %s" k (Lambda.raw_to_string t) (Lambda.raw_type_to_string ty) LEVEL DEBUG)
       obj_typing_env);
    let rule,new_prog=Reduction.generate_and_add_rule
      ~abs_cst:(name,abs_type)
      ~obj_princ_type
      ~obj_typing_env
      prog
      ~abs_sig:lex.abstract_sig
      ~obj_sig:lex.object_sig in
    let cst_id,_ = Sg.find_term name lex.abstract_sig in
    new_prog,RuleToCstMap.add rule.Datalog_AbstractSyntax.AbstractSyntax.Rule.id cst_id rule_to_cst
      
      
  let insert e ({dico=d} as lex) = match e with
    | Abstract_syntax.Type (id,loc,ty) -> {lex with dico=Dico.add id (Type (loc,Sg.convert_type ty lex.object_sig)) d}
    | Abstract_syntax.Constant (id,loc,t) ->
      let abs_type=Sg.expand_type (Sg.type_of_constant id lex.abstract_sig) lex.abstract_sig in
      let interpreted_type = (interpret_type abs_type lex) in
      let interpreted_term = 
	Lambda.normalize
	  ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	  (Sg.typecheck t interpreted_type lex.object_sig) in
      let prog = match lex.datalog_prog with
	| None -> None
	| Some p -> 
	  let new_prog= add_rule_for_cst_in_prog id abs_type interpreted_term lex p in
	  Some new_prog in
      {lex with
	dico=Dico.add id (Constant (loc,interpreted_term)) d;
	datalog_prog =prog}
	
  let rebuild_prog lex =
    match lex.datalog_prog with
    | None -> lex
    | Some _ ->
      let new_prog=
	Dico.fold
	  (fun key inter acc ->
	    match inter with
	    | Type (l,stype) -> acc
	    | Constant (l,t) -> 
	      add_rule_for_cst_in_prog
		key
		(Sg.expand_type (Sg.type_of_constant key lex.abstract_sig) lex.abstract_sig) 
		t
		lex
		acc)
	  lex.dico
	  (Datalog.Program.empty,RuleToCstMap.empty) in
      {lex with datalog_prog=Some new_prog}
      

  let parse term dist_type lex =
    match lex.datalog_prog,Sg.expand_type dist_type lex.abstract_sig with
    | None,_ -> 
      let () = Printf.printf "Parsing is not implemented for non 2nd order ACG\n!" in
      [],Utils.IntMap.empty
    | Some (prog,_), (Lambda.Atom _ as dist_type) ->
      let buff=Buffer.create 80 in
      let () = Buffer.add_buffer buff (Datalog_AbstractSyntax.AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract prog)) in
      LOG "Before parsing. Program is currently:" LEVEL DEBUG;
      Utils.log_iteration
	(fun s -> LOG s LEVEL DEBUG)
	(Buffer.contents buff);
      let dist_type_image = interpret_type dist_type lex in
      let obj_term= 
	Sg.eta_long_form
	  (Lambda.normalize
 	     ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	     term)
	  dist_type_image
	  lex.object_sig in
      let obj_princ_type,obj_typing_env = TypeInference.Type.inference obj_term in
      LOG "Going to set a query for the distinguised type \"%s(%s)\"" (Signature.type_to_string dist_type lex.abstract_sig) (Lambda.raw_type_to_string dist_type) LEVEL DEBUG;
      LOG "whose image is \"%s(%s)\"" (Signature.type_to_string dist_type_image lex.object_sig) (Lambda.raw_type_to_string dist_type_image) LEVEL DEBUG;
      LOG "resulting int the principle type \"%s\"" (Lambda.raw_type_to_string obj_princ_type) LEVEL DEBUG;
      let query,temp_prog =
	Reduction.edb_and_query
	  ~obj_term
	  ~obj_type:obj_princ_type
	  ~obj_typing_env
	  ~dist_type
	  prog
	  ~abs_sig:lex.abstract_sig
	  ~obj_sig:lex.object_sig in
      let buff'=Buffer.create 80 in
      let () = Buffer.add_buffer buff' (Datalog_AbstractSyntax.AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract temp_prog)) in
      LOG "Going to solve the query: \"%s\" with the program" (Datalog_AbstractSyntax.AbstractSyntax.Predicate.to_string query temp_prog.Datalog.Program.pred_table temp_prog.Datalog.Program.const_table) LEVEL TRACE;
      Utils.log_iteration
	(fun s -> LOG s LEVEL TRACE)
	(Buffer.contents buff');
      let derived_facts,derivations = Datalog.Program.seminaive temp_prog in
      let parse_forest = Datalog.Program.build_forest ~query:query derivations temp_prog in
      let resume = 
	match parse_forest with
	| [] -> []
	| [f] -> AlterTrees.AlternTrees.init f
	| _ -> failwith "Bug: not fully specified query" in
(*      let () = Datalog.Predicate.format_derivations2 
	~query:query
	temp_prog.Datalog.Program.pred_table
	temp_prog.Datalog.Program.const_table
	derivations in *)
      (List.rev resume,Utils.IntMap.empty)
    | Some _ , _ -> 
      let () = 
	Printf.printf "Parsing is not yet implemented for non atomic distinguished type\n%!" in
      [],Utils.IntMap.empty
      
    
  let get_analysis resume lex =
    LOG "Trying to get some analysis" LEVEL DEBUG;
    match lex.datalog_prog with
    | None -> let () = Printf.printf "Parsing is not yet implemented for non atomic distinguished type\n%!" in None,resume
    | Some (_,rule_id_to_cst) ->
      match AlterTrees.AlternTrees.resumption resume with
      | None,resume -> None,resume
      | Some t,resume ->
	LOG "Got a result. Ready to map it" LEVEL DEBUG;
	Some (AlterTrees.AlternTrees.fold_depth_first
		((fun rule_id -> RuleToCstMap.find rule_id rule_id_to_cst),
		 (fun x y -> Lambda.App(x,y)))
		t),
	resume

  let to_string ({name=n,_;dico=d;abstract_sig=abs_sg;object_sig=obj_sg} as lex) =
    let buff=Buffer.create 80 in
    let () = Printf.bprintf
      buff
      "lexicon %s(%s): %s =\n%send"
      n
      (fst (Sg.name abs_sg))
      (fst (Sg.name obj_sg))
      (match 
	  Dico.fold
	    (fun k i -> function
	    | None -> Some (Printf.sprintf "\t%s := %s;" k (interpretation_to_string k (fun id -> interpret_type (Sg.type_of_constant id abs_sg) lex) i obj_sg))
	    | Some a -> Some (Printf.sprintf "%s\n\t%s := %s;" a k (interpretation_to_string k (fun id -> interpret_type (Sg.type_of_constant id abs_sg) lex) i obj_sg)))
	    d
	    None with
	    | None -> ""
	    | Some s -> Printf.sprintf "%s\n" s) in
    let () = Printf.bprintf buff "\n************************\n" in
    let () = match lex.datalog_prog with
      | None -> Printf.bprintf buff "This lexicon was not recognized as having a 2nd order abstract signature\n" 
      | Some (p,_) -> let () = Printf.bprintf buff "This lexicon recognized as having a 2nd order abstract signature. The associated datalog program is:\n" in
		  Buffer.add_buffer buff (Datalog_AbstractSyntax.AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract p)) in
    Buffer.contents buff

  let check ({dico=d;abstract_sig=abs} as lex) =
    let missing_interpretations =
      Signature.fold
	(fun e acc ->
	   match Sg.is_declared e abs with
	     | Some s ->
		 (try
		    let _ = Dico.find s d in
		      acc
		  with
		    | Not_found -> s::acc) 
	     | None -> acc
	)
	[]
	abs in
      match missing_interpretations with
	| [] -> ()
	| lst -> emit_missing_inter lex lst
	    

  let compose lex1 lex2 n =
    LOG "Compose %s(%s) as %s" (fst(name lex1)) (fst(name lex2)) (fst n) LEVEL TRACE;
    let temp_lex=
      {name=n;
       dico = 
	  Dico.fold
	    (fun key inter acc ->
	      match inter with
	      | Type (l,stype) -> Dico.add key (Type (l,interpret_type stype lex1)) acc
	      | Constant (l,t) -> Dico.add key (Constant (l,Lambda.normalize ~id_to_term:(fun i -> Sg.unfold_term_definition i lex1.object_sig) (interpret_term t lex1))) acc)
	    lex2.dico
	    Dico.empty;
       abstract_sig = lex2.abstract_sig;
       object_sig=lex1.object_sig;
       datalog_prog=lex2.datalog_prog} in
    rebuild_prog temp_lex

  let program_to_buffer lex =
    let buff=Buffer.create 80 in
    let () = match lex.datalog_prog with
      | None -> Printf.bprintf buff "This lexicon was not recognized as having a 2nd order abstract signature\n" 
      | Some (p,_) -> 
	let () = Printf.bprintf buff "This lexicon recognized as having a 2nd order abstract signature. The associated datalog program is:\n" in
	Buffer.add_buffer buff (Datalog_AbstractSyntax.AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract p)) in
    buff



  let query_to_buffer term dist_type lex =
    match lex.datalog_prog,Sg.expand_type dist_type lex.abstract_sig with
    | None,_ -> 
      let buff=Buffer.create 80 in
      let () = Printf.bprintf buff "Parsing is not implemented for non 2nd order ACG\n!" in
      buff
    | Some (prog,_), (Lambda.Atom _ as dist_type) ->
      let dist_type_image = interpret_type dist_type lex in
      let obj_term= 
	Sg.eta_long_form
	  (Lambda.normalize
 	     ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	     term)
	  dist_type_image
	  lex.object_sig in
      let obj_princ_type,obj_typing_env = TypeInference.Type.inference obj_term in
      let query,temp_prog =
	Reduction.edb_and_query
	  ~obj_term
	  ~obj_type:obj_princ_type
	  ~obj_typing_env
	  ~dist_type
	  prog
	  ~abs_sig:lex.abstract_sig
	  ~obj_sig:lex.object_sig in
      let buff = Datalog.Program.edb_to_buffer temp_prog in
      let () = Printf.bprintf buff "Query:\n\t%s?\n" (Datalog_AbstractSyntax.AbstractSyntax.Predicate.to_string query temp_prog.Datalog.Program.pred_table temp_prog.Datalog.Program.const_table) in
      buff
    | Some _ , _ -> 
      let buff=Buffer.create 80 in
      let () = 
	Printf.bprintf buff "Parsing is not yet implemented for non atomic distinguished type\n%!" in
      buff
    
	

end

module Sylvain_lexicon = Make (Sylvains_signature)
