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

  type interpretation =
    | Type of (Abstract_syntax.location * Lambda.stype )
    | Constant of (Abstract_syntax.location * Lambda.term )


  let interpretation_to_string abstract_type_or_cst_id fun_type_from_id i sg = match i with
    | Type (_,t) -> Printf.sprintf "\t%s" (Signature.type_to_string t sg)
    | Constant (_,c) -> 
	let eta_long = Sg.eta_long_form c (fun_type_from_id abstract_type_or_cst_id) sg  in
	  Printf.sprintf "\t%s [eta-long form: %s {%s}]" (Sg.term_to_string c sg) (Sg.term_to_string eta_long sg ) (Lambda.raw_to_string eta_long)

  module Datalog=Datalog.Datalog

  type t = {name:string*Abstract_syntax.location;
	    dico:interpretation Dico.t;
	    abstract_sig:Sg.t;
	    object_sig:Sg.t;
	    datalog_prog:Datalog.Program.program option}


  let name {name=n}=n

  let get_sig {abstract_sig=abs;object_sig=obj} = abs,obj

  let empty name ~abs ~obj = 
    let prog = if Sg.is_2nd_order abs then Some (Datalog.Program.empty) else None in
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

  let insert e ({dico=d} as lex) = match e with
    | Abstract_syntax.Type (id,loc,ty) -> {lex with dico=Dico.add id (Type (loc,Sg.convert_type ty lex.object_sig)) d}
    | Abstract_syntax.Constant (id,loc,t) ->
      let abs_type=Sg.type_of_constant id lex.abstract_sig in
      let interpreted_type = (interpret_type abs_type lex) in
      let interpreted_term = 
	Lambda.normalize
	  ~id_to_term:(fun i -> Sg.unfold_term_definition i lex.object_sig)
	  (Sg.typecheck t interpreted_type lex.object_sig) in
      let eta_long_term = 
	Sg.eta_long_form 
	  interpreted_term
	  interpreted_type
	  lex.object_sig in
      LOG "term: %s:%s" (Sg.term_to_string interpreted_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig) LEVEL TRACE;
      LOG "eta-long form: %s" (Sg.term_to_string eta_long_term lex.object_sig) LEVEL TRACE;
      LOG "eta-long form (as caml term): %s" (Lambda.raw_to_caml eta_long_term) LEVEL TRACE;
      let prog = match lex.datalog_prog with
	| None -> None
	| Some p -> 
	  LOG "Datalog rule addition: lexicon \"%s\", constant \"%s:%s\" mapped to \"%s:%s\"" (fst lex.name) id (Sg.type_to_string abs_type lex.abstract_sig) (Sg.term_to_string eta_long_term lex.object_sig) (Sg.type_to_string interpreted_type lex.object_sig) LEVEL TRACE;
	  let obj_princ_type,obj_typing_env = TypeInference.Type.inference eta_long_term in
	  let _,new_prog=Reduction.generate_and_add_rule
	    ~abs_cst:(id,abs_type)
	    ~obj_princ_type
	    ~obj_typing_env
	    p
	    ~abs_sig:lex.abstract_sig
	    ~obj_sig:lex.object_sig in
	  Some new_prog in
      {lex with
	dico=Dico.add id (Constant (loc,interpreted_term)) d;
	datalog_prog =prog}

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
      | Some p -> let () = Printf.bprintf buff "This lexicon recognized as having a 2nd order abstract signature. The associated datalog program is:\n" in
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
     datalog_prog=None}
	

end

module Sylvain_lexicon = Make (Sylvains_signature)
