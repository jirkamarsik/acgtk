open Datalog_AbstractSyntax
open Lambda
open Utils
open Datalog
  
module Make(Sg:Interface.Signature_sig with type  term = Lambda.term and type stype = Lambda.stype) =
struct

  let rec sequentialize_rev stype sequence =
    match stype with
    | Lambda.Atom i -> i::sequence
    | Lambda.DAtom _ -> failwith "Bug: type definition should be unfolded"
    | Lambda.LFun (alpha,beta)
    | Lambda.Fun (alpha,beta) -> sequentialize_rev beta (sequentialize_rev alpha sequence)
    | _ -> failwith "Bug: Not a 2nd order type"

  let sequentialize stype = List.rev (sequentialize_rev stype [])

  (** [map_types abs_type obj_type sg] returns a list of triple
      [(id_n,name_n,image_n);...;(id_2,name_2,image_2);(id_1,name_1,image_1)]
      where [abst_type=Atom(id_1) -> Atom(id_2) -> ... Atom(id_n)] and
      is defined as [name_1 -> name_2 -> ... -> name_n] and
      [obj_type=image_1 -> image_2 -> ... -> image_n]. Note that the
      list is in the {em reverse order} and that [abs_type] should be
      2nd order. *)
  let map_types abs_type obj_type sg obj_sg=
    let rec map_types_aux abs_type obj_type lst =
      IFDEF BOLT THEN
	LOG "Mapping (aux) type:%s" (Sg.type_to_string abs_type sg) LEVEL TRACE;
      LOG "On (aux):          %s" (Lambda.raw_type_to_string obj_type) LEVEL TRACE;
      END;
      match abs_type,obj_type with
      | Lambda.Atom i,_ -> (i,Sg.type_to_string abs_type sg,obj_type)::lst
      | Lambda.DAtom _,_ -> failwith (Printf.sprintf "Bug: type definition in \"%s\" as \"%s\" should be unfolded" (Sg.type_to_string abs_type sg) (Lambda.raw_type_to_string abs_type))
      | Lambda.LFun (Lambda.Atom i as alpha,beta),Lambda.Fun (alpha',beta')
      | Lambda.Fun (Lambda.Atom i as alpha,beta),Lambda.Fun (alpha',beta') -> 
	map_types_aux beta beta' ((i,Sg.type_to_string alpha sg,alpha')::lst)
      | Lambda.LFun _,Lambda.Fun _
      | Lambda.Fun _,Lambda.Fun _ -> failwith "Bug: should be 2nd order type for abstract constant"
      | _,_ -> failwith "Bug: Not a 2nd order type or not corresponding abstract and object type" in
    IFDEF BOLT THEN
    LOG "Mapping type:%s (%s)" (Sg.type_to_string abs_type sg) (Lambda.raw_type_to_string abs_type) LEVEL TRACE;
    LOG "On:          %s" (Lambda.raw_type_to_string obj_type) LEVEL TRACE;
    END;
    map_types_aux abs_type obj_type []


  let build_predicate_w_var_args (name,obj_type) (prog,var_gen,type_to_var_map) =
    let atom_sequence = sequentialize_rev obj_type [] in
    IFDEF BOLT THEN
    LOG "Build predicate from %s:%s   ([%s])" name (Lambda.raw_type_to_string obj_type) (Utils.string_of_list ";" string_of_int atom_sequence) LEVEL TRACE;
    END;
    let var_sequence,var_gen,type_to_var_map =
      List.fold_left
	(fun (l_var_seq,l_var_gen,l_type_to_var_map) i ->
	  let var,l_var_gen,l_type_to_var_map=
	    try
	      IntMap.find i l_type_to_var_map,l_var_gen,l_type_to_var_map
	    with
	    | Not_found ->
	      let var,l_var_gen=VarGen.get_fresh_id l_var_gen in
	      var,l_var_gen,IntMap.add i var l_type_to_var_map in
	  (AbstractSyntax.Predicate.Var var)::l_var_seq,l_var_gen,l_type_to_var_map)
	([],var_gen,type_to_var_map)
	atom_sequence in
    let p_id,prog=Datalog.Program.add_pred_sym name prog in
    AbstractSyntax.Predicate.({p_id=p_id;arity=List.length var_sequence;arguments=var_sequence}),
    (prog,var_gen,type_to_var_map)


  let build_predicate_w_cst_args (name,obj_type) prog =
    let atom_sequence = sequentialize obj_type in
    IFDEF BOLT THEN
    LOG "Build predicate from %s:%s   ([%s])" name (Lambda.raw_type_to_string obj_type) (Utils.string_of_list ";" string_of_int atom_sequence) LEVEL TRACE;
    END;
    let const_sequence,prog =
      List.fold_left
	(fun (l_const_seq,l_prog) i ->
	  let const_id,l_prog=Datalog.Program.get_fresh_cst_id (string_of_int i) l_prog in
	  (AbstractSyntax.Predicate.Const const_id)::l_const_seq,l_prog)
	([],prog)
	atom_sequence in
    let p_id,prog=Datalog.Program.add_pred_sym name prog in
    AbstractSyntax.Predicate.({p_id=p_id;arity=List.length const_sequence;arguments=List.rev const_sequence}),prog

    

  let generate_and_add_rule
      ~abs_cst:(name,abs_t_type)
      ~obj_princ_type:principle_type
      ~obj_typing_env:env
      prog
      ~abs_sig
      ~obj_sig =
    let rule_id,prog=Datalog.Program.get_fresh_rule_id prog in
    let type_lst = map_types abs_t_type principle_type abs_sig obj_sig in
    match type_lst with
    | [] -> failwith "Bug: there should be a type correspondance"
    | (_,name,image)::tl ->
      let lhs,(prog,var_gen,type_to_var_map) = build_predicate_w_var_args (name,image) (prog,VarGen.init (),IntMap.empty) in
      let i_rhs,length,(prog,var_gen,type_to_var_map) =
	List.fold_left
	  (fun (rhs,l_length,l_tables) (_,l_name,l_image) ->
	    let new_pred,new_tables=build_predicate_w_var_args (l_name,l_image) l_tables in
	    let l_length=l_length+1 in
	    (new_pred,l_length)::rhs,l_length,new_tables)
	  ([],0,(prog,var_gen,type_to_var_map))
	  tl
      in
      let e_rhs,_,(prog,_,_) =
	IntMap.fold
	  (fun _ (cst,cst_type) (rhs,l_length,l_tables) ->
	    let const_name=Sg.term_to_string cst obj_sig in
	    let () = assert (fst (Sg.is_constant const_name obj_sig)) in
	    let new_pred,new_tables = build_predicate_w_var_args (const_name,cst_type) l_tables in
	    let l_length=l_length+1 in
	    (new_pred,l_length)::rhs,l_length,new_tables)
	  env
	  ([],0,(prog,var_gen,type_to_var_map) ) in
      IFDEF BOLT THEN
      LOG "Correctly set the number of intensional predi in rhs: %d" (let () = assert (length=List.length i_rhs) in length) LEVEL DEBUG;
      END;
      let new_rule = AbstractSyntax.Rule.({id=rule_id;lhs;e_rhs;i_rhs;i_rhs_num=length}) in
      new_rule,Datalog.Program.add_rule ~intensional:true new_rule prog


  (* It makes the assumption that no constant has been
     previously defined or used in the program *)
  let edb_and_query ~obj_term ~obj_type ~obj_typing_env ~dist_type prog ~abs_sig ~obj_sig =
    let type_lst = map_types dist_type obj_type abs_sig obj_sig in
    match type_lst with
    | [] -> failwith "Bug: there should be a type correspondance"
    | [_,name,image] ->
      let e_facts,prog=
	IntMap.fold
	  (fun _ (cst,cst_type) (l_facts,l_prog) ->
	    let const_name=Sg.term_to_string cst obj_sig in
	    let () = assert (fst (Sg.is_constant const_name obj_sig)) in
	    let new_pred,l_prog = build_predicate_w_cst_args (const_name,cst_type) l_prog in
	    let rule_id,l_prog=Datalog.Program.get_fresh_rule_id l_prog in
	    let new_fact = AbstractSyntax.Rule.({id=rule_id;lhs=new_pred;e_rhs=[];i_rhs=[];i_rhs_num=0}) in
	    (new_fact::l_facts),l_prog)
	  obj_typing_env
	  ([],prog) in
      let prog=Datalog.Program.add_e_facts prog (e_facts,prog.Datalog.Program.const_table,prog.Datalog.Program.rule_id_gen) in
      build_predicate_w_cst_args (name,image) prog 
    | (_,name,image)::tl -> failwith "Bug: querying non atomic types is not yet implemented"
		
		
end
