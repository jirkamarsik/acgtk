open IdGenerator

module Var=
struct
  type t=Var of int
  let compare (Var i) (Var j)=i-j
  let succ (Var i)=Var (i+1)
  let start=Var 0
  let to_string (Var i) = 
    let c = Printf.sprintf "%c" (char_of_int (97+i)) in
    c
    
end
  
module VarGen = IdGen(Var)
  
module Const=
struct
  type t=Const of int
  let compare (Const i) (Const j)=i-j
  let start=Const 0
  let succ (Const i)=Const (i+1)
  let to_string (Const i) = string_of_int i
end

module ConstGen=IdGen(Const)

module AbstractSyntax =
struct
  (** These modules are the abstract syntactic representations of the
      predicates, of the rules, and of the programs *)
  module Predicate =
  struct
    type term = 
    | Var of VarGen.id
    | Const of ConstGen.id
	
    module VarMap = Map.Make (Var)
      
    let map_content_compare (k1,map1) (k2,map2) =
      try
	let val1 = VarMap.find k1 map1 in
	(try
	   val1-(VarMap.find k2 map2)
	 with
	 | Not_found -> 1)
      with
      | Not_found -> 
	(try
	   let _ = VarMap.find k2 map2 in
	   -1	    
	 with
	 | Not_found -> 0)
	  
	  
	  
    let term_compare l1 l2 =
      let rec term_compare_aux l1 l2 (l1_vars,l2_vars) pos =
	match l1,l2 with
	| [],[] -> 0
	| [],_ -> -1
	| _,[] -> 1
	| (Const _)::_,(Var _)::_ -> 1
	| (Var _)::_,(Const _)::_ -> -1
	| (Const a1)::tl1,(Const a2)::tl2 ->
	  let res = ConstGen.compare a1 a2 in
	  if ConstGen.compare a1 a2 <> 0 then
	    res
	  else
	    term_compare_aux tl1 tl2 (l1_vars,l2_vars) (pos+1)
	| (Var a1)::tl1,(Var a2)::tl2 ->
	  let res = map_content_compare (a1,l1_vars) (a2,l2_vars) in
	  if VarGen.compare a1 a2 <> 0 then
	    res
	  else
	    term_compare_aux tl1 tl2 (VarMap.add a1 pos l1_vars,VarMap.add a2 pos l2_vars) (pos+1) in
      term_compare_aux l1 l2 (VarMap.empty,VarMap.empty) 0
	
	
    let term_to_string t cst_id_table = 
      match t with
      | Var v -> Var.to_string v
      | Const c -> (* Const.to_string c *)
	ConstGen.Table.find_sym_from_id c cst_id_table
	  
    type pred_id=int
      
    module PredIdMap = IntIdGen.IdMap
    module PredIdTable = IntIdGen.Table
      
    type predicate={p_id:pred_id;
		    arity:int;
		    arguments: term list
		   (** It is assumed that the size of the list is the
		       arity *)
		   }      
      
    let to_string predicate (*{p_id=p_id;arguments=parameters}*) pred_id_table cst_id_table=
      Printf.sprintf "%s(%s)"
	(PredIdTable.find_sym_from_id predicate.p_id pred_id_table)
	(Utils.string_of_list "," (fun p -> term_to_string p cst_id_table) predicate.arguments)
	
    let compare ({p_id=id1;arity=a1;arguments=l1}:predicate) ({p_id=id2;arity=a2;arguments=l2}:predicate) =
      let res = compare id1 id2 in
      if res<>0 then
	res
      else
	let res = a1-a2 in
	if res<>0 then
	  res
	else
	  term_compare l1 l2
	    
    module PredIds=Utils.IntSet
      
  end
    
  module Proto_Rule =
  struct
    type t = {proto_id:int;
	      proto_lhs:Predicate.predicate;
	      proto_rhs:Predicate.predicate list;
	     (** represents the predicates of the rule *)
	     }
      
    let to_string r pred_id_table cst_id_table=
      let head=Predicate.to_string r.proto_lhs pred_id_table cst_id_table in
      let tail=
	match r.proto_rhs with
	| [] -> "."
	| _ -> 
	  Printf.sprintf
	    ":- %s."
	    (Utils.string_of_list "," (fun p -> Predicate.to_string p pred_id_table cst_id_table) r.proto_rhs) in
      Printf.sprintf "%s%s\n" head tail
	
	
    let to_buffer rules  pred_id_table cst_id_table = 
      let buff=Buffer.create 4 in
      let () =
	List.iter
	  (fun r -> Buffer.add_string
	    buff
	    (Printf.sprintf "%s\n" (to_string r pred_id_table cst_id_table)))
	  rules in
      buff
  end

  module Rule =
  struct
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:Predicate.predicate list;
	       i_rhs:Predicate.predicate list;
	      }
      
    let to_string r  pred_id_table cst_id_table =
      let head=Predicate.to_string r.lhs pred_id_table cst_id_table in
      let string_of_predicate_list lst = Utils.string_of_list "," (fun p -> Predicate.to_string p pred_id_table cst_id_table) lst in
      let vdash,e_i_sep =
	match r.e_rhs,r.i_rhs with
	| [],[] -> "",""
	| [],_ -> ":- "," "
	| _,[] -> ":- "," "
	| _,_ -> ":- "," , " in
      Printf.sprintf "%s%s%s%s." head vdash (string_of_predicate_list r.e_rhs) (string_of_predicate_list r.i_rhs)
	
    module Rules=Set.Make(struct
      type t=rule
      let compare {id=i} {id=j} = i-j
    end)

    let to_buffer rules pred_id_table cst_id_table = 
      let buff=Buffer.create 4 in
      let () =
	Rules.iter
	  (fun r -> 
	    let () =
	      Buffer.add_string
		buff
		(to_string r pred_id_table cst_id_table) in
	    Buffer.add_string buff "\n")
	  rules in
      buff

    let split_rhs proto_preds intensional_pred =
      List.fold_left
	(fun (i_preds,e_preds) ({Predicate.p_id=p_id} as pred) ->
	  if Predicate.PredIds.mem p_id intensional_pred 
	  then
	    (pred::i_preds,e_preds)
	  else
	    (i_preds,pred::e_preds))
	([],[])
	proto_preds
	
    let proto_rule_to_rule proto_rule intensional_pred =
      let i_preds,e_preds = 
	split_rhs proto_rule.Proto_Rule.proto_rhs intensional_pred in
      {id=proto_rule.Proto_Rule.proto_id;
       lhs=proto_rule.Proto_Rule.proto_lhs;
       e_rhs=List.rev e_preds;
       i_rhs=List.rev i_preds}	
  end

  module Proto_Program =
  struct
    type t =   {rules:Proto_Rule.t list;
		pred_table: Predicate.PredIdTable.table;
		const_table: ConstGen.Table.table;
		i_preds:Predicate.PredIds.t;
		rule_id_gen:IntIdGen.t}

    type tables = Predicate.PredIdTable.table*(VarGen.Table.table*ConstGen.Table.table)
      
    let empty = {rules=[];
		 pred_table=Predicate.PredIdTable.empty;
		 const_table=ConstGen.Table.empty;
		 i_preds=Predicate.PredIds.empty;
		 rule_id_gen=IntIdGen.init ()}

    let add_proto_rule (f_lhs,f_rhs) prog =
      let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id prog.rule_id_gen in
      let lhs,(new_pred_id_table,new_tables)=f_lhs (prog.pred_table,(VarGen.Table.empty,prog.const_table)) in
      let rhs,(new_pred_id_table',(_,new_const_table))=f_rhs (new_pred_id_table,new_tables) in
      let new_i_preds=
	match rhs with
	| [] -> prog.i_preds
	| _ -> Predicate.PredIds.add lhs.Predicate.p_id prog.i_preds in
      let new_rule =  {Proto_Rule.proto_id=rule_id;
		       Proto_Rule.proto_lhs=lhs;
		       Proto_Rule.proto_rhs=rhs} in
      {rules=new_rule::prog.rules;
       pred_table=new_pred_id_table';
       const_table=new_const_table;
       i_preds=new_i_preds;
       rule_id_gen=new_rule_id_gen;
      }
      

  end

  module Program =
  struct
    type program =  {rules:Rule.Rules.t;
		     pred_table: Predicate.PredIdTable.table;
		     const_table: ConstGen.Table.table;
		     i_preds:Predicate.PredIds.t;
		     rule_id_gen:IntIdGen.t;
		     e_pred_to_rules: Rule.Rules.t Predicate.PredIdMap.t}
      
    let make_program {Proto_Program.rules;Proto_Program.pred_table;Proto_Program.const_table;Proto_Program.i_preds;Proto_Program.rule_id_gen}=
      let actual_rules = 
	List.fold_left 
	  (fun acc p_rule -> 
	    Rule.Rules.add
	      (Rule.proto_rule_to_rule p_rule i_preds)
	      acc)
	  Rule.Rules.empty
	  rules in
      {rules=actual_rules;
       pred_table=pred_table;
       const_table=const_table;
       i_preds=i_preds;
       rule_id_gen=rule_id_gen;
       e_pred_to_rules=Predicate.PredIdMap.empty}
	
    let to_buffer prog (*{rules=rules;pred_table=pred_table;i_preds=i_preds}*) =
      let buff = Rule.to_buffer prog.rules prog.pred_table prog.const_table in
      let () = Buffer.add_string buff "Intentional predicates are:\n" in
      let () =
	Predicate.PredIds.iter
	  (fun elt -> Buffer.add_string buff (Printf.sprintf "\t%s\n%!" (Predicate.PredIdTable.find_sym_from_id elt prog.pred_table)))
	  prog.i_preds in
      buff
  end
    
end

