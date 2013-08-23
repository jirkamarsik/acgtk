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
	
    let rec term_compare l1 l2 =
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
	  term_compare tl1 tl2
      | (Var a1)::tl1,(Var a2)::tl2 ->
	let res = VarGen.compare a1 a2 in
	if VarGen.compare a1 a2 <> 0 then
	  res
	else
	  term_compare tl1 tl2
	    
	    
    let term_to_string = function
      | Var v -> Var.to_string v
      | Const c -> Const.to_string c
	
    type pred_id=int
      
    module PredIdMap = IntIdGen.IdMap
    module PredIdTable = IntIdGen.Table
      
    type predicate={p_id:pred_id;
		    arity:int;
		    arguments: term list
		 (** It is assumed that the size of the list is the
		     arity *)
		   }      
      
    let predicate_to_string {p_id=p_id;arguments=parameters} pred_id_table=
      Printf.sprintf "%s(%s)"
	(PredIdTable.find_sym_from_id p_id pred_id_table)
	(Utils.string_of_list "," term_to_string parameters)

    let fact_compare ({p_id=id1;arity=a1;arguments=l1}:predicate) ({p_id=id2;arity=a2;arguments=l2}:predicate) =
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
    
    
  module Rule =
  struct
    type proto_rule={proto_id:int;
		     proto_lhs:Predicate.predicate;
		     proto_rhs:Predicate.predicate list;
		    (** represents the predicates of the rule *)
		    }
      
    let proto_rule_to_string {proto_lhs=lhs;proto_rhs=rhs} pred_id_table =
      let head=Predicate.predicate_to_string lhs pred_id_table in
      let tail=
	match rhs with
	| [] -> "."
	| _ -> 
	  Printf.sprintf
	    ":- %s."
	    (Utils.string_of_list "," (fun p -> Predicate.predicate_to_string p pred_id_table) rhs) in
      Printf.sprintf "%s%s\n" head tail
	
	
    let print_proto_rules pred_id_table = 
      List.iter
	(fun r -> Printf.printf "%s\n" (proto_rule_to_string r pred_id_table))      
	
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:Predicate.predicate list;
	       i_rhs:Predicate.predicate list;
	      }
      
    let print_rule {lhs=lhs;e_rhs=e_rhs;i_rhs=i_rhs} pred_id_table =
      let head=Predicate.predicate_to_string lhs pred_id_table in
      let tail=
	match e_rhs,i_rhs with
	| [],[] -> "."
	| [],_ -> 
	  Printf.sprintf
	    ":-  %s."
	    (Utils.string_of_list "," (fun p -> Predicate.predicate_to_string p pred_id_table) i_rhs)
	| _,[] -> 
	  Printf.sprintf
	    ":- %s ."
	    (Utils.string_of_list "," (fun p -> Predicate.predicate_to_string p pred_id_table) e_rhs)
	| _,_ ->
	  Printf.sprintf
	    ":- %s , %s."
	    (Utils.string_of_list "," (fun p -> Predicate.predicate_to_string p pred_id_table) e_rhs)
	    (Utils.string_of_list "," (fun p -> Predicate.predicate_to_string p pred_id_table) i_rhs) in
      Printf.printf "%s%s\n" head tail
	
    module Rules=Set.Make(struct
      type t=rule
      let compare {id=i} {id=j} = i-j
    end)
  end

  module Program =
  struct
    type program =  {rules:Rule.Rules.t;
		     pred_table: Predicate.PredIdTable.table;
		     i_preds:Predicate.PredIds.t}
      
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
	
    let make_program proto_rules pred_id_table intensional_pred =
      let rules =
	List.fold_left
	  (fun acc p_rule ->
	    let i_preds,e_preds = 
	      split_rhs p_rule.Rule.proto_rhs intensional_pred in
	    Rule.Rules.add
	      {Rule.id=p_rule.Rule.proto_id;
	       lhs=p_rule.Rule.proto_lhs;
	       e_rhs=List.rev e_preds;
	       i_rhs=List.rev i_preds}
	      acc)
	  Rule.Rules.empty
	  proto_rules in
      {rules=rules;
       pred_table=pred_id_table;
       i_preds=intensional_pred}
	
    let print_program {rules=rules;pred_table=pred_table;i_preds=i_preds} =
      let () = Rule.Rules.iter (fun r -> Rule.print_rule r pred_table) rules in
      let () = Printf.printf "Intentional predicates are:\n" in
      Predicate.PredIds.iter (fun elt -> Printf.printf "\t%s\n%!" (Predicate.PredIdTable.find_sym_from_id elt pred_table)) i_preds
  end
    
end

