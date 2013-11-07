open IdGenerator
  
module VarGen:IdGen_TYPE
module ConstGen:IdGen_TYPE

(** These modules are the abstract syntactic representations of
    predicates and rules *)


module AbstractSyntax :
sig
  module Predicate :
  sig
    type term = 
    | Var of VarGen.id
    | Const of ConstGen.id
	
    type pred_id
    
    module PredIdMap:Map.S with type key=pred_id
    module PredIdTable:CorrespondanceTableTYPE with type identifier=pred_id
    module PredIds : Set.S with type elt=pred_id
      
    type predicate={p_id:pred_id;
		    arity:int;
		    arguments:term list 
		 (** It is assumed that the size of the list is the
		     arity *)
		   }      
      
    val to_string : predicate -> PredIdTable.table -> ConstGen.Table.table -> string
    val compare : predicate -> predicate -> int 
      

  end



  module Proto_Rule:
  sig
    type t={proto_id:int;
	    proto_lhs:Predicate.predicate;
	    proto_rhs:Predicate.predicate list;
	   (** represents the predicates of the rule *)
	   }
    val to_string : t -> Predicate.PredIdTable.table -> ConstGen.Table.table -> string

  end


  module Rule:
  sig

    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:Predicate.predicate list;
	     (** represents the extensionnal predicates of the rule *)
	       i_rhs:Predicate.predicate list; 
	    (** represents the intensionnal predicates of the rule *)
	      }
    val to_string : rule -> Predicate.PredIdTable.table -> ConstGen.Table.table -> string
    val proto_rule_to_rule : Proto_Rule.t -> Predicate.PredIds.t -> rule
      
    module Rules : Set.S with type elt=rule
  end

  module Proto_Program :
  sig
    type t=   {rules:Proto_Rule.t list;
		pred_table: Predicate.PredIdTable.table;
		const_table: ConstGen.Table.table;
		i_preds:Predicate.PredIds.t;
		rule_id_gen:IntIdGen.t}

    type tables = Predicate.PredIdTable.table*(VarGen.Table.table*ConstGen.Table.table)

    val empty : t

    val add_proto_rule : ((tables -> (Predicate.predicate*tables))*(tables -> ((Predicate.predicate list)*tables))) -> t -> t

  end
    
  module Program : 
  sig 
    
    type program =  {rules:Rule.Rules.t;
		     pred_table: Predicate.PredIdTable.table;
		     const_table: ConstGen.Table.table;
		     i_preds:Predicate.PredIds.t;
		     rule_id_gen:IntIdGen.t;
		     e_pred_to_rules: Rule.Rules.t Predicate.PredIdMap.t}
      
    val make_program : Proto_Program.t -> program
    val to_buffer : program -> Buffer.t
  end
end


