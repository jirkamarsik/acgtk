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
      
    val predicate_to_string : predicate -> PredIdTable.table -> string

    val fact_compare : predicate -> predicate -> int 
    (*  val compare : pred_id -> pred_id -> int*)
      

  end

  module Rule:
  sig
    type proto_rule={proto_id:int;
		     proto_lhs:Predicate.predicate;
		     proto_rhs:Predicate.predicate list;
		    (** represents the predicates of the rule *)
		    }
    val proto_rule_to_string : proto_rule -> Predicate.PredIdTable.table -> string
      
    val print_proto_rules : Predicate.PredIdTable.table -> proto_rule list -> unit
      
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:Predicate.predicate list;
	     (** represents the extensionnal predicates of the rule *)
	       i_rhs:Predicate.predicate list; 
	    (** represents the intensionnal predicates of the rule *)
	      }
      
    module Rules : Set.S with type elt=rule
      
  end
    
  module Program : 
  sig 
    
    type program =  {rules:Rule.Rules.t;
		     pred_table: Predicate.PredIdTable.table;
		     i_preds:Predicate.PredIds.t}
      
    val make_program : Rule.proto_rule list -> Predicate.PredIdTable.table -> Predicate.PredIds.t -> program
      
    val print_program : program -> unit
      
  end
end


