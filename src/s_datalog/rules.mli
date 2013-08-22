module type CorrespondanceTableTYPE=
sig
  type identifier
  type table
  exception Not_found
  val empty:table
  val find_id_of_sym : string -> table -> identifier
  val find_sym_from_id : identifier -> table -> string
  val add_sym : string -> table -> identifier*table
end

module type IdGen_TYPE =
sig
  type id
  type t
  val init : unit -> t
  val get_fresh_id : t -> (id*t)
  val eq : id -> id -> bool
  val compare : id -> id -> int
    
  module IdMap : Map.S with type key=id
  module Corr : CorrespondanceTableTYPE with type identifier=id
end
  
module VarGen:IdGen_TYPE
module ConstGen:IdGen_TYPE

module type RulesAbstractSyntac_TYPE =
sig
  type _ content = 
  | Var : VarGen.id -> VarGen.id content
  | Const : ConstGen.id -> ConstGen.id content
  type predicate_id_type = string
  type 'a predicate={p_id:predicate_id_type;
		     arity:int;
		     components:'a content list 
		    (** It is assumed that the size of the list is the
			arity *)
		    }      
  type 'a rule={id:int;
		lhs:'a predicate;
		e_rhs:'a predicate list;
		(** represents the extensionnal predicates of the rule *)
		i_rhs:'a predicate list; 
	       (** represents the intensionnal predicates of the rule *)
	       }
  type  fact = ConstGen.id predicate
  type  ground_clause = ConstGen.id rule
    

  val fact_compare : fact -> fact -> int
  val compare : predicate_id_type -> predicate_id_type -> int


end

(** These modules are the abstract syntactic representations of
    predicates and rules *)
module Pred_and_Rules : RulesAbstractSyntac_TYPE
