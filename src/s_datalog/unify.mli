module VarGen :
sig
  type var
  type t
  val init : unit -> t
  val get_fresh_var : t -> (var*t)

  module VarMap : Map.S with type key=var
end

module Const :
sig
  type t
  val eq : t -> t -> bool
(* TODO: in case the type become more complex, unification, based on
   default equality and not [eq], should be revised *)
end

module type RulesAbstractSyntac_TYPE =
sig
  type content =  | Var of VarGen.var | Const of Const.t
  type predicate_id_type=string
  type predicate={p_id:predicate_id_type;
		  arity:int;
		  components:content list
		 (* It is assumed that the size of the list is the
		    arity *)
		 }      
  type rule={id:int;
	     lhs:predicate;
	     e_rhs:predicate list; (*represents the extensionnal predicates of the rule *)
	     i_rhs:predicate list; (*represents the intensionnal predicates of the rule *)
	    }
end
