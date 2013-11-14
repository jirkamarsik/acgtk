module ASPred:module type of Datalog_AbstractSyntax.AbstractSyntax.Predicate with type pred_id=Datalog_AbstractSyntax.AbstractSyntax.Predicate.pred_id and type PredIdTable.table = Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table
module ASRule:module type of Datalog_AbstractSyntax.AbstractSyntax.Rule with type rule=Datalog_AbstractSyntax.AbstractSyntax.Rule.rule
module ASProg:module type of Datalog_AbstractSyntax.AbstractSyntax.Program with type program = Datalog_AbstractSyntax.AbstractSyntax.Program.program

module Make :
  functor (S : UnionFind.Store) ->
    sig
      exception Fails
      module UF:UnionFind.S

      module Predicate :
        sig
          type predicate = { p_id : ASPred.pred_id; arity : int; }
          val make_predicate : ASPred.predicate -> predicate
	  (*          val instantiate_with :
		      ASPred.predicate ->
		      int * Datalog_AbstractSyntax.ConstGen.id UF.t ->
		      int * Datalog_AbstractSyntax.ConstGen.id UF.t *)
          module PredMap : Map.S with type key = ASPred.pred_id
          module FactSet :Set.S with type elt = ASPred.predicate
          val conditionnal_add :
            FactSet.elt -> FactSet.t -> FactSet.t -> FactSet.t -> FactSet.t
	  val facts_to_string : FactSet.t PredMap.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
(*          module Indexed_Facts : Map.S with type key = int *)
	  module PredicateMap : Map.S with type key = ASPred.predicate
	  module Premise :
	    sig
	      type t = ASPred.predicate list
	      val to_string : t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
	    end
	  module PremiseSet : Set.S with type elt = Premise.t
	  val add_map_to_premises_to_buffer : Buffer.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit
	  val format_derivations2 : ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit
        end

      module Rule :
      sig
        type rule = {
          id : int;
          lhs : Predicate.predicate;
          e_rhs : (Predicate.predicate*int) list;
          i_rhs : (Predicate.predicate*int) list;
          content : Datalog_AbstractSyntax.ConstGen.id UF.t;
        }
        val add_pred_arguments_to_content :
          ASPred.term list ->
          Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
            int Datalog_AbstractSyntax.VarGen.IdMap.t ->
          Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
            int Datalog_AbstractSyntax.VarGen.IdMap.t
        val make_rule : ASRule.rule -> rule
        val cyclic_unify : int -> int -> 'a UF.t -> 'a UF.t
        val extract_consequence :
          rule -> Datalog_AbstractSyntax.ConstGen.id UF.t -> ASPred.predicate
        module FactArray :
        sig
          type row = Predicate.FactSet.t
          type array = row list
          val collect_results :
            ('a -> (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> 'a) ->
            'a ->
            (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> array -> 'a
        end
        val immediate_consequence_of_rule :
          rule -> FactArray.row Predicate.PredMap.t -> ASPred.predicate list

	module Rules:Set.S with type elt=rule
      end

      module Program :
      sig
	type program = {
          rules : Rule.rule list Predicate.PredMap.t;
          edb : ASPred.pred_id list;
	  edb_facts:Predicate.FactSet.t Predicate.PredMap.t;
          idb : ASPred.pred_id list;
	  pred_table: ASPred.PredIdTable.table;
	  const_table: Datalog_AbstractSyntax.ConstGen.Table.table;
	  rule_id_gen:IdGenerator.IntIdGen.t;
	  (* e_pred_to_rules: Rule.Rules.t Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdMap.t *)
	}
        val make_program : ASProg.program -> program
	val temp_facts :
          Rule.rule ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t ->
          (ASPred.predicate * Predicate.FactSet.elt list -> 'a -> 'a) -> 'a -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> 'a
	val p_semantics_for_predicate :
          Predicate.PredMap.key ->
          program ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t ->
          Rule.FactArray.row Predicate.PredMap.t -> Predicate.PremiseSet.t Predicate.PredicateMap.t -> Predicate.FactSet.t * Predicate.PremiseSet.t Predicate.PredicateMap.t 
	val seminaive : program -> Rule.FactArray.row Predicate.PredMap.t * Predicate.PremiseSet.t Predicate.PredicateMap.t
	val to_abstract : program -> ASProg.program

	val extend : program -> ASProg.modifier -> program

	val add_e_facts : program -> (ASRule.rule list*Datalog_AbstractSyntax.ConstGen.Table.table*IdGenerator.IntIdGen.t) -> program
      end
    end
