module Pred:module type of Datalog_AbstractSyntax.AbstractSyntax.Predicate
module Rule:module type of Datalog_AbstractSyntax.AbstractSyntax.Rule
module Prog:module type of Datalog_AbstractSyntax.AbstractSyntax.Program

module Unify :
  functor (S : UnionFind.Store) ->
    sig
      exception Fails
      module UF:UnionFind.S
      module Predicate :
        sig
          type predicate = { p_id : Pred.pred_id; arity : int; }
          val make_predicate : Pred.predicate -> predicate
	  (*          val instantiate_with :
		      Pred.predicate ->
		      int * Datalog_AbstractSyntax.ConstGen.id UF.t ->
		      int * Datalog_AbstractSyntax.ConstGen.id UF.t *)
          module PredMap : Map.S with type key = Pred.pred_id
          module FactSet :Set.S with type elt = Pred.predicate
          val conditionnal_add :
            FactSet.elt -> FactSet.t -> FactSet.t -> FactSet.t -> FactSet.t
          module Indexed_Facts : Map.S with type key = int
        end
      module Rule :
      sig
        type rule = {
          id : int;
          lhs : Predicate.predicate;
          e_rhs : Predicate.predicate list;
          i_rhs : Predicate.predicate list;
          content : Datalog_AbstractSyntax.ConstGen.id UF.t;
        }
        val add_pred_arguments_to_content :
          Pred.term list ->
          Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
            int Datalog_AbstractSyntax.VarGen.IdMap.t ->
          Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
            int Datalog_AbstractSyntax.VarGen.IdMap.t
        val make_rule : Rule.rule -> rule
        val cyclic_unify : int -> int -> 'a UF.t -> 'a UF.t
        val extract_consequence :
          rule -> Datalog_AbstractSyntax.ConstGen.id UF.t -> Pred.predicate
        module FactArray :
        sig
          type row = Predicate.FactSet.t
          type array = row list
          val collect_results :
            ('a -> int * Datalog_AbstractSyntax.ConstGen.id UF.t -> 'a) ->
            'a ->
            int * Datalog_AbstractSyntax.ConstGen.id UF.t -> array -> 'a
        end
        val immediate_consequence_of_rule :
          rule -> FactArray.row Predicate.PredMap.t -> Pred.predicate list
        val temp :
          rule ->
          Predicate.Indexed_Facts.key ->
          Predicate.predicate list * Predicate.predicate *
            Predicate.predicate list ->
            FactArray.row Predicate.PredMap.t ->
            FactArray.row Predicate.PredMap.t Predicate.Indexed_Facts.t ->
            FactArray.row Predicate.PredMap.t Predicate.Indexed_Facts.t ->
            (Pred.predicate -> 'a -> 'a) -> 'a -> 'a
        end
      type program = {
        rules : Rule.rule list Predicate.PredMap.t;
        edb : Predicate.predicate list;
        idb : Predicate.predicate list;
      }
      val all_temp_results_for_predicate :
        Predicate.predicate ->
        Predicate.Indexed_Facts.key ->
        program ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t Predicate.Indexed_Facts.t ->
        Rule.FactArray.row Predicate.PredMap.t Predicate.Indexed_Facts.t ->
        Pred.predicate list
      val temp_facts :
        Rule.rule ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t ->
        (Pred.predicate -> 'a -> 'a) -> 'a -> 'a
      val p_semantics_for_predicate :
        Predicate.PredMap.key ->
        program ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t ->
        Rule.FactArray.row Predicate.PredMap.t -> Predicate.FactSet.t
      val seminaive : program -> unit
    end
