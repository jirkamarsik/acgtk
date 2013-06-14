open PersistentArray
open Focused_list

module VarGen =
struct
  type var = Var of int
  type t = Generator of int
  let init () = Generator 0
  let get_fresh_var (Generator n) = Var n, Generator (n+1)

  module VarMap=Map.Make(struct type t=var let compare (Var i) (Var j)=i-j end)
end

module Const =
struct
  type t = int
  let eq i j = (i=j)
(* TODO: in case the type become more complex, unification, based on
   default equality, should be revised *)
end

module type RulesAbstractSyntac_TYPE =
sig
  type _ content = 
  | Var : VarGen.var -> VarGen.var content
  | Const : Const.t -> Const.t content
  type predicate_id_type=string
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
  type  fact = Const.t predicate
  type  ground_clause = Const.t rule
    
    
  val compare : predicate_id_type -> predicate_id_type -> int
end

(** These modules are the abstract syntactic representations of
    predicates and rules *)
module Pred_and_Rules =
struct
  type _ content = 
  | Var : VarGen.var -> VarGen.var content
  | Const : Const.t -> Const.t content
  type predicate_id_type=string
  type 'a predicate={p_id:predicate_id_type;
		     arity:int;
		     components:'a content list
		    (** It is assumed that the size of the list is the
			arity *)
		    }      
  type 'a rule={r_id:int;
		lhs:'a predicate;
		e_rhs:'a predicate list;
		i_rhs:'a predicate list;
	       }

  type  fact = Const.t predicate
  type  ground_clause = Const.t rule
  let compare id1 id2 = String.compare id1 id2
end


  
module Unify (S:UnionFind.Store)(P:RulesAbstractSyntac_TYPE) =
struct
  exception Fails
  module UF= UnionFind.Make(S)

(*  type predicate_id_type=string *)
  type predicate={p_id:P.predicate_id_type;    (* TODO: Should be an int for efficiency*)
		  arity:int;
		 }

  (** A map whose key is of type [predicate_id_type] *)
  module PredMap=Map.Make
    (struct
      type t=P.predicate_id_type
      let compare=P.compare 
     end)
    
  (** A map indexed by integers to store facts at step (or time) [i]
      in the seminaive algorithm. These facts are also indexed by
      [predicate_id_type]. *)
  module Indexed_Facts=Utils.IntMap 
    
  (** In an [internal_rule], all the compoments of all the predicates
      are stored in a {! UnionFind} indexed data structure. We assume
      here that from [1] to [lhs.arity] the components of the left
      hand side predicate are stored, then from [lhs.arity+1] to
      [lhs.arity+(hd rhs).arity] the components of the first predicate
      on the right hand side are stored, etc. It is assumed that this
      structure is correct (no cycle, links within the range, etc.) *)
  type rule={id:int;
	     lhs:predicate;
	     e_rhs:predicate list;
	     i_rhs:predicate list;
	     content:Const.t UF.t
	    (* TODO: Maybe put the label of the predicate in the
	       content in order to enforce checking of the current
	       instantiation *)
	    }

  let make_predicate p = {p_id=p.P.p_id;arity=p.P.arity}

  (** [add_pred_components_to_content components
      (content,idx,mapped_vars)] returns a triple
      (content',idx',mapped_vars') where [content'] is the list
      [content] to which has been added {e *in the reverse order*} the
      information from [components]. The update is such that if the
      component is a [Var i] then it is replaced by a [Link_to j] such
      that [j] is the index at which the variable [Var i] was met for
      the first time. If the component is a [Const c], the a [Value c]
      is added at the current position. [idx'] is the value of the
      next position if another list of component has to be added. And
      [mapped_vars'] is a map from variables [Var i] to positions
      (i.e. [int]) in which these variables first occur in [content']

      - [components] is the list of components of some predicate
      
      - [content] is a list meant to become the content of a rule,
      i.e. an indexed storage data structure that is meant to be
      extended with [components]. *BE CAREFUL: IT COMES IN INVERSE
      ORDER*
      
      - [idx] is the index to be given for the next element of
      [content]
      
      - [mapped_vars] is a mapping from [P.Var i] variables to the
      index at which they've been stored in [content]. When such a
      variable is met for the first time, as expected in the
      UnionFind data structure, the content at [idx] is [Link_to]'ed
      itself. *)
  let add_pred_components_to_content components (content,idx,mapped_vars) =
    List.fold_left
      (fun (type t) (cont,i,vars) (content : t P.content) -> 
	match content with
	| P.Var v -> 
	  begin
	    try ((UF.Link_to(VarGen.VarMap.find v vars)) :: cont,idx+1,vars) with
	    | Not_found -> 
	      ((UF.Link_to idx) :: cont,idx+1,VarGen.VarMap.add v idx vars)
	  end
	| P.Const c -> ((UF.Value c) :: cont,idx+1,vars))
      (content,idx,mapped_vars)
      components
      
  (** [make_rule r] returns an internal rule, that is one whose
      content is now a {! UnionFind.UnionFind} indexed data
      structure *)
      
  let make_rule {P.id=id;P.lhs=lhs;P.e_rhs=e_rhs;P.i_rhs=i_rhs} =
    (* Be careful, the list of the rhs is reversed *)
    let lhs_content=
      add_pred_components_to_content lhs.P.components ([],1,VarGen.VarMap.empty) in
    let e_rhs,e_rhs_content =
      List.fold_left
	(fun (rhs,content) {P.p_id=n;P.arity=k;P.components=pred_comps} ->
	  ({p_id=n;arity=k} :: rhs,
	   add_pred_components_to_content pred_comps content))
	([],lhs_content)
	e_rhs in
    let i_rhs,(content,_,_) =
      List.fold_left
	(fun (rhs,content) {P.p_id=n;P.arity=k;P.components=pred_comps} ->
	  ({p_id=n;arity=k} :: rhs,
	   add_pred_components_to_content pred_comps content))
	([],e_rhs_content)
	i_rhs in
    {id=id;
     lhs=make_predicate lhs;
     e_rhs=List.rev e_rhs;
     i_rhs=List.rev i_rhs;
     content=UF.create (List.rev content)
    }
      
  (* the [dag] parameter is meant to be the components of the
     predicates *)
  let cyclic_unify i j h =
    match UF.cyclic i h with
    | true,_ -> raise Fails
    | _, h' -> 
      (try UF.union i j h with
      | UF.Union_Failure -> raise Fails)
	
  (** [instantiate_with p (i,c)] instantiates the content [c] with the
      fact [p] starting at [i]. It returns a pair [(i',c')] when [i]
      is the index of the first component of the [p] predicate in the
      content [c] {e THIS IS NOT CHECKED HERE}. [i'=i+a] where [a] is
      the arity of [p] (it means [i'] should index the first component
      of the next predicate in the content of the rule) and [c'] is a
      new content where all the components between [i] and [i'-1] have
      been instantiated with the components of [p]. When such an
      instantiation fails, it raises {! UF.Union_Failure} *)
  let instantiate_with
      {P.p_id=_;P.arity=_;P.components=components}
      (idx,content) =
    List.fold_left
      (fun  (i,cont) value ->
	(i+1,
	 match value with
	 | P.Const v -> UF.instantiate i v cont))
      (idx,content)
      components
      
  (** [extract_consequence_from_rule r content] returns a fact whose
      components all are of the form [Const c] (that is something of
      type [P.rule]). *)
  let extract_consequence_from_rule r content =
    {P.p_id=r.lhs.p_id;
     P.arity=r.lhs.arity;
     P.components=
	List.map 
	  (function
	  | UF.Value v -> P.Const v
	  | _ -> failwith "Bug: you're trying to extract non completely instantiated predicates")
	  (UF.extract (r.lhs.arity) content)}


  (** [FactArray] is a module implementing a traversal of facts using
      the {! ArrayTraversal.Make} functor. The [update] function is
      such that we don't consider cells (i.e. facts) that don't unify
      with the rule (i.e. a {! UF.Union_Failure} exception was
      raised).*)
  module FactArray=ArrayTraversal.Make(
    struct
      type state = int*(Const.t UF.t)
      type cell = P.fact
      let update s c =
	try 
	  Some (instantiate_with c s)
	with
	| UF.Union_Failure -> None
    end
  )
    
  (** [immediate_consequence_of_rule r db] returns a list of facts
      generated by the rule [r] using the facts stored in [db]. {e
      *these facts are not added to [db] when collecting the new
      facts*}.

      Note that it is important that resulting states need to be
      processed otherwise they will be lost in backtracking when using
      {! PersistentArray}.*)
  let rec immediate_consequence_of_rule r db =
    (* We collect all the contents compatible with the facts of the
       database corresponding to intensional predicates *)
    let make_search_array_i_pred =
      List.map (fun pred -> PredMap.find pred.p_id db) r.i_rhs in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
    let resume_on_i_pred acc state =
      FactArray.collect_results
	(fun l_acc (_,content) -> (extract_consequence_from_rule r content)::l_acc)
	acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the facts of
       the extensional database (facts of the database corresponding
       to extensional predicates). *)
    let make_search_array_e_pred =
      List.map (fun pred -> PredMap.find pred.p_id db) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      []
      (r.lhs.arity+1,r.content)
      make_search_array_e_pred
      
  (** [temp r time (rev_pred_lst,delta_position,pred_lst) e_facts
      time_indexed_intensional_facts
      time_indexed_delta_intensional_facts] returns a list ot new
      facts that are deduced from [temp]{^ [time+1]}{_ [S]} where [S] is
      the head predicate of the rule [r], [delta_position] the index
      of the intensional predicate that is under focus as described in
      {{: http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13
      of "Foundations of Databases", Abiteboul, Hull, and Vianu}
      (p.315), [rev_pred_list] (resp.  [pred_lst]) the lists of
      predicates in the rule that precede [delta_position]
      (resp. [follow]) *)
  let temp r time (rev_pred_lst,delta_position,pred_lst) e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts agg_function start =
    (* We collect all the contents compatible with the facts of the
       intensional database *)
    let make_search_array_i_pred =
      let delta_facts=
	PredMap.find delta_position.p_id (Indexed_Facts.find time time_indexed_delta_intensional_facts) in
      let end_pred_facts =
	List.map
	  (fun pred -> PredMap.find pred.p_id (Indexed_Facts.find (time-1) time_indexed_intensional_facts))
	  pred_lst in
      List.fold_left
	(fun acc pred ->
	  (PredMap.find pred.p_id (Indexed_Facts.find time time_indexed_intensional_facts))::acc)
	(delta_facts::end_pred_facts)
	rev_pred_lst in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
    let resume_on_i_pred acc state =
      FactArray.collect_results
	(fun l_acc (_,content) -> agg_function (extract_consequence_from_rule r content) l_acc)
	acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the
       facts of the extensional database *)
    let make_search_array_e_pred =
      List.map (fun pred -> PredMap.find pred.p_id e_facts) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      start
      (r.lhs.arity+1,r.content)
      make_search_array_e_pred


      
  type program = {rules:rule list PredMap.t;
		  (* the list of the rules of the program indexed by
		     the id of this predicate *)
		  edb:predicate list;
		  (* the list of the extensional predicates *)
		  idb:predicate list;
		  (* the list of the intensional predicates *)
		 }

    
  let all_temp_results_for_predicate s time {rules=rules} e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts =
    List.fold_left
      (fun acc r ->
	let zip=Focused_list.init r.i_rhs in
	Focused_list.fold
	  (fun l_acc focus -> temp r time focus e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts  (fun hd tl -> hd::tl) l_acc)
	  acc
	  zip)
      []
      (PredMap.find s.p_id rules)
      
  (** [temp_facts r time e_facts time_indexed_intensional_facts
      time_indexed_delta_intensional_facts agg_f start] returns the
      result of applying [agg_f] to [start] and to all the facts that
      are deduced from [temp]{^ [time+1]}{_ [S]} where [S] is the head
      predicate of the rule [r] and [temp] is the set of temporary
      rules associated with [r] as in the algorithm described in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu}
      (p.315).

      [time_indexed_delta_intensional_facts] and
      [time_indexed_intensional_facts] are the databases from which
      the facts for these deductions are taken, Hence they are indexed
      by time (or step). *)

  let temp_facts r time e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts agg_function start =
    (* We firs collect all the contents compatible with the facts of
       the intensional database. They depend on the intensional
       predicate [delta_position] and the ones that are before it
       ([rev_pred_lst]) and the ones that are after it
       ([pred_lst]). This triple correspond to a {!Focused_list.t}
       type. *)
    let make_search_array_i_pred (rev_pred_lst,delta_position,pred_lst) =
      let delta_facts=
	PredMap.find delta_position.p_id (Indexed_Facts.find time time_indexed_delta_intensional_facts) in
      let end_pred_facts =
	List.map
	  (fun pred -> PredMap.find pred.p_id (Indexed_Facts.find (time-1) time_indexed_intensional_facts))
	  pred_lst in
      List.fold_left
	(fun acc pred ->
	  (PredMap.find pred.p_id (Indexed_Facts.find time time_indexed_intensional_facts))::acc)
	(delta_facts::end_pred_facts)
	rev_pred_lst in
    (* We now init the focused list corresponding to the intensional
       predicates of the rule [r] *)
    let zip=Focused_list.init r.i_rhs in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates. This
       function will run a result collection (with
       [FactArray.collect_results]) for each of the possible
       [delta_facts], that is for each of the possible [Focused_list]
       that can be reach from [zip] (including [zip] itself). *)
    let resume_on_i_pred acc state =
      Focused_list.fold
	(fun l_acc focus ->
	  (* For a given focus in the intensional list of predicates
	     of [r], we extract all the possible facts from the rule
	     [r] *)
	  FactArray.collect_results
	    (fun ll_acc (_,content) -> agg_function (extract_consequence_from_rule r content) ll_acc)
	    l_acc
	    state
	    (make_search_array_i_pred focus))
	acc
	zip in
    (* We now collect all the contents compatible with the
       facts of the extensional database *)
    let make_search_array_e_pred =
      List.map (fun pred -> PredMap.find pred.p_id e_facts) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> 
	(* For each partial completion of the rule on the extensional
	   database, we need to take into account the remaining
	   intensional predicates. *)
	resume_on_i_pred acc s)
      start
      (r.lhs.arity+1,r.content)
      make_search_array_e_pred

  (** [p_semantics_for_predicate s time prog e_facts
      time_indexed_intensional_facts
      time_indexed_delta_intensional_facts] returns a list of all the
      facts that can deduced by all the rules in [prog] at time [time]
      whose lhs predicate is [s] when the edb is [e_facts], the time
      indexed facts are [time_indexed_intensional_facts] and the time
      indexed variation of facts are
      [time_indexed_delta_intensional_facts].

      It corresponds to [P]{^ [time]}{_ [S]} [(edb,T]{^ [time -1]}{_
      [1]}[,...,T]{^ [time-1]}{_ [l]}[,T]{^ [time]}{_ [1]}[,...,T]{^
      [time]}{_ [l]}[, Delta]{^ [time]}{_ [T]{_ [1]}},...,[Delta]{^
      [time]}{_ [T]{_ [l]}}) in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu} *)
  let p_semantics_for_predicate s time {rules=rules} e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts =
    List.fold_left
      (fun acc r ->
	temp_facts r time  e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts  (fun hd tl -> hd::tl) acc)
      []
      (PredMap.find s.p_id rules)

      
(*
  let seminaive prog =
    let seminaive_aux facts delta_facts =
      if PredMap.is_empty indexed_delta_facts then
	facts
      else
	(* TODO: Check that PredMap has all intentionsal predicates of
	   prog *)
	PredMap.fold
	  (fun pred fact_list acc ->
	    PredSet.
	    p_semantics_for_predicate pred.lhs prog e_facts facts delta_facts
	  )
	  facts
	  PredMap.empty
	  (PredMap.merge
	     (fun pred_id v1 v2 ->
	       match v1,v2 with
	       | Some l1,Some l2 -> Some (List.rev_append l1 l2)
	       | _ -> failwith "Bug: this should not happen")
	     facts
	     delta_facts)
*)	  
	  
	  


end
  
  
