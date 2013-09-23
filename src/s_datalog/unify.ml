open PersistentArray
open Focused_list
open Datalog_AbstractSyntax


module ASPred=AbstractSyntax.Predicate
module ASRule=AbstractSyntax.Rule
module ASProg=AbstractSyntax.Program
  
module Unify (S:UnionFind.Store) =
struct
  exception Fails
  module UF= UnionFind.Make(S)

  module Predicate =
  struct
    
    (** For the type of the predicates, we use the same identifiers as
	for the predicates of the datalog abstract syntax {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate} *)
    type predicate={p_id:ASPred.pred_id;
		    arity:int;
		   }
      
    (** [make_predicate p] returns an actual predicate from some
	abstract syntax representation {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate} *)
    let make_predicate p = {p_id=p.ASPred.p_id;arity=p.ASPred.arity}
      
    (** [to_abstract p (s,content) (vars,vargen)] returns a triple
	[(abs_p,vars',vargen')] where [abs_p] is the [p] predicate
	translated into an equivalent predicate from the datalog
	abstract syntax. In order to be able to perform this
	translation, we need [s] and index and [content] a indexed
	storage data structure which is meant to contain the arguments
	of [p] starting at index [s]. Then, in case some variable are
	still present, to be able to translate them according to the
	other variables that could be in the content [content], we
	need to check in [vars] if it's index already was associated
	to some [VarGen.id] generated by [vargen]. In this case
	[vars'=vars] and [vargen'=vargen], otherwise [vars'] is [var]
	with a new variable generated by [vargen] associated to the
	variable index, and [vargen'] is the result of generating this
	new variable from [vargen].*)
    let to_abstract {p_id=id;arity=arity} (start,content) (vars,vargen) =
      let get_var i (vars,vargen) = 
	try
	  Utils.IntMap.find i vars,(vars,vargen)
	with
	| Not_found -> 
	  let new_var,new_vargen = VarGen.get_fresh_id vargen in
	  new_var,(Utils.IntMap.add i new_var vars,new_vargen) in
      let new_vars,new_vargen,rev_arguments=
	List.fold_left
	  (fun (vars,vargen,acc) -> function
	  | UF.Value v -> vars,vargen,((ASPred.Const v)::acc)
	  | UF.Link_to i -> let var,(new_vars,new_vargen)=get_var i (vars,vargen) in
			    new_vars,new_vargen,(ASPred.Var var)::acc)
	  (vars,vargen,[])
	  (UF.extract ~start:start arity content) in      
      {ASPred.p_id=id;
       ASPred.arity=arity;
       arguments=List.rev rev_arguments},
      new_vars,
      new_vargen
	
    (** [lst_to_abstract lst (start,content) (vars,vargen)] returns a
	4-uple [(abs_p_lst,start',vars',vargen')] where all the
	predicates of [lst] have been translated and put into
	[abs_p_lst]. The predicates in [lst] are supposed to be
	represendet in [content] starting at index [start] in an
	adjacent way. [start'] indexes the component of the next
	predicate in [content], and [vars'] and [vargen'] keep track
	of the variable that can have been generated. *)
    let lst_to_abstract lst (start,content) (vars,vargen) =
      let next_idx,vars',vargen',abs_preds =
	List.fold_left
	  (fun (s,l_vars,l_vargen,acc) p ->
	    let abs_p,new_vars,new_vargen = to_abstract p (s,content) (l_vars,l_vargen) in
	    s+p.arity+1,new_vars,new_vargen,abs_p::acc)
	  (start,vars,vargen,[])
	  lst in
      (List.rev abs_preds),next_idx,vars,vargen'
	
	
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
	{ASPred.p_id=_;ASPred.arity=_;ASPred.arguments=args}
	(idx,content) =
      List.fold_left
	(fun  (i,cont) value ->
	  (i+1,
	   match value with
	   | ASPred.Const v -> UF.instantiate i v cont
	   | ASPred.Var _ -> failwith "Bug: Trying to instantiate with a non-closed predicate"))
	(idx,content)
	args
	
	
	
    (** A map whose key is of type of the predicates identifers *)
    module PredMap=ASPred.PredIdMap
      
    (* TODO : This should be a map recording the way each of this
       predicate was derived *)
      
    (** A map whose key is of type [predicate] *)
    (* TODO: Could it be replaced by predicate id only? *)
    module FactSet=Set.Make
      (struct
	type t=ASPred.predicate
	let compare = ASPred.compare 
       end)
      
    (** [conditionnal_add e s1 s2 s3] adds [e] to the set [s1] only if
	[e] doesn't belong to [s2] nor to [s3]*)
    let conditionnal_add e s1 s2 s3=
      if FactSet.mem e s2 then
	s1
      else
	if FactSet.mem e s3 then
	  s1
	else
	  FactSet.add e s1
	    
    (** A map indexed by integers to store facts at step (or time) [i]
	in the seminaive algorithm. These facts are also indexed by
	[predicate_id_type]. *)
    module Indexed_Facts=Utils.IntMap 
  end
    
  module Rule =
  struct
    
    (** In a [rule], all the compoments of all the predicates
	are stored in a {! UnionFind} indexed data structure. We assume
	here that from [1] to [lhs.arity] the components of the left
	hand side predicate are stored, then from [lhs.arity+1] to
	[lhs.arity+(hd rhs).arity] the components of the first predicate
	on the right hand side are stored, etc. It is assumed that this
	structure is correct (no cycle, links within the range, etc.) *)
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:Predicate.predicate list;
	       i_rhs:Predicate.predicate list;
	       content:ConstGen.id UF.t
	      (* TODO: Maybe put the label of the predicate in the
		 content in order to enforce checking of the current
		 instantiation *)
	      }
      
      
  (** [add_pred_arg_to_content arguments (content,idx,mapped_vars)]
      returns a triple (content',idx',mapped_vars') where [content']
      is the list [content] to which has been added {e *in the reverse
      order*} the information from [arguments]. The update is such
      that if the argument of [arguments] is a [Var i] then it is
      replaced by a [Link_to j] such that [j] is the index at which
      the variable [Var i] was met for the first time (it is stored in
      [mapped_vars]. If the argument is a [Const c], then a [Value c]
      is added at the current position. [idx'] is the value of the
      next position if another list of arguments has to be added. And
      [mapped_vars'] is a map from variables [Var i] to positions
      (i.e. [int]) in which these variables first occur in [content']
      - [arguments] is the list of the arguments of some predicate
      - [content] is a list meant to become the content of a rule,
      i.e. an indexed storage data structure that is meant to be
      extended with [arguments]. *BE CAREFUL: IT COMES IN INVERSE
      ORDER*
      - [idx] is the index to be given for the next element of
      [content]
      - [mapped_vars] is a mapping from [P.Var i] variables to the
      index at which they've been stored in [content]. When such a
      variable is met for the first time, as expected in the
      UnionFind data structure, the content at [idx] is [Link_to]'ed
      itself. *)
    let add_pred_arguments_to_content arguments (content,idx,mapped_vars) =
      List.fold_left
	(fun (cont,i,vars) (arg : ASPred.term) -> 
	  match arg with
	  | ASPred.Var v -> 
	    begin
	      try ((UF.Link_to(VarGen.IdMap.find v vars)) :: cont,idx+1,vars) with
	      | Not_found -> 
		((UF.Link_to idx) :: cont,idx+1,VarGen.IdMap.add v idx vars)
	    end
	  | ASPred.Const c -> ((UF.Value c) :: cont,idx+1,vars))
	(content,idx,mapped_vars)
	arguments
	
  (** [make_rule r] returns an internal rule, that is one whose
      content is now a {! UnionFind.UnionFind} indexed data
      structure *)
	
    let make_rule {ASRule.id=id;ASRule.lhs=lhs;ASRule.e_rhs=e_rhs;ASRule.i_rhs=i_rhs} =
    (* Be careful, the list of the rhs is reversed *)
      let lhs_content=
	add_pred_arguments_to_content lhs.ASPred.arguments ([],1,VarGen.IdMap.empty) in
      let e_rhs,e_rhs_content =
	List.fold_left
	  (fun (rhs,content) {ASPred.p_id=n;ASPred.arity=k;ASPred.arguments=pred_args} ->
	    ({Predicate.p_id=n;Predicate.arity=k} :: rhs,
	     add_pred_arguments_to_content pred_args content))
	  ([],lhs_content)
	  e_rhs in
      let i_rhs,(content,_,_) =
	List.fold_left
	  (fun (rhs,content) {ASPred.p_id=n;ASPred.arity=k;ASPred.arguments=pred_args} ->
	    ({Predicate.p_id=n;Predicate.arity=k} :: rhs,
	     add_pred_arguments_to_content pred_args content))
	  ([],e_rhs_content)
	  i_rhs in
      {id=id;
       lhs=Predicate.make_predicate lhs;
       e_rhs=List.rev e_rhs;
       i_rhs=List.rev i_rhs;
       content=UF.create (List.rev content)
      }
	
  (* the [dag] parameter [h] is meant to be the components of some
     predicate or rule *)
    let cyclic_unify i j h =
      match UF.cyclic i h with
      | true,_ -> raise Fails
      | _, h' -> 
	(try UF.union i j h with
	| UF.Union_Failure -> raise Fails)
	  
    (** [extract_consequence r content] returns a fact whose components
	all are of the form [Const c] (that is something of type {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate}) given . *)
    let extract_consequence r content =
      {ASPred.p_id=r.lhs.Predicate.p_id;
       ASPred.arity=r.lhs.Predicate.arity;
       ASPred.arguments=
	  List.map 
	    (function
	    | UF.Value v -> ASPred.Const v
	    | _ -> failwith "Bug: you're trying to extract non completely instantiated predicates")
	    (UF.extract (r.lhs.Predicate.arity) content)}
	
    (** [to_abstract r content] returns a datalog abstract syntax rule
	where the arguments of all (datalog abstract syntax)
	predicates have been computed using [content]. *)
    let to_abstract {id=id;lhs=lhs;e_rhs=e_rhs;i_rhs=i_rhs} content =
      let abs_lhs,vars,vargen=Predicate.to_abstract lhs (1,content) (Utils.IntMap.empty,VarGen.init ()) in
      let abs_e_rhs,start',vars',vargen'=Predicate.lst_to_abstract e_rhs (1+lhs.Predicate.arity,content) (vars,vargen) in
      let abs_i_rhs,_,_,_ = Predicate.lst_to_abstract i_rhs (start',content) (vars',vargen') in
      {ASRule.id=id;
       ASRule.lhs=abs_lhs;
       ASRule.e_rhs=abs_e_rhs;
       ASRule.i_rhs=abs_i_rhs
      }


    (** [FactArray] is a module implementing a traversal of facts using
	the {! ArrayTraversal.Make} functor. The [update] function is
	such that we don't consider cells (i.e. facts) that don't unify
	with the rule (i.e. a {! UF.Union_Failure} exception was
	raised).*)
    module FactArray=ArrayTraversal.Make2(
      struct
	type state = int*(ConstGen.id UF.t)
	type cell = Predicate.FactSet.elt (*P.fact *)
	module CellSet=Predicate.FactSet
	let update s c =
	  try 
	    Some (Predicate.instantiate_with c s)
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
	List.map (fun pred -> Predicate.PredMap.find pred.Predicate.p_id db) r.i_rhs in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
      let resume_on_i_pred acc state =
	FactArray.collect_results
	  (fun l_acc (_,content) -> (extract_consequence r content)::l_acc)
	  acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the facts of
       the extensional database (facts of the database corresponding
       to extensional predicates). *)
    let make_search_array_e_pred =
      List.map (fun pred -> Predicate.PredMap.find pred.Predicate.p_id db) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      []
      (r.lhs.Predicate.arity+1,r.content)
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
	Predicate.PredMap.find delta_position.Predicate.p_id (Predicate.Indexed_Facts.find time time_indexed_delta_intensional_facts) in
      let end_pred_facts =
	List.map
	  (fun pred -> Predicate.PredMap.find pred.Predicate.p_id (Predicate.Indexed_Facts.find (time-1) time_indexed_intensional_facts))
	  pred_lst in
      List.fold_left
	(fun acc pred ->
	  (Predicate.PredMap.find pred.Predicate.p_id (Predicate.Indexed_Facts.find time time_indexed_intensional_facts))::acc)
	(delta_facts::end_pred_facts)
	rev_pred_lst in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
    let resume_on_i_pred acc state =
      FactArray.collect_results
	(fun l_acc (_,content) -> agg_function (extract_consequence r content) l_acc)
	acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the
       facts of the extensional database *)
    let make_search_array_e_pred =
      List.map (fun pred -> Predicate.PredMap.find pred.Predicate.p_id e_facts) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      start
      (r.lhs.Predicate.arity+1,r.content)
      make_search_array_e_pred
  end

  module Program =
  struct
    type program = {rules:Rule.rule list Predicate.PredMap.t;
		  (* the list of the rules of the program indexed by
		     the id of this predicate *)
		    edb:ASPred.pred_id list;
		  (* the list of the ids of the extensional predicates *)
		    idb:ASPred.pred_id list;
		  (* the list of the ids of the intensional predicates *)
		    pred_table: ASPred.PredIdTable.table;
		 (* the table to record the translation from ids to
		    sym of the predicate *)
		   }
      
    let extend k v map_list =
      try
	let lst=Predicate.PredMap.find k map_list in
	Predicate.PredMap.add k (v::lst) map_list
      with
      | Not_found -> Predicate.PredMap.add k [v] map_list
	
    let make_program {ASProg.rules=r;ASProg.pred_table=pred_table;ASProg.i_preds=i_preds} =
      let rules = ASRule.Rules.fold
	(fun ({ASRule.lhs=lhs} as r) acc ->
	  let new_rule = Rule.make_rule r in
	  extend lhs.ASPred.p_id new_rule acc)
	r
	Predicate.PredMap.empty in
      let edb,idb=
	ASPred.PredIdTable.fold
	  (fun k _ (e,i) ->
	    if ASPred.PredIds.mem k i_preds then
	      (e,k::i)
	    else
	      (k::e,i))
	  pred_table
	  ([],[]) in
      {rules=rules;edb=edb;idb=idb;pred_table=pred_table}
	
	
    let to_abstract {rules=r;idb=idb;pred_table=pred_table} =
      let rules = 
	Predicate.PredMap.fold
	  (fun _ rules acc -> 
	    List.fold_left
	      (fun acc' rule -> ASRule.Rules.add (Rule.to_abstract rule rule.Rule.content) acc')
	      acc
	      rules)
	  r
	  ASRule.Rules.empty in
      let i_preds=
	List.fold_left
	  (fun acc id -> ASPred.PredIds.add id acc)
	  ASPred.PredIds.empty
	  idb in
      {ASProg.rules=rules;ASProg.pred_table=pred_table;ASProg.i_preds=i_preds}
	
	
	
    let all_temp_results_for_predicate s time {rules=rules} e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts =
      List.fold_left
	(fun acc r ->
	  let zip=Focused_list.init r.Rule.i_rhs in
	  Focused_list.fold
	    (fun l_acc focus -> Rule.temp r time focus e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts  (fun hd tl -> hd::tl) l_acc)
	    acc
	    zip)
	[]
	(Predicate.PredMap.find s.Predicate.p_id rules)
	
  (** [temp_facts r e_facts previous_step_facts facts delta_facts
      agg_f start] returns the result of applying [agg_f] to [start]
      and to all the facts that are deduced from [temp]{^ [time+1]}{_
      [S]} where [S] is the head predicate of the rule [r] and [temp]
      is the set of temporary rules associated with [r] as in the
      algorithm described in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu} (p.315).
      
      [previous_step_facts] and [facts] denote the intentional facts
      at the two required successive steps and [delta_facts] denote
      the new facts that are computed during this step. *)
	
  (* TODO: if a set of facts for a predicate of the rhs is empty, we
     can stop the computation *)
    let temp_facts r e_facts previous_step_facts facts delta_facts agg_function start =
    (* We firs collect all the contents compatible with the facts of
       the intensional database. They depend on the intensional
       predicate [delta_position] and the ones that are before it
       ([rev_pred_lst]) and the ones that are after it
       ([pred_lst]). This triple correspond to a {!Focused_list.t}
       type. *)
      let make_search_array_i_pred (rev_pred_lst,delta_position,pred_lst) =
	let delta_facts=
	  Predicate.PredMap.find delta_position.Predicate.p_id delta_facts in
	let end_pred_facts =
	  List.map
	    (fun pred -> Predicate.PredMap.find pred.Predicate.p_id previous_step_facts)
	    pred_lst in
	List.fold_left
	  (fun acc pred ->
	    (Predicate.PredMap.find pred.Predicate.p_id facts)::acc)
	  (delta_facts::end_pred_facts)
	  rev_pred_lst in
      (* We now init the focused list corresponding to the intensional
       predicates of the rule [r] *)
      let zip=Focused_list.init r.Rule.i_rhs in
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
	    Rule.FactArray.collect_results
	      (fun ll_acc (_,content) -> agg_function (Rule.extract_consequence r content) ll_acc)
	      l_acc
	      state
	      (make_search_array_i_pred focus))
	  acc
	  zip in
      (* We now collect all the contents compatible with the
	 facts of the extensional database *)
      let make_search_array_e_pred =
	List.map (fun pred -> Predicate.PredMap.find pred.Predicate.p_id e_facts) r.Rule.e_rhs in
      Rule.FactArray.collect_results
	(fun acc s -> 
	  (* For each partial completion of the rule on the extensional
	     database, we need to take into account the remaining
	     intensional predicates. *)
	  resume_on_i_pred acc s)
	start
	(r.Rule.lhs.Predicate.arity+1,r.Rule.content)
	make_search_array_e_pred
	
  (** [p_semantics_for_predicate s prog e_facts previous_step_facts
      facts delta_facts] returns a set of all the facts that can
      deduced by all the rules in [prog] at a given step and whose lhs
      predicate is [s] when the edb is [e_facts], the step has
      produced [facts] and the previous step has produced
      [previous_step_facts] and the variation of facts at this step
      are [delta_facts].
      
      It corresponds to [P]{^ [time]}{_ [S]} [(edb,T]{^ [time -1]}{_
      [1]}[,...,T]{^ [time-1]}{_ [l]}[,T]{^ [time]}{_ [1]}[,...,T]{^
      [time]}{_ [l]}[, Delta]{^ [time]}{_ [T]{_ [1]}},...,[Delta]{^
      [time]}{_ [T]{_ [l]}}) in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu} *)
    let p_semantics_for_predicate s_id {rules=rules} e_facts previous_step_facts facts delta_facts =
      List.fold_left
	(fun acc r ->
	  temp_facts
	    r
	    e_facts
	    previous_step_facts
	    facts
	    delta_facts
	    (fun hd tl -> Predicate.conditionnal_add
	      hd
	      tl
	      (Predicate.PredMap.find r.Rule.lhs.Predicate.p_id previous_step_facts)
	      (Predicate.PredMap.find r.Rule.lhs.Predicate.p_id delta_facts))
	    acc)
	Predicate.FactSet.empty
	(Predicate.PredMap.find s_id rules)
	
    let seminaive prog =
      let e_facts=Predicate.PredMap.empty in
      (** [seminaive_aux facts delta_facts] returns [(S]{^
	  [i]}[,][Delta]{^ [i+1]}{_ [S]}[)] for all [S] when [facts]
	  corresponds to [S]{^ [i-1]} for all [S] and [delta_facts] to
	  [Delta]{^ [i]}{_ [S]} for all [S] *)
      let rec seminaive_aux facts delta_facts =
	(* TODO: Check that PredMap has all intensional predicates of
	   prog *)
	let new_facts = 
	  Predicate.PredMap.merge
	    (fun pred_id v1 v2 ->
	      match v1,v2 with
	      | Some l1,Some l2 -> Some (Predicate.FactSet.union l1 l2)
	      | Some _ as v,None -> v
	      | None, (Some _ as v) -> v
	      | None,None -> None)
	    facts
	    delta_facts in
	let new_delta_facts = 
	  Predicate.PredMap.fold
	    (fun pred previous_facts acc ->
	      Predicate.PredMap.add
		pred
		(p_semantics_for_predicate pred prog e_facts previous_facts facts delta_facts)
		acc) 
	    Predicate.PredMap.empty 
	    new_facts in
	(new_facts,new_delta_facts) in
      (** [seminaive_rec (facts,delta_facts)] returns the result when
	  the fixpoint is reached, ie when [seminaive_aux facts
	  delta_facts] does not produce any new fact. This is the
	  iteration at step 5 in the seminaive algo. *)
      let rec seminaive_rec (facts,delta_facts) = 
	if Predicate.PredMap.is_empty delta_facts then
	  facts
	else
	  seminaive_rec (seminaive_aux facts delta_facts) in
      ()
	
	
  end
    
    
end
    
    
