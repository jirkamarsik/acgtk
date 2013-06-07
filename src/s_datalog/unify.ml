open PersistentArray

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

(** These modules are the abstract syntactic representations of
    predicates and rules *)
module Pred_and_Rules =
struct
  type content =  | Var of VarGen.var | Const of Const.t
  type predicate_id_type=string
  type predicate={p_id:predicate_id_type;
		  arity:int;
		  components:content list
		 (* It is assumed that the size of the list is the
		    arity *)
		 }      
  type rule={r_id:int;
	     lhs:predicate;
	     e_rhs:predicate list;
	     i_rhs:predicate list;
	    }
end


  
module Unify (S:UnionFind.Store)(P:RulesAbstractSyntac_TYPE) =
struct
  exception Fails
  module UF= UnionFind.Make(S)

  type predicate_id_type=string 
  type predicate={p_id:P.predicate_id_type;    (* TODO: Should be an int for efficiency*)
		  arity:int;
		 }

  module Facts=Utils.StringMap
  (*Map.Make( struct type t=string let compare=String.compare end) *)

  module Indexed_Facts=Utils.IntMap 
    
  (** In an [internal_rule], all the compoments of all the predicates
      are stored in a {! UnionFind} indexed data structure. We assume
      here that from 1 to lhs.arity the components of the left hand
      side predicate are stored, then from [lhs.arity+1] to
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
	       instanciation *)
	    }

  let make_predicate p = {p_id=p.P.p_id;arity=p.P.arity}



  (** [add_pred_components_to_content components
      (content,idx,mapped_vars)] returns a triple
      (content',idx',mapped_vars') where [content'] is the list
      [content] to which has been added {e *in the reverse order*}
      updated with information from components. The update is such
      that if the component is a [Var i] then it is replaced by a
      [Link_to j] such that [j] is the index at which the variable
      [Var i] was met for the first time. If the component is a [Const
      c], the a [Value c] is added at the current position. [idx'] is
      the value of the next position if another list of component has
      to be added. And [mapped_vars'] is a map from variables [Var i]
      to positions (i.e. [int]) in which these variables first occur
      in [content']

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
      (fun (cont,i,vars)  -> function
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
	
  (** [instanciate_idb_predicate_in_rule p (i,c)] returns a pair
      [(i',c')] when [i] is the index of the first component of the
      [p] predicate in the content [c] {e THIS IS NOT CHECKED
      HERE}. [i'=i+a] where [a] is the arity of [p] (it means [i']
      should index the first component of the next predicate in the
      content of the rule) and [c'] is a new content where all the
      components between [i] and [i'-1] have been instanciated with
      the components of [p]. When such an instanciation fails, it
      raises {! UF.Union_Failure} *)
  let instanciate_idb_predicate_in_rule
      {P.p_id=_;P.arity=_;P.components=components}
      (idx,content) =
    List.fold_left
      (fun (i,cont) value ->
	(i+1,
	 match value with
	 | P.Var _ -> failwith "Buh: you're trying to instanciate with a noun ground term"
	 | P.Const v -> UF.instantiate i v cont))
      (idx,content)
      components

  (** [extract_consequence_from_rule r content] returns a fact (that
      is something of type [P.rule] whose components all are of the
      form [Const c] *)
  let extract_consequence_from_rule r content =
    {P.p_id=r.lhs.p_id;
     P.arity=r.lhs.arity;
     P.components=
	List.map 
	  (function
	  | UF.Value v -> P.Const v
	  | _ -> failwith "Bug: you're trying to extract non completely instanciated predicates")
	  (UF.extract (r.lhs.arity) content)}


  (** [FactArray] is a module implementing a traversal of facts using
      the {! ArrayTraversal.Make} functor. The [update] function is
      such that we don't consider cells (i.e. facts) that don't unify
      with the rule (i.e. a {! UF.Union_Failure} exception was
      raised).*)
  module FactArray=ArrayTraversal.Make(
    struct
      type state = int*(Const.t UF.t)
      type cell = P.predicate
      let update s c =
	try 
	  Some (instanciate_idb_predicate_in_rule c s)
	with
	| UF.Union_Failure -> None
    end
  )
    
  (** [immediate_consequence_of_rule r idb] returns a list of facts
      generated by the rule [r] using the facts stored in [idb]. {e
      *these facts are not added to [idb] when collecting the new
      facts}.

      Note that it is important that resulting states need to be
      processed otherwise they will be lost in backtracking when using
      {! PersistentArray}.*)
  let rec immediate_consequence_of_rule r idb =
    (* We collect all the contents compatible with the facts of the
       intensional database *)
    let make_search_array_i_pred =
      List.map (fun pred -> Facts.find pred.p_id idb) r.i_rhs in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
    let resume_on_i_pred acc state =
      FactArray.collect_results
	(fun l_acc (_,content) -> (extract_consequence_from_rule r content)::l_acc)
	acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the facts of
       the extensional database *)
    let make_search_array_e_pred =
      List.map (fun pred -> Facts.find pred.p_id idb) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      []
      (r.lhs.arity+1,r.content)
      make_search_array_e_pred

  (** [temp r time delta_position e_facts
      time_indexed_intensional_facts
      time_indexed_delta_intensional_facts] returns a list ot new
      facts that are deduced from [temp{^ time+1}{_ S}] whrer [S] is
      the head predicate of the rule [r] and [delta_position] the
      index of the intensional predicate that is under focus as
      described in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu}
      (p.315).*)
  let temp r time (rev_pred_lst,delta_position,pred_lst) e_facts time_indexed_intensional_facts time_indexed_delta_intensional_facts =
    (* We collect all the contents compatible with the facts of the
       intensional database *)
    let make_search_array_i_pred =
      let delta_facts=
	Facts.find delta_position.p_id (Indexed_Facts.find time time_indexed_delta_intensional_facts) in
      let end_pred_facts =
	List.map
	  (fun pred -> Facts.find pred.p_id (Indexed_Facts.find (time-1) time_indexed_intensional_facts))
	  pred_lst in
      List.fold_left
	(fun acc pred ->
	  (Facts.find pred.p_id (Indexed_Facts.find time time_indexed_intensional_facts))::acc)
	(delta_facts::end_pred_facts)
	rev_pred_lst in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
    let resume_on_i_pred acc state =
      FactArray.collect_results
	(fun l_acc (_,content) -> (extract_consequence_from_rule r content)::l_acc)
	acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the
       facts of the extensional database *)
    let make_search_array_e_pred =
      List.map (fun pred -> Facts.find pred.p_id e_facts) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      []
      (r.lhs.arity+1,r.content)
      make_search_array_e_pred
      
  type program = {i_db:rule list Facts.t;e_db:rule list Facts.t}
    
(*  let all_temp_results_for_predicate s {i_db=i_db;e_db=idb} time_indexed_intensional_facts time_indexed_delta_intensional_facts =
    let rules =
      List.fold_left
	(fun acc rule -> )
	Facts.find s i_db
*)  


end
  
  
