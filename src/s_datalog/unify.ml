open Utils
open PersistentArray

(** Modules with this module type should provide Union-Find algorithms
    and the indexed storage data structure. Note that we take the
    opportunity of implementing from scratch such algorithms to allow
    the [find] function returns not only the index of the
    representative and the values it indexes, but also the storage
    data structure, so that the [find] algorithm can modify it, in
    particular with path compression.
*)

module type UnionFind = 
sig
  (** The type of the indexed data structure *)
  type 'a t

  (** The type of the values (content) that are indexed. It is either
      an actual value of type ['a] or a link to another indexed
      value. If a content at an index [i] points to [i], it is meant
      that to be a variable.*)
  type 'a content = Link_to of int | Value of 'a

  (** Exception raised when a the union of to indexed value can not
      happen. It should be raised by the [union] function when it
      amounts to make the union between to actual values [Value a] and
      [Value b] and [a != b]. *)
  exception Union_Failure

  (** [create l] returns the corresponding indexed storage data
      structure where each value (or link) is indexed by its position in [l]
      (starting at 1 *)
  val create : 'a content list -> 'a t

  (** [extract i t] returns a list of the [i] first elements of
      [t] *)
  val extract : int -> 'a t -> 'a content list

  (** [find i h] returns not only the index of the representative and
      the values it indexes, but also the storage data structure, so
      that the [find] algorithm can modify it, in particular with path
      compression. *)
  val find : int -> 'a t -> ((int * 'a content) * 'a t)
  (* the content returned by [find] should not be a link. Can we
     enforce this using polymorphic variants and/or GADT? *)
    
  (** [union i j h] returns a new indexed storage data structure where
      values indexed by [i] and [j] have been unified (ie one of the
      two is now linked to the index of the representative of the
      other. It fails and raises the {! UnionFind.Union_Failure}
      exception if both [i] and [j] representatives index actual
      values [Value a] and [Value b] and [a != b]. *)
  val union : int -> int -> 'a t -> 'a t

  (** [instantiate i t h] returns a new indexed storage data structure
      where the value indexed by [i] and [t] have been unified. It
      fails and raises the {! UnionFind.Union_Failure} exception if
      [i]'s representative indexes an actual values [Value a] such
      that [a] differs from [t]. *)
  val instantiate : int ->  'a  -> 'a t -> 'a t

  (** [cyclic i h] returns a pair [(b,h')] where [b] is [true] if [h]
      has a cycle (following the [Link_to] links) containing [i] and
      [false] otherwise, and where [h'] contains the same information
      as [h] (possibly differently stored, for instance using path
      compression while checking [h] cyclicity. *)
  val cyclic : int -> 'a t -> (bool * 'a t)
end

(** Modules with this module type should provide an indexed (by [int]
    indexes) storage data structure for ['a] type values and access
    and update functions.
*)
  
module type Store =
sig
  type 'a t
  exception Not_found

  (** [empty i] should return an empty indexed storage data structure
      that will allow indexing {e with values from [1] to [i]}. *)
  val empty : int -> 'a t
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
end

(** This (functor) module implements a {! UnionFind} data structure. The
    [S] parameter is used to try different implementations of indexed
    data structure, in particular eventually persistent arrays as
    described in {{:
    http://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps}"A Persistent
    Union-Find Data Structure" (Sylvain Conchon and Jean-Chrisophe
    Filliâtre} *)
module Make(S:Store) : UnionFind  = 
struct

  (** The type of the values (content) that are indexed. It is either
      an actual value of type ['a] or a link to another indexed
      value. If a content at an index [i] points to [i], it is meant
      that to be a variable.*)
  type 'a content =
  | Link_to of int
  | Value of 'a

  (** The actual type of the data structure. The rank is used to
      implement weighted union. See {{:
      http://www.risc.jku.at/education/courses/ss2012/unification/slides/02_Syntactic_Unification_Improved_Algorithms.pdf}
      Introduction to Unification Theory. Speeding Up (Temur
      Kutsia)} *)
  type 'a t = {rank:int S.t;parents:'a content S.t}

  exception Union_Failure


  (* Indexing starts at 1, not at 0 *)
  (* TODO: Should we check that indexes belong to the range, or that
     links to belong the set of indexes? *)
  (* TODO: specify the properties of the data structure (no cycle,
     coherent numbering, [find] always returns a value, etc. *)
  let create contents =
    let ln = List.length contents in
    let res,_=
      List.fold_left
	(fun ({rank=r;parents=p},k) -> function
	| Link_to i as c ->
	  (* rank is unset for contents that are initially a link *)
	  ({rank=
	      (try
		let rank=S.get i r in
		S.set i (rank+1) r
	      with
	      | S.Not_found -> S.set i 1 r);
	    parents=S.set k c p},k+1)
	| Value v as c ->
	  ({rank=
	      (try
		 let _ = S.get k r in
		 r
	       with
	       | S.Not_found -> S.set k 0 r);
	    parents=S.set k c p},k+1))
	({rank=S.empty ln;parents=S.empty ln},1)
	contents in
    res

  (** [extract i t] returns a list of the [i] first elements of
      [t] *)
  let extract i {parents=p} =
    let rec extract_aux k res =
      match k with
      | 0 -> res
      | i when i>0 -> extract_aux (i-1) ((S.get i p) :: res)
      | _ -> failwith "Bug: you should ask for a positive index" in
    extract_aux i []
    

  
  (** [find_aux i f] returns a pair [(i',v),f'] where [i'] is the
      index of the representative of the data indexed by [i]. [i=i']
      means that the [i]-th element is linked to itself: it is meant
      to be a variable, not an actual value. It also performs path
      compression *)
  let rec find_aux i f =
    match S.get i f with
    | Value _ as v -> (i,v),f 
    (* An actual value was reached at index [i]. So [i] is returned
       together with [v] and [f] *)
    | Link_to next as v when next=i -> (i,v),f
    (* The content indexed by [i] points to [i]. [i] is then the
       representative for the variable it denotes. *)
    | Link_to next ->
      (* In the other cases, we follow the links to reach the
	 representative and the content it indexes *)
      let (representative_index,representative_value),new_f = find_aux next f in
      (* Then we update the storage data structure linking the context
	 indexed by [i] directly to the representative index *)
      let updated_f = S.set i (Link_to representative_index) new_f in
      (representative_index,representative_value),updated_f
	
  (** [find i h] returns a pair [(i',v),f'] where [i'] is the index of
      the representative of the data indexed by [i]. [i=i'] means that
      the [i]-th element is linked to itself: it is meant to be a
      variable, not an actual value. It also performs path
      compression. The difference with [find_aux] is that it applyes
      to the whole storage data structure (that includes data for
      weighted union). *)
  let find i h =
    let rep_i,f = find_aux  i h.parents in
    rep_i,{h with parents=f}

  (** [union i j h] returns a new storage data structure [h'] where
      [h'] has an equivalent content as [h] plus the unification
      between the elements indexed by [i] and [j] and plus, possibly,
      some path compression. *)
  let union i j h =
    let rep_i,h' = find i h in
    let rep_j,h'' = find j h' in
    match rep_i,rep_j with
    (* in case [rep_i] (rexp. [rep_j]) is a [(i,Link_to i')] we should
       have [i=i'], else there is a bug *)
    | (_,v_i),(_,v_j) when v_i=v_j -> h''
    | (_,(Value _ as v_i)),(rep_j_index,Link_to _) ->
      {h'' with parents=S.set rep_j_index v_i h''.parents}
    | (rep_i_index,Link_to _),(_,(Value _ as v_j)) ->
      {h'' with parents=S.set rep_i_index v_j h''.parents}
    | (rep_i_index,Link_to i'),(rep_j_index,Link_to j') -> 
      let rk_i = S.get rep_i_index h''.rank in
      let rk_j = S.get rep_j_index h''.rank in
      if rk_i > rk_j then
	{h'' with parents=S.set rep_j_index (Link_to rep_i_index) h''.parents}
      else
	if rk_i < rk_j then
	  {h'' with parents=S.set rep_i_index (Link_to rep_j_index) h''.parents}
	else
	  {rank=S.set rep_i_index (rk_i+1) h''.rank;parents=S.set rep_j_index (Link_to rep_i_index) h''.parents}
    |  (_,Value v_i),(_,Value v_j) -> raise Union_Failure
  (* v_i=v_j is caught by the first case *)

  (** [find_and_instantiate_aux i t f] returns a new indexed storage
      datastructure [f'] where the content at index [i] (and the ones
      it points to) has been set to [Value t]. If [i]'s representative
      indexes a variable or a value equal to [Value t] then the
      instantiation suceeds, otherwise it raises Union_failure. It
      also performs path compression.  *)
  let rec find_and_instantiate_aux i term f =
    match S.get i f with
    | Value v when v=term -> f 
    | Value _ -> raise Union_Failure
    (* An actual value was reached at index [i] and we're in the case
       that it differs from [term]. So the union fails *)
    | Link_to next when next=i -> S.set i (Value term) f
    (* The content indexed by [i] points to [i]. [i] is then the
       representative for the variable it denotes and can be unified
       with [term]. [f] is updated. *)
    | Link_to next ->
      (* In the other cases, we follow the links to reach the
	 representative and the content it indexes *)
      let new_f = find_and_instantiate_aux next term f in
      (* Then we update the storage data structure linking the context
	 indexed by [i] directly to the representative index. We know
	 it's safe to do it now since unification succeeded. *)
      let updated_f = S.set i (Value term) new_f in
      updated_f


  (** [instantiate i t h] returns a new indexed storage data structure
      where the value indexed by [i] and [t] have been unified. It
      fails and raises the {! UnionFind.Union_Failure} exception if
      [i]'s representative indexes an actual values [Value a] such
      that [a] differs from [t]. *)
  let instantiate i t h =
    let f = find_and_instantiate_aux i t h.parents in
    {h with parents=f}


	
  (* cyclic_aux includes path compression *)
  let rec cyclic_aux i f acc =
    match S.get i f with
    | Value v -> false,i,f
    | Link_to next ->
      if IntSet.mem next acc then
	true,i,f
      else
	let cyclic,representative_index,new_f = cyclic_aux next f (IntSet.add next acc) in
	let updated_f = S.set i (Link_to representative_index) new_f in
	cyclic,representative_index,updated_f
	  
    (* the cyclic function, calling cyclic_aux, compress paths
       (hence also returns the parents) *)
  let cyclic i h = 
    let res,_,f = cyclic_aux i h.parents (IntSet.empty) in
    res,{h with parents=f}
      
      
end

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

module Store_as_Map =
struct
  module Map_from_int=Map.Make(struct type t=int let compare = Pervasives.compare end)
  type 'a t = 'a Map_from_int.t
  exception Not_found
  let empty = Map_from_int.empty
  let get k m = 
    try
      Map_from_int.find k m
    with
    | Not_found -> raise Not_found
  let set k v m = Map_from_int.add k v m
end



module type Pred_and_Rules_TYPE =
sig
  type content =  | Var of VarGen.var | Const of Const.t
  type predicate={name:string;
		  arity:int;
		  components:content list
		 (* It is assumed that the size of the list is the
		    arity *)
		 }      
  type rule={id:int;
	     lhs:predicate;
	     rhs:predicate list
	    }
end

(** These modules are the abstract syntactic representations of
    predicates and rules *)
module Pred_and_Rules =
struct
  type content =  | Var of VarGen.var | Const of Const.t
  type predicate_name=string
  type predicate={name:predicate_name;
		  arity:int;
		  components:content list
		 (* It is assumed that the size of the list is the
		    arity *)
		 }      
  type rule={id:int;
	     lhs:predicate;
	     rhs:predicate list
	    }
end

module Facts=Map.Make(
  struct
    type t=string
    let compare=String.compare
  end)

  
module Unify (S:Store)(P:Pred_and_Rules_TYPE) =
struct
  exception Fails
  module UF= Make(S)
    
  type predicate={name:string; (* TODO: Should be an int for efficiency*)
		  arity:int;
		 }
    
  (** In an [internal_rule], all the compoments of all the predicates
      are stored in a {! UnionFind} indexed data structure. We assume
      here that from 1 to lhs.arity the components of the left hand
      side predicate are stored, then from [lhs.arity+1] to
      [lhs.arity+(hd rhs).arity] the components of the first predicate
      on the right hand side are stored, etc. It is assumed that this
      structure is correct (no cycle, links within the range, etc.) *)
  type rule={id:int;
	     lhs:predicate;
	     rhs:predicate list;
	     content:Const.t UF.t
	    (* TODO: Maybe but the label of the predicate in the
	       content in order to enforce checking of the current
	       instantiation *)
	    }

  type fact=P.predicate Facts.t

  (* {P.name=n;P.arity=k;P.components=comp} *)

  let make_predicate p = {name=p.P.name;arity=p.P.arity}

  let add_pred_components_to_content components (content,idx,mapped_vars) =
    (* [components] is the list of components of some predicate *)
    (* [content] is a list meant to become the content of a rule,
       i.e. an indexed storage data structure that is meant to be
       extended with [components]. *BE CAREFUL: IT COMES IN INVERSE
       ORDER* *)
    (* [idx] is the index to be given for the next element of [content]*)
    (* [mapped_vars] is a mapping from [P.Var i] variables to the
       index at which they've been stored in [content]. When such a
       variable is met for the first time, as expected in the
       UnionFind data structure, the content at [idx] is [Lin_to]'ed
       itself. *)
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

  let make_rule {P.id=id;P.lhs=lhs;P.rhs=rhs} =
    (* Be careful, the list of the rhs is reversed *)
    let lhs_content=
      add_pred_components_to_content lhs.P.components ([],1,VarGen.VarMap.empty) in
    let rhs,(content,_,_) =
      List.fold_left
	(fun (rhs,content) {P.name=n;P.arity=k;P.components=pred_comps} ->
	  ({name=n;arity=k} :: rhs,
	   add_pred_components_to_content pred_comps content))
	([],lhs_content)
	rhs in
    {id=id;
     lhs=make_predicate lhs;
     rhs=List.rev rhs;
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
	
  (** [instantiate_idb_predicate_in_rule p (i,c)] returns a pair
      [(i',c')] when [i] is the index of the first compoment of the
      [p] predicate in the content [c] {e THIS IS NOT CHECKED
      HERE}. [i'=i+a] where [a] is the arity of [p] (it means [i']
      should index the first component of the next predicate in the
      content of the rule) and [c'] is a new content where all the
      components between [i] and [i'-1] have been instantiated with
      the components of [p]. When such an instantiation fails, it
      raises {! UF.Union_Failure} *)
  let instantiate_idb_predicate_in_rule
      {P.name=_;P.arity=_;P.components=components}
      (idx,content) =
    List.fold_left
      (fun (i,cont) value -> (i+1,UF.instantiate i value cont))
      (idx,content)
      components


  let extract_fact_from_rule r content =
    {P.name=r.lhs.name;
     P.arity=r.lhs.arity;
     P.components=
	List.map 
	  (function
	  | UF.Value v -> v
	  | _ -> failwith "Bug: you're trying to extract non completely instanciated predicates")
	  (UF.extract (r.lhs.arity) content)}
	  
(*  let rec instantiate_rule r idb =
    let i = r.lhs.arity +1 in
    let (next_i,new_content),resume = instantiate_rule_aux r.rhs i idb r.content [] in
    
    
    
    
    let rec instantiate_rule_aux predicates i idb content resume =
      match predicates with
      | [] -> (Some content),resume
      | pred::remaining_preds ->
	let ground_instances_of_pred = Facts.find pred.P.name idb in
	(* TODO: catch the exception *)
	match ground_instances_of_pred with
	| [] -> None,resume
	| p::tl -> 
	  let next_i,new_content = instantiate_idb_predicate_in_rule p (i,content) in
	  instantiate_rule_aux
	    remaining_preds
	    next_i
	    idb
	    new_content
	    (i,content,tl)::resume
	    
	    
*)



	    
	    

end
  
  
