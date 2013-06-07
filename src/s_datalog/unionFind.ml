open Utils


(** Modules with this module type should provide Union-Find algorithms
    and the indexed storage data structure. Note that we take the
    opportunity of implementing from scratch such algorithms to allow
    the [find] function returns not only the index of the
    representative and the values it indexes, but also the storage
    data structure, so that the [find] algorithm can modify it, in
    particular with path compression.
*)

module type S = 
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

  val copy : 'a t -> 'a t
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
  val copy : 'a t -> 'a t
end
  
(** This (functor) module implements a {! UnionFind} data structure. The
    [S] parameter is used to try different implementations of indexed
    data structure, in particular eventually persistent arrays as
    described in {{:
    http://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps}"A Persistent
    Union-Find Data Structure" (Sylvain Conchon and Jean-Chrisophe
    Filliâtre} *)
module Make(S:Store) : S  = 
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

  let copy {rank=r;parents=p}={rank=S.copy r;parents=S.copy p}
      
end

module StoreAsMap =
struct
  type 'a t = 'a IntMap.t
  exception Not_found
  let empty _ = IntMap.empty
  let get k m = 
    try
      IntMap.find k m
    with
    | Not_found -> raise Not_found
  let set k v m = IntMap.add k v m
  let copy m=m
end


