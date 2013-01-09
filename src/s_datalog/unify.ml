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

  (** The type of the values that are indexed. It is either an actual
      value of type ['a] or a link to another indexed value. *)
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

  (** [cyclic i h] returns a pair [(b,h')] where [b] is [true] if [h]
      has a cycle (following the [Link_to] links) containing [i] and
      [false] otherwise, and where [h'] contains the same information
      as [h] (possibly differently stord, for instance using path
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
  val empty : 'a t
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
end

module Make(S:Store) : UnionFind  = 
struct
  type 'a content =
  | Link_to of int
  | Value of 'a
  type 'a t = {rank:int S.t;parents:'a content S.t}
  exception Union_Failure

    (* On numérote depuis 1 et pas 0 *)
    (* TODO: Vérifier que les index restent dans le range *)
    (* Spécifier ce qu'on attend de la structure d'entrée (pas de
       cycle, numérotation cohérente, find fonctionne toujours,
       etc.) *)
    let create contents =
      let res,_=
	List.fold_left
	  (fun ({rank=r;parents=p},k) -> function
	  | Link_to i as c ->
	    (* rank is unset for contents that are initially a ling *)
	    ({rank=S.set i 1 r;parents=S.set k c p},k+1)
	  | Value v as c ->
	    ({rank=
		(try
		   let _ = S.get k r in
		   r
		 with
		 | S.Not_found -> S.set k 0 r);
	      parents=S.set k c p},k+1))
	  ({rank=S.empty;parents=S.empty},1)
	  contents in
      res
	
    let rec find_aux f i =
      match S.get i f with
      | Value _ as v -> (i,v),f
      | Link_to next as v when next=i -> (i,v),f
      | Link_to next -> 
	let (representative_index,representative_value),new_f = find_aux f next in
	let updated_f = S.set i (Link_to representative_index) new_f in
	(representative_index,representative_value),updated_f

    let find i h =
      let rep_i,f = find_aux h.parents i in
      rep_i,{h with parents=f}

       
    let union i j h =
      let rep_i,h' = find i h in
      let rep_j,h'' = find j h' in
      match rep_i,rep_j with
      (* in case rep_i (rexp. rep_j) is a (i,Link_to i') we should
	 have i=i', else there is a bug *)
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
      |  (_,Value v_i),(_,Value v_j) -> raise Union_Failure (* v_i=v_j is caught by the first case *)

    module IntSet=Set.Make(struct type t=int let compare=compare end)

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
end

module Const =
struct
  type t = int
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


module Predicate =
struct
  type content = [`Var of VarGen.var | `Const of Const.t]
  type t={name:string;
	  arity:int;
	  components:content list
	   (* It is assumed that the size of the list is the arity *)
	 }      
end
      
module Unify (S:Store) =
struct
  exception Fails
  module UF= Make(S)
  (* the [dag] parameter is meant to be the components of the
     predicates *)

  let cyclic_unify i j h =
    match UF.cyclic i h with
    | true,_ -> raise Fails
    | _, h' -> 
      (try UF.union i j h with
      | UF.Union_Failure -> raise Fails)
end
  
  
