module type UnionFind =
  sig
    type 'a t
    type 'a content = Link_to of int | Value of 'a
    val create : ('a content) list -> 'a t
    val find : int -> 'a t -> (int * 'a t)
    val union : ?use_rank:bool -> int -> int -> 'a t -> 'a t
    val cyclic : int -> 'a t -> (bool*'a t)
end
    
module type Store =
sig
  type 'a t
  exception Not_found
  val empty : 'a t
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
end



module Make(S:Store) : UnionFind =
  struct
    type 'a content =
    | Link_to of int
    | Value of 'a
    type 'a t = {rank:int S.t;parents:'a content S.t}

    (* On numérote depuis 1 et pas 0 *)
    (* TODO: Vérifier que les index restent dans le range *)
    (* Spécifier ce qu'on attend de la structure d'entrée (pas de
       cycle, numérotation cohérente, find fonctionne toujours,
       etc.) *)
    let create contents =
      let res,_=
	List.fold_left
	  (fun ({rank=r;parents=p},k) -> function
	  | Link_to i as c -> ({rank=S.set i 1 r;parents=S.set k c p},k+1)
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
      | Value v -> i,f
      | Link_to next ->
	let representative_index,new_f = find_aux f next in
	let updated_f = S.set i (Link_to representative_index) new_f in
	representative_index,updated_f

    let find i h =
      let rep_i,f = find_aux h.parents i in
      rep_i,{h with parents=f}

       
    let union ?(use_rank=true) i j h =
      let rep_i,h' = find i h in
      let rep_j,h'' = find j h' in
      if rep_i=rep_j then
	h''
      else
	if (not use_rank) then
	  {h with parents=S.set rep_i (Link_to rep_j) h.parents}
	else
	  let rk_i = S.get rep_i h.rank in
	  let rk_j = S.get rep_j h.rank in
	  if rk_i > rk_j then
	    {h with parents=S.set rep_j (Link_to rep_i) h.parents}
	  else
	    if rk_i < rk_j then
	      {h with parents=S.set rep_i (Link_to rep_j) h.parents}
	    else
	      {rank=S.set rep_i (rk_i+1) h.rank;parents=S.set rep_j (Link_to rep_i) h.parents}

    module IntSet=Set.Make(struct type t=int let compare=compare end)

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
    type content = Var of VarGen.var | Const of Const.t
    type t={name:string;
	    arity:int;
	    components:content list
	   (* It is assumed that the size of the list is the arity *)
	   }
      
end
      
module Unify (S:Store) =
struct
  exception Fails
  module UF:UnionFind=Make(S)
  let cyclic_unify i j h dag =
    let cyclic_unify_aux i j h =
      if i = j then
	h
      else
	match S.get i dag,S.get j dag with
	| Predicate.Var _,Predicate.Var _ -> UF.union  i j h
	| Predicate.Var _,Predicate.Const _ -> UF.union ~use_rank:false i j h
	| Predicate.Const _ ,Predicate.Var _ -> UF.union ~use_rank:false j i h
	| Predicate.Const c_i,Predicate.Const c_j when c_i=c_j -> h
	| Predicate.Const _,Predicate.Const _ -> raise Fails
    in
    match UF.cyclic i h with
    | true,_ -> raise Fails
    | _, h' -> cyclic_unify_aux i j h' 
end
    
	      
