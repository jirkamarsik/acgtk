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

  
module Unify (S:UnionFind.Store)(P:Pred_and_Rules_TYPE) =
struct
  exception Fails
  module UF= UnionFind.Make(S)
    
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

(*  type fact=(P.predicate list) Facts.t*)

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
      [(i',c')] when [i] is the index of the first component of the
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
      (fun (i,cont) value ->
	(i+1,
	 match value with
	 | P.Var _ -> failwith "Buh: you're trying to instantiate with a noun ground term"
	 | P.Const v -> UF.instantiate i v cont))
      (idx,content)
      components

  (** [extract_fact_from_rule r content] returns a fact (that is
      something of type [P.rule] whose components all are of the form [Const

  *)
  let extract_fact_from_rule r content =
    {P.name=r.lhs.name;
     P.arity=r.lhs.arity;
     P.components=
	List.map 
	  (function
	  | UF.Value v -> v
	  | _ -> failwith "Bug: you're trying to extract non completely instanciated predicates")
	  (UF.extract (r.lhs.arity) content)}


  module FactArray=ArrayTraversal.Make(
    struct
      type state = int*(Const.t UF.t)
      type cell = P.predicate
      let update s c =
	try 
	  Some (instantiate_idb_predicate_in_rule c s)
	with
	| UF.Union_Failure -> None
    end
    )
	  
	  
  let rec instantiate_rule r idb =
    let make_search_array = List.map (fun pred -> Facts.find pred.name idb) r.rhs in
    FactArray.collect_results
      (fun acc (_,s) -> s::acc)
      []
      (r.lhs.arity+1,r.content)
      make_search_array
      

	    
	    

end
  
  
