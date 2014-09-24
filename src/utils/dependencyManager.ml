open Utils

module type Manager_sig =
  sig
    type t
    type elt
    val empty : t
    val add_dependency : elt -> elt -> t -> t
    val dependencies : elt -> t -> elt list
  end


module Make(O:sig type t val compare:t->t->int val to_string:t->string end) =
struct
  module EltSet=Set.Make(O)
  module EltMap=Map.Make(O)

  type t = {depends_on:EltSet.t EltMap.t;
	    dependants:EltSet.t EltMap.t;
	   }

  type elt=O.t

  let empty = {depends_on=EltMap.empty;
	       dependants=EltMap.empty}

  let add_dependency e1 e2 man =
    let depends_on =
      try
	EltMap.find e1 man.depends_on
      with
      | Not_found -> EltSet.empty in
    let dependants =
      try
	EltMap.find e2 man.dependants
      with
      | Not_found -> EltSet.empty in
    {depends_on=EltMap.add e1 (EltSet.add e2 depends_on) man.depends_on;
     dependants=EltMap.add e2 (EltSet.add e1 dependants) man.dependants}
    
  let rec dependencies_rec elt man depth depthMap =
    try
      let dependants = EltMap.find elt man.dependants in
      let new_depthMap = 
	EltSet.fold
	  (fun elt depthMap ->
	   let depth =
	     try
	       max (depth+1) (EltMap.find elt depthMap)
	     with
	     | Not_found -> depth+1 in
	   EltMap.add elt depth depthMap)
	  dependants
	  depthMap in
      EltSet.fold
	(fun elt depthMap ->
	 dependencies_rec elt man (EltMap.find elt depthMap) depthMap)
	dependants
	new_depthMap
    with
    | Not_found -> depthMap
			      
  let dependencies elt man =
    let depthMap = dependencies_rec elt man 0 EltMap.empty in
    let orderedElt =
      EltMap.fold
	(fun elt depth acc ->
	 try 
	   IntMap.add depth (elt::(IntMap.find depth acc)) acc
	 with
	 | Not_found -> IntMap.add depth [elt] acc)
	depthMap
	IntMap.empty in
    let () =
      IntMap.iter
	(fun depth elts ->
	 Printf.printf
	   "at depth %i from %s we have the following elements: %s\n"
	   depth
	   (O.to_string elt)
	   (string_of_list " " O.to_string elts))
	orderedElt
    in
    List.rev
      (IntMap.fold
	 (fun _ elts acc -> elts@acc)
	 orderedElt
	 [])
   
      



end
