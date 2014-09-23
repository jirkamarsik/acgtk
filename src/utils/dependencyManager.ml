module type Manager_sig =
  sig
    type t
    type elt
    val add_dependency : elt -> elt -> t -> t
    val dependencies : elt -> t -> elt list
  end


module Make(O:OrderedType) =
struct
  module EltSet=Set.Make(O)
  module EltMap=Map.Make(O)

  type t = {depends_on:EltSet.t EltMap.t;
	    dependants:EltSet.t EltMap.t;
	   }

  type elt=O.t

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
    {depends_on=EltMap.add e1 (EltSet.add e2 depends_on);
     dependants=EltMap.add e2 (EltSet.add e1 depends_on)}
    
  let dependencies_rec elt man depth =
      try
	let dependants = EltMap.find elt man.dependants in
	EltSet.fold
	  (fun elt acc ->
	   
      with
      | Not_found -> depth
      


  let dependencies elt man =
    
      



end
