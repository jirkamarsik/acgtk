module AlternTrees =
struct
  type address=(int*int) list * (int option)
  (* (child position, position in the forest) list plus a last child
     position *)
  type relative_path=int*(int option)*address
  (* the 2nd argument is to move in the alternative trees at the top
     of the forest *)

  let rec add_diff (add1,child1) (add2,child2) back =
    match add1,add2 with
    | [],[] ->
      (match child1,child2 with
      | None,None -> back,([],None)
      | Some _,None ->back+1,([],None)
      | None,Some _ ->back,([],child2)
      | Some _,Some _ ->back+1,([],child2))
    | _,[] -> back+List.length add1+1,([],child2)
    | [],_ -> 
      (match child1 with
      | None -> back,(add2,child2)
      | Some _ -> back+1,(add2,child2))
    | (i,j)::tl1,(i',j')::tl2 when i=i' && j=j' -> add_diff (tl1,child1) (tl2,child2) back
    | _::_,_::_ -> back+List.length add1+1,(add2,child2)

  let diff (alt,address) (alt',address') =
    if alt=alt' then
      let back,add=add_diff address address' 0 in
      back,None,add
    else
      let back =
	match address with
	| [],None -> 0
	| lst,Some _ -> List.length lst +1
	| _,None -> failwith "Bug: Only root can have ([],None) address" in
      back,Some alt',address'

  let address_to_string = function
    | [],None -> ""
    | lst,Some child ->
      Printf.sprintf "%s, child %d" (Utils.string_of_list ";" (fun (i,j) -> Printf.sprintf "(%d,%d)" i j) lst) child
    | _ -> failwith "Bad address"

  let path_to_string (i,alt,add) =
    let alt=
      match alt with
      | None -> "same tree"
      | Some i -> Printf.sprintf "tree %d" i in
    Printf.sprintf "(-%d,%s,%s)" i alt (address_to_string add)


  type 'a stack='a list
  type 'a list_context ='a stack

  type 'a focused_list = 'a list_context * 'a list


  type 'a alternatives = 'a tree focused_list
  and 'a tree = Node of 'a * 'a child list
  and 'a child = 
  | Forest of 'a alternatives
  | Link_to of relative_path
  and 'a alt_tree_zipper = 
  | Top of ('a tree) focused_list
  | Zip of 'a * ('a child) focused_list * ('a tree) focused_list * 'a alt_tree_zipper * 'a alt_tree_zipper option
  (* The last argument is a local context when the current tree
     was reached after a Link_to move *)


  type 'a simple_tree = SimpleTree of 'a * 'a simple_tree list



  let rec fold_depth_first ((transform,apply) as f) t =
    match t with
    | SimpleTree (v,[]) -> transform v
    | SimpleTree (v,children) -> 
      List.fold_left
	(fun acc child -> apply acc (fold_depth_first f child))
	(transform v)
	children



      
  type 'a focused_alt_tree = 'a alt_tree_zipper * 'a  tree

  type 'a zipper = 
  | ZTop
  | Zipper of ('a * 'a simple_tree focused_list * 'a zipper)

  type 'a focused_tree = 'a zipper * 'a simple_tree


  type move =
  | Up
  | Down
  | Right
  | Forward
  | Backward

  exception Move_failure of move
  exception Not_well_defined
  exception No_next_alt
  exception Bad_argument
  exception Bad_address

  let rec unstack = function
    | [],l ->l
    | a::tl,l -> unstack (tl,(a::l))


  let rec unstack_rev = function
    | [],l ->l
    | a::tl,l -> unstack_rev (tl,((List.rev a)::l))



  let f_list_forward = function
    | _,[] -> raise (Move_failure Forward)
    | s,a::tl -> a::s,tl

  let f_list_backward = function
    | [],_ -> raise (Move_failure Backward)
    | a::s,l-> s,a::l

  let rec f_list_up = function
    | [],l -> [],l
    | a::s,l-> f_list_up (s,a::l)

  let rec f_list_down = function
    | s,[] -> s,[]
    | s,a::tl -> f_list_down (a::s,tl)


  let rec f_list_fold (p,n) f acc =
    match n with
    | [] -> (p,[]),acc
    | a::tl -> f_list_fold (a::p,tl) f (f (p,tl) a acc)

  let f_list_fold_and_hd (p,n) f acc =
    match n with
    | [] -> raise Bad_argument
    | [a] -> f (p,[]) a,(p,[]),acc
    | a::tl ->
      let head = f (p,tl) a in
      let ctxt,res = f_list_fold (a::p,tl) (fun ctxt elt l_acc -> (f ctxt elt)::l_acc) acc in
      head,ctxt,res
	



  let f_tree_up = function
    | ZTop,t -> raise (Move_failure Up)
    | Zipper (v,(l,r),z'),t -> z',SimpleTree (v,unstack (l,t::r))




  let rec zip_up_aux f_tree = 
    try
      zip_up_aux (f_tree_up f_tree)
    with
    | Move_failure Up -> f_tree

  let zip_up f_tree =
    let _,t = zip_up_aux f_tree in
    t

  let rec move_forward i (l,r) =
    match i,r with
    | 1,a::tl -> (l,tl),a
    | i,a::tl when i>1 -> move_forward (i-1) (a::l,tl)
    | _ -> raise Bad_address
      
  let rec enter addr (z,(Node (v,children) as t)) =
    LOG "Entering \"%s\" on a node with %d children%!" (address_to_string addr) (List.length children) LEVEL DEBUG;
    match addr with
    | [],None ->
      (match z with
      | Top (p,n) -> None,([],[]),f_list_up (p,t::n)
      | _ -> failwith "Bug: entering a non top forest with an empty address")
    | _,None -> raise Not_well_defined
    | [],Some i -> 
      let (l,r),c = move_forward i ([],children) in
      (match c with
      | Forest (p,n) -> Some (z,t),(l,r),(p,n)
      | Link_to (back,alt,add) -> forest_at (back-1,alt,add) (z,t))
    | (i_child,j_alt)::tl,Some c ->
      let (l,r),forest=move_forward i_child ([],children) in
      (match forest with
      | Forest (p,n) -> 
	let (p,n),t = move_forward j_alt (f_list_up (p,n)) in
	enter (tl,Some c) (Zip(v,(l,r),(p,n),z,None),t)
      | Link_to (back,alt,add) -> 
	(match forest_at (back-1,alt,add) (z,t) with
	| None,(l',r'),f ->
	  let (p'',n''),t''= move_forward j_alt (f_list_up f) in
	  enter (tl,Some c) (Zip (v,(l,r),(p'',n''),z,Some (Top (p'',n''))),t'')
	| Some (z',Node(v',_)),(l',r'),f ->
	  let (p'',n''),t''= move_forward j_alt (f_list_up f) in
	  let local_context=Zip(v',(l',r'),(p'',n''),z',None) in
	  enter (tl,Some c) (Zip (v,(l,r),(p'',n''),z',Some local_context),t'')))
  and forest_at (back,alt,addr) (z,(Node (_,children) as t)) = 
    LOG "Look for forest at path %s\n%!" (path_to_string (back,alt,addr)) LEVEL DEBUG;
    LOG "current focused tree has %d children\n%!" (List.length children) LEVEL DEBUG;
    match z,t with
    | Top _ ,_ when back>0 -> raise (Move_failure Up)
    | Top (p,n) as z,t when back=0 ->
      (match alt with
      | None -> enter addr (z,t)
      | Some j_alt -> 
	let (p,n),t = move_forward j_alt ([],unstack (p,t::n)) in
	enter addr (Top (p,n),t))
    | z,t when back=0 -> enter addr (z,t)
    | Zip (v,(l,r),(p,n),z',None),t -> 
      let children=unstack (l,(Forest (p,t::n))::r) in
      forest_at (back-1,alt,addr) (z',Node (v,children))      
    | Zip (_,_,(p,n),_,Some local_context),t -> 
      (match local_context with
      | Top (p,n) -> failwith "want to move back on a top context"
      | Zip (v,(l,r),(p,n),z',_) ->
	let children=unstack (l,(Forest (p,t::n))::r) in
	forest_at (back-1,alt,addr) (z',Node (v,children)))
    | _,_ -> raise Bad_address
      
  let set_local_context l_ctxt = function
    | Top _ -> raise Bad_argument
    | Zip (v,sibling,alt,z,_) -> Zip (v,sibling,alt,z,Some l_ctxt) 

(*  let rec down_in_tree (z,t) =
    match t with
    | Node (_,[]) -> raise  (Move_failure Down)
    | Node (v,a::tl) -> 
      (match a with
      | Forest ([],[]) -> raise Not_well_defined
      | Forest (_,[]) -> raise  (Move_failure Down)
      | Forest (p,f_t::n) -> Zip (v,([],tl),(p,n),z,None),f_t
      | Link_to path -> 
	let z',t' = tree_at path (z,t) in
	let z = set_local_context z' z in
	down_in_tree (z,t'))
*)
    
(*  let up_in_tree (z,t) =
    match z with
    | Top _ -> raise (Move_failure Up)
    | Zip (v,(left,right),(p,n),z',_) -> 
      let alt= (p,t::n) in
      let children = unstack (left,alt::right) in
      z',Tree (Node (v,children))
*)
	
(*  let right_in_tree (z,t) =
    match z with 
    | Top _ -> raise (Move_failure Right)
    | Zip(_,(_,[]),_,_,_) -> raise (Move_failure Right)
    | Zip(v,(l,a::r),alt,z',local_context) -> 
      (match a with
      | ([],[]) -> raise Not_well_defined
      | (_,[]) -> raise (Move_failure Right)
      | (p,f_t::n) -> Zip(v,(alt::l,r),(p,n),z',local_context),f_t)
*)
	
(*  let next_alt_forward (z,t) =
    match z with
    | Top (_,[]) -> raise No_next_alt
    | Top (p,a::tl) ->  Top (t::p,tl),a
    | Zip (_,(_,_),(_,[]),_,_) -> raise No_next_alt
    | Zip (v,(l,r),(p,a::n),z',local_context) -> Zip (v,(l,r),(t::p,n),z',local_context),a
*)    

(*  let rec fold_on_children_aux (l,r) f acc =
    match r with
    | [] -> acc,l
    | a::tl ->
      (match a with
      | (_,[]) -> raise No_next_alt
      | (p,t::n) -> 
	let acc',t'=f t acc in
	fold_on_children_aux ((t'::p,n)::l,tl) f acc')
	
  let rec extract_tree t  =
    match t with
    | Tree (Node (v,children)) ->
      let tree_children_rev,alt_tree_children_rev=
	fold_on_children_aux
	  ([],children)
	  (fun t acc -> 
	    let tree,alt_tree= extract_tree t in
	    tree::acc,alt_tree)
	  [] in
      SimpleTree(v,List.rev tree_children_rev),Tree (Node(v,List.rev alt_tree_children_rev))
    | Link_to path -> failwith "Not possible to implement: missing context arg"
*)
    
  let simple_tree (Node (v,_)) = SimpleTree (v,[])
      

  let dispatch f acc = function
    | [] -> raise Bad_argument
    | a::tl -> f a,List.fold_left f acc tl
      
      
(*  let actual_tree (z,t) = 
    match t with
    | Tree _ as t -> z,t
    | Link_to path -> 
      let z',t'=tree_at path (z,t) in
      set_local_context z' z,t'
*)

  let actual_forest path (z,t)=
    let local_context,_,(p,n) = forest_at path (z,t) in
    let l_ctxt=
      match local_context with
      | None -> None
      | Some (z',_) -> Some z' in
    l_ctxt,(p,n)

  let rec down (z,t) (zipper,b_t) resume =
    match t with
    | Node (_,[]) -> raise (Move_failure Down)
    | Node (v,forest::tl) ->
      (match forest with
      | Link_to (back,alt,add) ->
	let z'',f =  actual_forest (back-1,alt,add) (z,t) in
	let (f_forest,f_tree),_,resume =
	  f_list_fold_and_hd
	    f
	    (fun (p,n) t' ->
	      let z'=Zip (v,([],tl),(p,n),z,z'') in
	      (z',t'),
	      (Zipper(v,([],[]),zipper),
	       simple_tree t'))
	    resume in
	f_forest,f_tree,resume
      | Forest ([],[]) -> raise Not_well_defined
      | Forest (_,[]) -> raise No_next_alt
      | Forest f -> 
	let (f_forest,f_tree),_,resume =
	  f_list_fold_and_hd
	    f
	    (fun (p,n) t' ->
	      let z'=Zip (v,([],tl),(p,n),z,None) in
	      (*	      let z',t'= actual_tree (z',t') in *)
	      (z',t'),
	      (Zipper(v,([],[]),zipper),
	       simple_tree t'))
	    resume in
	f_forest,f_tree,resume)
      
  let right (z,t) (zipper,b_t) resume =
    match z,zipper with
    | _ ,ZTop -> raise (Move_failure Right)
    | Top _,_ ->  raise (Move_failure Right)
    | Zip (v,(_,[]),_,_,_), Zipper(v',_,_) when v=v'-> raise (Move_failure Right)
    | Zip (v,(l,a::r),(p,n),z',_), Zipper(v',(l',r'),z'') when v=v'->
      let l_c,f =
	match a with
	| Forest f -> None,f
	| Link_to (0,alt,add) -> actual_forest (0,alt,add)  (z',Node(v,unstack (l,(Forest(p,t::n))::a::r)))
	| Link_to (back,alt,add) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(p,t::n))::a::r)) in
	  actual_forest (back-1,alt,add) new_ctx in
      let (f_forest,f_tree),_,resume =
	f_list_fold_and_hd
	  f
	  (fun (p',n') t' ->
	    let z'=Zip (v,((Forest(p,t::n))::l,r),(p',n'),z',l_c) in
	    (z',t'),
	    (Zipper(v',(b_t::l',r'),z''),simple_tree t'))
	  resume in
      f_forest,f_tree,resume
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"
      

  let up (z,t) (zipper,b_t) =
    match z,zipper with
    | Top _,ZTop -> raise (Move_failure Up)
    | _,ZTop -> failwith "Bug: both forest and tree context should be top"
    | Top _,_ ->  failwith "Bug: both forest and tree context should be top"
    | Zip (v,(l,r),(p,n),z',_),Zipper(v',_,_) when v=v' -> 
      (z',Node (v,unstack (l,(Forest(p,t::n))::r))),
      f_tree_up (zipper,b_t)
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"


  let rec close_forest_context_up f_forest f_tree resume =
    let f_forest,f_tree = up f_forest f_tree in
    try
      right f_forest f_tree resume
    with 
    | Move_failure Right -> 
      (try
	 close_forest_context_up f_forest f_tree resume
       with
       | Move_failure Up -> f_forest,f_tree,resume)
	
	
  let rec build_tree_aux f_forest f_tree resume =
    try
      LOG "Trying to go down" LEVEL DEBUG;
      let f_forest,f_tree,resume = down f_forest f_tree resume in
      LOG "Succeeded" LEVEL DEBUG;
      build_tree_aux f_forest f_tree resume
    with
    | Move_failure Down ->
      (try
	 LOG "Trying to go right" LEVEL DEBUG;
	 let f_forest,f_tree,resume = right f_forest f_tree resume in
	 LOG "Succeeded" LEVEL DEBUG;
	 build_tree_aux f_forest f_tree resume
       with
       | Move_failure Right ->
	 LOG "Trying to close up" LEVEL DEBUG;
	 (match close_forest_context_up f_forest f_tree resume with
	 | ((Top _ ,_),(ZTop,_),_) as res -> 
	   LOG "Succeeded" LEVEL DEBUG;
	   res
	 | (Zip _,_) as l_f_forest,((Zipper _,_) as l_f_tree),resume' -> 
	   LOG "Succeeded" LEVEL DEBUG;
	   LOG "Trying to restart a building" LEVEL DEBUG;
	   build_tree_aux l_f_forest l_f_tree resume'
	 | _ -> failwith "Bug: not representing the same tree"))
	
  let build_tree f_forest f_tree resume = build_tree_aux f_forest f_tree resume

  let rec build_trees_aux f_forest f_tree resume acc =
    let _,(_,tree),resume = build_tree f_forest f_tree resume in
    match resume with
    | [] -> tree::acc
    | (f_forest,f_tree)::tl -> build_trees_aux f_forest f_tree tl (tree::acc)

      

  let init = function
    | [] -> raise Not_well_defined
    | alt_trees ->
      snd (f_list_fold
	     ([],alt_trees) 
	     (fun (p,n) t acc -> ((Top (p,n),t),(ZTop,simple_tree t))::acc)
	     [])

  let build_trees forest =
    match init forest with
    | [] -> failwith "Bug"
    | (f_forest,f_tree)::resume -> build_trees_aux f_forest f_tree resume []

  let resumption = function
    | [] -> None,[]
    | (f_forest,f_tree)::resume -> 
      LOG "Building a tree from a forest" LEVEL DEBUG;
      LOG "It remains %d elements in the resumption list" (List.length resume) LEVEL DEBUG;
      let _,(_,tree),resume=build_tree f_forest f_tree resume in
      Some tree,resume

end


