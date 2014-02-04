module AlternTrees =
struct
  type address=(int*int) list
  (* (position in the forest,child position) *)
  type relative_path=int*address
  (* the 2nd argument is to move in the alternative trees at the top
     of the forest *)

  let rec diff_aux add1 add2 back =
    match add1,add2 with
    | [],[] -> back,[]
    | _,[] -> back+List.length add1,[]
    | [],_ -> back,add2
    | (i,j)::tl1,(i',j')::tl2 when i=i' && j=j' -> diff_aux tl1 tl2 back
    | _::_,_::_ -> back+List.length add1,add2

  let diff add1 add2 = diff_aux add1 add2 0

  let address_to_string addr = 
    Printf.sprintf "[%s]" (Utils.string_of_list ";" (fun (i,j) -> Printf.sprintf "(%d,%d)" i j) addr)

  let path_to_string (i,add) =
    Printf.sprintf "(-%d,%s)" i (address_to_string add)


  type 'a stack='a list
  type 'a list_context ='a stack

  type 'a focused_list = 'a list_context * 'a list


  type 'a alternatives = 'a tree focused_list
  and 'a tree = Node of 'a * 'a child list
  and 'a child = 
  | Forest of 'a alternatives
  | Link_to of relative_path
  and 'a alt_tree_zipper = 
  | Top of ('a tree) focused_list * int
  (* the last int argument correspond to the position of the focused
     tree in the list. It is useful to check wheter an child point to one
     of its ancestor *)
  | Zip of 'a * ('a child) focused_list * ('a tree) focused_list * int * 'a alt_tree_zipper * 'a alt_tree_zipper option
  (* the int argument after the focused list correspond to the
     position of the focused tree in the list. It is useful to check
     wheter an child point to one of its ancestor *)
  (* The last argument is a local context when the current tree was
     reached after a Link_to move *)


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


  type 'a resumption = ('a focused_alt_tree * 'a focused_tree) list


  let extend_resume (f_f,f_t) (resume:'a resumption) = (f_f,f_t)::resume

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
    | (_,[],_) -> raise (Move_failure Forward)
    | (s,a::tl,i) -> (a::s,tl),i+1

  let f_list_backward = function
    | ([],_),_ -> raise (Move_failure Backward)
    | (a::s,l),i-> (s,a::l),i-1

  let rec f_list_up = function
    | [],l -> [],l
    | a::s,l-> f_list_up (s,a::l)

  let rec f_list_down = function
    | s,[] -> s,[]
    | s,a::tl -> f_list_down (a::s,tl)


  let rec f_list_fold ((p,n),i) f acc =
    match n with
    | [] -> (p,[]),acc
    | a::tl -> f_list_fold ((a::p,tl),i+1) f (f ((p,tl),i) a acc)

  let f_list_fold_and_hd (p,n) f acc =
    match n with
    | [] -> raise Bad_argument
    | [a] -> f ((p,[]),1) a,(p,[]),acc
    | a::tl ->
      let head = f ((p,tl),1) a in
      let ctxt,res = 
	f_list_fold
	  ((a::p,tl),2)
	  (fun ctxt elt l_acc -> (f ctxt elt)::l_acc) acc in
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

  (* invariant: the result is [(z,t),forest] where [t] belongs to the
     forest [forest]. [t] is not necessarily the first element of the
     forest *)
      
  let rec enter addr (z,(Node (v,children) as t)) =
    LOG "Entering \"%s\" on a node with %d children%!" (address_to_string addr) (List.length children) LEVEL DEBUG;
    match addr with
    | [] -> 
      let forest =
	match z with
	| Top ((p,n),_) -> unstack (p,t::n)
	| Zip (_,_,(p,n),_,_,_) -> unstack (p,t::n) in
      (z,t),forest
    | (j_alt,i_child)::tl ->
      let z,Node(v',children')=
	match z with
	| Top ((p,n),_) -> 
	  let (p,n),t'= move_forward j_alt (f_list_up (p,t::n)) in
	  Top ((p,n),j_alt),t'
	| Zip (v',(l,r),(p,n),_,z',l_c) ->
	  let (p,n),t'=move_forward j_alt (f_list_up (p,t::n)) in
	  Zip (v',(l,r),(p,n),j_alt,z',l_c),t' in
      let (l',r'),f_forest=move_forward i_child ([],children') in
      match f_forest with
      | Forest f ->
	let (p',n'), t'=move_forward 1 (f_list_up f) in
	enter tl (Zip(v',(l',r'),(p',n'),1,z,None),t')
      | Link_to (back,addr) -> forest_at (back-1,addr) (z,Node(v',children'))
  and forest_at (back,addr) (z,(Node (_,children) as t)) = 
    LOG "Look for forest at path %s\n%!" (path_to_string (back,addr)) LEVEL DEBUG;
    LOG "current focused tree has %d children\n%!" (List.length children) LEVEL DEBUG;
    if back < 0 then
      failwith "Bug: looking for a forest with a negative back parameter"
    else
      match z,t with
      | Top _ ,_ when back>0 -> raise (Move_failure Up)
      | _,_ when back=0 -> enter addr (z,t)
      | Zip (v,(l,r),(p,n),_,z',None),t -> 
	let children=unstack (l,(Forest (p,t::n))::r) in
	forest_at (back-1,addr) (z',Node (v,children))      
      | Zip (_,_,(p,n),_,_,Some local_context),t -> 
	(match local_context with
      (*      | Top ((p,n),i) when back=1-> forest_at (0,alt,addr) (Top((p,n),i),t) *)
	| Top ((p,n),i) -> failwith "want to move back on a top context"
	| Zip (v,(l,r),(p,n),_,z',_) ->
	  let children=unstack (l,(Forest (p,t::n))::r) in
	  forest_at (back-1,addr) (z',Node (v,children)))
      | _,_ -> raise Bad_address
	
  let set_local_context l_ctxt = function
    | Top _ -> raise Bad_argument
    | Zip (v,sibling,alt,i,z,_) -> Zip (v,sibling,alt,i,z,Some l_ctxt) 

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
	
  let next_alt (z,t) =
    match z with
    | Top ((_,[]),_) -> raise No_next_alt
    | Top ((p,a::tl),i) ->  Top ((t::p,tl),i+1),a
    | Zip (_,(_,_),(_,[]),_,_,_) -> raise No_next_alt
    | Zip (v,(l,r),(p,a::n),i,z',local_context) -> 
      Zip (v,(l,r),(t::p,n),i+1,z',local_context),a
    

  let rec get_all_alt (z,t) acc =
    try
      let alt= next_alt (z,t) in
      get_all_alt alt (alt::acc)
    with
    | No_next_alt -> acc
    
  let simple_tree (Node (v,_)) = SimpleTree (v,[])
      
  let actual_forest path (z,t)=
    let (z',t'),f = forest_at path (z,t) in
    (z',t'),([],f)

  let rec down (z,t) (zipper,b_t) resume =
    match t with
    | Node (_,[]) -> raise (Move_failure Down)
    | Node (v,forest::tl) ->
      (match forest with
      | Link_to (back,add) ->
	let (z'',_),f =  actual_forest (back-1,add) (z,t) in
	let (p,n),a = 
	  match f with
	  | _,[] -> raise Bad_address
	  | p,a::n -> (p,n),a in
	let foc_forest = Zip (v,([],tl),(p,n),1,z,Some z''),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) ->
	      extend_resume ((z,t),(zipper,simple_tree t)) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,resume
      | Forest ([],[]) -> raise Not_well_defined
      | Forest (_,[]) -> raise No_next_alt
      |	Forest (p,a::n) ->
	let foc_forest=Zip (v,([],tl),(p,n),1,z,None),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) -> extend_resume ((z,t),(zipper,simple_tree t)) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,resume)
      
  let right (z,t) (zipper,b_t) resume =
    match z,zipper with
    | _ ,ZTop -> raise (Move_failure Right)
    | Top _,_ ->  raise (Move_failure Right)
    | Zip (v,(_,[]),_,_,_,_), Zipper(v',_,_) when v=v'-> raise (Move_failure Right)
    | Zip (v,(l,a::r),(p,n),i,z',_), Zipper(v',(l',r'),z'') when v=v'->
      let l_c,f =
	match a with
	| Forest f -> None,f
	| Link_to (back,add) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(p,t::n))::a::r)) in
	  let (z,_),f=actual_forest (back-1,add) new_ctx in
	  Some z,f in
      let (p',n'),t' = 
	match f with
	| _,[] -> raise Bad_address
	| p,a::n -> (p,n),a in
      let foc_forest = Zip (v,((Forest(p,t::n))::l,r),(p',n'),1,z',l_c),t' in
      let zipper= Zipper(v',(b_t::l',r'),z'') in
      let foc_tree=zipper,simple_tree t' in
      let resume =
	let all_alt = get_all_alt foc_forest [] in
	List.fold_left
	  (fun acc (z,t) ->
	    extend_resume ((z,t),(zipper,simple_tree t)) acc)
	  resume
	  all_alt in
      foc_forest,foc_tree,resume
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"
      

  let up (z,t) (zipper,b_t) =
    match z,zipper with
    | Top _,ZTop -> raise (Move_failure Up)
    | _,ZTop -> failwith "Bug: both forest and tree context should be top"
    | Top _,_ ->  failwith "Bug: both forest and tree context should be top"
    | Zip (v,(l,r),(p,n),_,z',_),Zipper(v',_,_) when v=v' -> 
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
	     (([],alt_trees),1)
	     (fun ((p,n),i) t acc -> ((Top ((p,n),i),t),(ZTop,simple_tree t))::acc)
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
(*      let resume =
	try
	  let f_forest = next_alt f_forest in
	  (f_forest,f_tree)::resume
	with
	| No_next_alt -> resume in *)
      Some tree,resume

end


