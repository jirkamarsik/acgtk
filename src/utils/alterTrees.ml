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
  | Zip of 'a * ('a child) focused_list * ('a tree) focused_list * int * 'a alt_tree_zipper * 'a alt_tree_zipper option * address
  (* the int argument after the focused list correspond to the
     position of the focused tree in the list. It is useful to check
     wheter an child point to one of its ancestor *)
  (* The last argument is a local context when the current tree was
     reached after a Link_to move *)


  type 'a simple_tree = SimpleTree of 'a * 'a simple_tree list


  let forest_address = function
    | Top _ -> []
    | Zip (_,_,_,_,_,_,add) -> add

  let tree_address = function
    | Top ((_,_),i) -> i,[]
    | Zip (_,_,_,i,_,_,add) -> i,add



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




  type 'a simple_resumption = ('a focused_alt_tree * 'a focused_tree * int) list

  type 'a delayed_resumption = ('a simple_resumption) Utils.IntMap.t

  type 'a resumption = 'a simple_resumption * 'a delayed_resumption


  exception Infinite_loop

  let extend_simple_resume (f_f,f_t,i) resume = (f_f,f_t,i)::resume


  let extend_sized_indexed_resume (f_f,f_t,i) resume =
    try
      Utils.IntMap.add i ((f_f,f_t,i)::(Utils.IntMap.find i resume)) resume
    with
    | Not_found -> Utils.IntMap.add i [(f_f,f_t,i)] resume

  let extend_resume ?actual ?delayed ((resume1,resume2):'a resumption) =
    match actual,delayed with
    | None,None -> resume1,resume2
    | Some v,None -> extend_simple_resume v resume1,resume2
    | None,Some v -> resume1,extend_sized_indexed_resume v resume2
    | Some v1,Some v2->extend_simple_resume v1 resume1,extend_sized_indexed_resume v2 resume2

  type move =
  | Up
  | Down
  | Right
  | Forward
  | Backward
  | Cycle

  exception Move_failure of move
  exception Not_well_defined
  exception No_next_alt
  exception No_previous_alt
  exception Bad_argument
  exception Bad_address

  let swap = function
    | [],delayed ->
      (try
	 (match Utils.IntMap.min_binding delayed with
	 | _,[] -> failwith "Bug: such a binding should be removed"
	 | i,[a] -> a,([], Utils.IntMap.remove i delayed)
	 | i,a::res -> a,(res,Utils.IntMap.remove i delayed))
       with
       | Not_found -> raise No_next_alt)
    | a::actual,delayed -> a,(actual,delayed)


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

  let f_list_cycle = function
    | [],[] -> raise (Move_failure Cycle)
    | p,a::n -> a::p,n
    | f_lst -> f_list_up f_lst


  let focus_of = function
    | [],[] -> raise Not_well_defined
    | p,a::n -> (p,n),a,1+List.length p
    | p,[] -> 
      match List.rev p with
      | [] -> raise Not_well_defined
      | a::n ->  ([],n),a,1


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
    IFDEF BOLT THEN
    LOG "Entering \"%s\" on a node with %d children%!" (address_to_string addr) (List.length children) LEVEL DEBUG;
    END;
    match addr with
    | [] -> 
	(match z with
	| Top (([],[]),_) -> (z,t),([],[t])
	| Top ((p,[]),_) ->
	  (match unstack (p,[t]) with
	  | [] -> raise Not_well_defined
	  | a::n -> (Top (([],n),1),a),([],a::n))
	| Top ((p,a::n),i) -> (Top ((t::p,n),i+1),a),(t::p,a::n)
	| Zip (_,_,([],[]),_,_,_,_) -> (z,t),([],[t])
	| Zip (v,sibling,(p,[]),i,z',l_c,add) -> 
	  (match unstack (p,[t]) with
	  | [] -> raise Not_well_defined
	  | a::n -> (Zip (v,sibling,([],n),1,z',l_c,add),a),([],a::n))
	| Zip (v,sibling,(p,a::n),i,z',l_c,add) -> 
	  (Zip (v,sibling,(t::p,n),i+1,z',l_c,add),a),(t::p,a::n))
    | (j_alt,i_child)::tl ->
      let z,Node(v',children')=
	match z with
	| Top ((p,n),_) -> 
	  let (p,n),t'= move_forward j_alt (f_list_up (p,t::n)) in
	  Top ((p,n),j_alt),t'
	| Zip (v',(l,r),(p,n),_,z',l_c,add) ->
	  let (p,n),t'=move_forward j_alt (f_list_up (p,t::n)) in
	  Zip (v',(l,r),(p,n),j_alt,z',l_c,add),t' in
      let (l',r'),f_forest=move_forward i_child ([],children') in
      match f_forest with
      | Forest f ->
	let (p',n'), t'=move_forward 1 (f_list_up f) in
	enter tl (Zip(v',(l',r'),(p',n'),1,z,None,(j_alt,i_child)::(forest_address z)),t')
      | Link_to (back,addr) -> forest_at (back-1,addr) (z,Node(v',children'))
  and forest_at (back,addr) (z,(Node (_,children) as t)) = 
    IFDEF BOLT THEN
    LOG "Look for forest at path %s\n%!" (path_to_string (back,addr)) LEVEL DEBUG;
    LOG "current focused tree has %d children\n%!" (List.length children) LEVEL DEBUG;
    END;
    if back < 0 then
      failwith "Bug: looking for a forest with a negative back parameter"
    else
      match z,t with
      | Top _ ,_ when back>0 -> raise (Move_failure Up)
      | _,_ when back=0 -> enter addr (z,t)
      | Zip (v,(l,r),(p,n),_,z',None,_),t -> 
	let children=unstack (l,(Forest (t::p,n))::r) in
	forest_at (back-1,addr) (z',Node (v,children))      
      | Zip (_,_,(p,n),_,_,Some local_context,_),t -> 
	(match local_context with
	| Top ((p,n),i) -> failwith "want to move back on a top context"
	| Zip (v,(l,r),(p,n),_,z',_,_) ->
	  let children=unstack (l,(Forest (t::p,n))::r) in
	  forest_at (back-1,addr) (z',Node (v,children)))
      | _,_ -> raise Bad_address
	
  let set_local_context l_ctxt = function
    | Top _ -> raise Bad_argument
    | Zip (v,sibling,alt,i,z,_,add) -> Zip (v,sibling,alt,i,z,Some l_ctxt,add) 

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
    | Top ((p,a::tl),i) ->  (Top ((t::p,tl),i+1),a)
    | Zip (_,(_,_),(_,[]),_,_,_,_) -> raise No_next_alt
    | Zip (v,(l,r),(p,a::n),i,z',local_context,add) -> 
      (Zip (v,(l,r),(t::p,n),i+1,z',local_context,add),a)

  let previous_alt (z,t) =
    match z with
    | Top (([],n),_) -> raise No_previous_alt
    | Top ((a::p,n),i) ->  (Top ((p,t::n),i-1),a)
    | Zip (_,(_,_),([],_),_,_,_,_) -> raise No_previous_alt
    | Zip (v,(l,r),(a::p,n),i,z',local_context,add) -> 
      (Zip (v,(l,r),(p,t::n),i-1,z',local_context,add),a)

	
  let rec get_all_next_alt_aux (z,t) acc =
    try
      let alt= next_alt (z,t) in
      get_all_next_alt_aux alt (alt::acc)
    with
    | No_next_alt -> acc

  let rec get_all_previous_alt_aux (z,t) acc =
    try
      let alt= previous_alt (z,t) in
      get_all_previous_alt_aux alt (alt::acc)
    with
    | No_previous_alt -> acc


  let get_all_alt (z,t) acc =
    let acc = get_all_next_alt_aux (z,t) acc in
    let acc = get_all_previous_alt_aux (z,t) acc in
    List.rev acc
      
  let simple_tree (Node (v,_)) = SimpleTree (v,[])
      
  let actual_forest path (z,t)=
    let (z',t'),f = forest_at path (z,t) in
    (z',t'),f



  let rec down (z,t) (zipper,b_t) depth resume=
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
	let foc_forest = Zip (v,([],tl),(p,n),1+List.length p,z,Some z'',forest_address z''),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	(match add with
	| [] ->
	  let resume =
	    let all_alt = get_all_alt foc_forest [foc_forest] in
	    List.fold_left
	      (fun acc (z,t) ->
		extend_resume ~delayed:((z,t),(zipper,simple_tree t),depth+1) acc)
	      resume
	      all_alt in
	  let (foc_forest,foc_tree,depth'),resume = swap resume in
	  (*	    match resume with
		    | [],[] -> failwith "Bug"
		    | [],a::tl -> a,([],tl)
		    | a::tl,r2 -> a,(tl,r2) in*)
	  foc_forest,foc_tree,depth',resume
	(*	  (try
		  let (foc_forest,foc_tree),resume = swap (extend_resume ~delayed:(foc_forest,foc_tree) resume) in
		  foc_forest,foc_tree,resume
		  with
		  | Infinite_loop -> foc_forest,foc_tree,resume) *)
	| _ ->
	  let resume =
	    let all_alt = get_all_alt foc_forest [] in
	    List.fold_left
	      (fun acc (z,t) ->
		extend_resume ~actual:((z,t),(zipper,simple_tree t),depth+1) acc)
	      resume
	      all_alt in
	  foc_forest,foc_tree,depth+1,resume)
      | Forest ([],[]) -> raise Not_well_defined
(*      | Forest (_,[]) -> raise No_next_alt *)
      | Forest l_f ->
	let t_alt,add=tree_address z in
	let (p,n),a,pos=focus_of l_f in
	let foc_forest=Zip (v,([],tl),(p,n),pos,z,None,(t_alt,1)::add),a in
	let zipper=Zipper(v,([],[]),zipper) in
	let foc_tree=zipper,simple_tree a in
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) -> extend_resume ~actual:((z,t),(zipper,simple_tree t),depth+1) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,depth+1,resume)
	
  let right (z,t) (zipper,b_t) depth resume =
    match z,zipper with
    | _ ,ZTop -> raise (Move_failure Right)
    | Top _,_ ->  raise (Move_failure Right)
    | Zip (v,(_,[]),_,_,_,_,_), Zipper(v',_,_) when v=v'-> raise (Move_failure Right)
    | Zip (v,(l,a::r),(p,n),i,z',_,add), Zipper(v',(l',r'),z'') when v=v'->
      let l_c,f,loop =
	match a with
	| Forest f -> None,f_list_up f,false
	| Link_to (back,[]) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(t::p,n))::a::r)) in
	  let (z,_),f=actual_forest (back-1,[]) new_ctx in
	  Some z,f,true
	| Link_to (back,add) -> 
	  let new_ctx = z', Node(v,unstack (l,(Forest(t::p,n))::a::r)) in
	  let (z,_),f=actual_forest (back-1,add) new_ctx in
	  Some z,f,false in
      let (p',n'),t' = 
	match f with
	| _,[] -> raise Bad_address
	| p,a::n -> (p,n),a in
      let foc_forest = Zip (v,((Forest(p,t::n))::l,r),(p',n'),1,z',l_c,add),t' in
      let zipper= Zipper(v',(b_t::l',r'),z'') in
      let foc_tree=zipper,simple_tree t' in
      (match loop with
      | true -> 
	let resume =
	  let all_alt = get_all_alt foc_forest [foc_forest] in
	  List.fold_left
	    (fun acc (z,t) ->
	      extend_resume ~delayed:((z,t),(zipper,simple_tree t),depth) acc)
	    resume
	    all_alt in
	let (foc_forest,foc_tree,depth'),resume = swap resume in
(*	  match resume with
	  | [],[] -> failwith "Bug"
	  | [],a::tl -> a,([],tl)
	  | a::tl,r2 -> a,(tl,r2) in *)
	foc_forest,foc_tree,depth',resume
      (*	  try
		  swap (extend_resume ~delayed:(foc_forest,foc_tree) resume)
	  with
	  | Infinite_loop -> (foc_forest,foc_tree),resume in
	foc_forest,foc_tree,resume *)
      | false ->
	let resume =
	  let all_alt = get_all_alt foc_forest [] in
	  List.fold_left
	    (fun acc (z,t) ->
	      extend_resume ~actual:((z,t),(zipper,simple_tree t),depth) acc)
	    resume
	    all_alt in
	foc_forest,foc_tree,depth,resume)
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"
      

  let up (z,t) (zipper,b_t) depth =
    match z,zipper with
    | Top _,ZTop -> raise (Move_failure Up)
    | _,ZTop -> failwith "Bug: both forest and tree context should be top"
    | Top _,_ ->  failwith "Bug: both forest and tree context should be top"
    | Zip (v,(l,r),(p,n),_,z',_,_),Zipper(v',_,_) when v=v' -> 
      (z',Node (v,unstack (l,(Forest(p,t::n))::r))),
      f_tree_up (zipper,b_t),
      depth-1
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"


  let rec close_forest_context_up f_forest f_tree depth resume =
    let f_forest,f_tree,depth = up f_forest f_tree depth in
    try
      right f_forest f_tree depth resume
    with 
    | Move_failure Right -> 
      (try
	 close_forest_context_up f_forest f_tree depth resume
       with
       | Move_failure Up -> f_forest,f_tree,depth,resume)
	
	
  let rec build_tree_aux f_forest f_tree depth resume=
    try
      IFDEF BOLT THEN
      LOG "Trying to go down" LEVEL DEBUG;
      END;
      let f_forest,f_tree,depth,resume = down f_forest f_tree depth resume in
      IFDEF BOLT THEN
      LOG "Succeeded" LEVEL DEBUG;
      END;
      build_tree_aux f_forest f_tree depth resume
    with
    | Move_failure Down ->
      (try
	 IFDEF BOLT THEN
	 LOG "Trying to go right" LEVEL DEBUG;
	 END;
	 let f_forest,f_tree,depth,resume = right f_forest f_tree depth resume in
	 IFDEF BOLT THEN
	 LOG "Succeeded" LEVEL DEBUG;
	 END;
	 build_tree_aux f_forest f_tree depth resume
       with
       | Move_failure Right ->
	 IFDEF BOLT THEN
	 LOG "Trying to close up" LEVEL DEBUG;
	 END;
	 (match close_forest_context_up f_forest f_tree depth resume with
	 | ((Top _ ,_),(ZTop,_),_,_) as res -> 
	   IFDEF BOLT THEN
	   LOG "Succeeded" LEVEL DEBUG;
	   END;
	   res
	 | (Zip _,_) as l_f_forest,((Zipper _,_) as l_f_tree),depth',resume' -> 
	   IFDEF BOLT THEN
	   LOG "Succeeded" LEVEL DEBUG;
	   LOG "Trying to restart a building" LEVEL DEBUG;
	   END;
	   build_tree_aux l_f_forest l_f_tree depth' resume'
	 | _ -> failwith "Bug: not representing the same tree"))
	
  let build_tree f_forest f_tree depth resume = build_tree_aux f_forest f_tree depth resume

  let rec build_trees_aux f_forest f_tree depth resume acc =
    let _,(_,tree),_,resume = build_tree f_forest f_tree depth resume in
    try
      let (f_forest,f_tree,depth),resume = swap resume in
      build_trees_aux f_forest f_tree depth resume (tree::acc)
    with
    | No_next_alt -> tree::acc

      

  let init = function
    | [] -> raise Not_well_defined
    | alt_trees ->
      (snd (f_list_fold
	     (([],alt_trees),1)
	     (fun ((p,n),i) t acc -> ((Top ((p,n),i),t),(ZTop,simple_tree t),1)::acc)
	     []))

  let build_trees forest =
    match init forest with
    | [] -> failwith "Bug"
    | (f_forest,f_tree,depth)::resume -> 
      let res = build_trees_aux f_forest f_tree depth (resume,Utils.IntMap.empty) [] in
      res

  let resumption (res) = 
    match res with
    | [],_ ->
      (try
	 let (f_forest,f_tree,depth),resume=swap res in
	 let _,(_,tree),_,res'=build_tree f_forest f_tree depth resume in
	 Some tree,(res')
       with
       | No_next_alt -> None,res)
    | (f_forest,f_tree,depth)::resume,delayed -> 
      let _,(_,tree),_,res'=build_tree f_forest f_tree depth (resume,delayed) in
      Some tree,(res')

end


