module AlternTrees =
struct
  type address=(int*int) list
  type relative_path=int*address

  type 'a stack='a list
  type 'a list_context ='a stack

  type 'a focused_list = 'a list_context * 'a list


  type 'a alternatives = 'a alt_tree focused_list
  and 'a alt_tree = Tree of 'a tree | Link_to of relative_path
  and 'a tree = Node of 'a * 'a children list
  and 'a children = 'a alternatives

  type 'a simple_tree = SimpleTree of 'a * 'a simple_tree list

  type 'a alt_tree_zipper = 
  | Top of ('a alt_tree) focused_list
  | Zip of 'a * ('a alt_tree focused_list) focused_list * ('a alt_tree) focused_list * 'a alt_tree_zipper
      
  type 'a focused_alt_tree = 'a alt_tree_zipper * 'a  alt_tree

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

(* 
  let rec enter addr (z,t) =
    match addr with
    | [] -> (z,t)
    | (i,j)::tl ->
      (match t with
      | Link_to path -> enter addr (tree_at path (z,t))
      | Tree (Node (v,children)) -> 
	let (l,r),alt=move_forward_rev (i-1) ([],children) in
	let (p,n),t=move_forward (j-1) ([],alt) in
	enter tl (Zip (v,(l,r),(p,n),z),t))
  and tree_at (back,addr) = function
    | Top _ ,_ when back>0 -> raise (Move_failure Up)
    | z,t when back=0 -> enter addr (z,t)
    | (Zip (v,(l,r),(p,n),z)),t -> 
      let _,f_l = f_list_up (p,t::n) in
      let children=unstack_rev (l,f_l::r) in
      tree_at (back-1,addr) (z,Tree (Node (v,children)))      
    | _,_ -> raise Bad_address
*)
    
  let down_in_tree (z,t) =
    match t with
    | Tree (Node (_,[])) -> raise  (Move_failure Down)
    | Tree (Node (v,a::tl)) -> 
      (match a with
      | ([],[]) -> raise Not_well_defined
      | (_,[]) -> raise  (Move_failure Down)
      | (p,f_t::n) -> Zip (v,([],tl),(p,n),z),f_t)
    | Link_to _ -> failwith "Not yet implemented"
      
  let up_in_tree (z,t) =
    match z with
    | Top _ -> raise (Move_failure Up)
    | Zip (v,(left,right),(p,n),z') -> 
      let alt= (p,t::n) in
      let children = unstack (left,alt::right) in
      z',Tree (Node (v,children))
	
  let right_in_tree (z,t) =
    match z with 
    | Top _ -> raise (Move_failure Right)
    | Zip(_,(_,[]),_,_) -> raise (Move_failure Right)
    | Zip(v,(l,a::r),alt,z') -> 
      (match a with
      | ([],[]) -> raise Not_well_defined
      | (_,[]) -> raise (Move_failure Right)
      | (p,f_t::n) -> Zip(v,(alt::l,r),(p,n),z'),f_t)
	
  let next_alt_forward (z,t) =
    match z with
    | Top (_,[]) -> raise No_next_alt
    | Top (p,a::tl) ->  Top (t::p,tl),a
    | Zip (_,(_,_),(_,[]),_) -> raise No_next_alt
    | Zip (v,(l,r),(p,a::n),z') -> Zip (v,(l,r),(t::p,n),z'),a
      

  let rec fold_on_children_aux (l,r) f acc =
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
    | Link_to _ -> failwith "Not yet implemented"
      
  let simple_tree = function
    | Tree (Node (v,_)) -> SimpleTree (v,[])
    | Link_to _ -> failwith "Not yet impemented"
      
  let down (z,t) (zipper,b_t) =
    match t with
    |  Tree (Node (_,[])) -> raise (Move_failure Down)
    |  Tree (Node (v,a::tl)) ->
      (match a with
      | [],[] -> raise Not_well_defined
      | _,[] -> raise No_next_alt
      | p,t'::n -> (Zip (v,([],tl),(p,n),z),t'),(Zipper(v,([],[]),zipper),simple_tree t'))
    | Link_to _ -> failwith "Not yet implemented"
	
  let right (z,t) (zipper,b_t) =
    match z,zipper with
    | _ ,ZTop -> raise (Move_failure Right)
    | Top _,_ -> raise (Move_failure Right)
    | Zip (v,(_,[]),_,_), Zipper(v',_,_) when v=v'-> raise (Move_failure Right)
    | Zip (v,(_,(_,[])::_),_,_), Zipper(v',_,_) when v=v'-> raise No_next_alt
    | Zip (v,(l,(p',t'::n')::r),(p,n),z'), Zipper(v',(l',r'),z'') when v=v'->
      (Zip (v,((p,t::n)::l,r),(p',n'),z'),t'), (Zipper(v',(b_t::l',r'),z''),simple_tree t')
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"
      

  let up (z,t) (zipper,b_t) =
    match z,zipper with
    | Top _,ZTop -> raise (Move_failure Up)
    | _,ZTop -> failwith "Bug: both forest and tree context should be top"
    | Top _,_ ->  failwith "Bug: both forest and tree context should be top"
    | Zip (v,(l,r),(p,n),z'),Zipper(v',_,_) when v=v' -> 
      (z',Tree (Node (v,unstack (l,(p,t::n)::r)))),
      f_tree_up (zipper,b_t)
    | _ -> failwith "Bug: alt_tree and Simpletree are not representing the same trees"



  let rec close_forest_context_up f_forest f_tree =
    let f_forest,f_tree = up f_forest f_tree in
    try
      right f_forest f_tree
    with 
    | Move_failure Right -> 
      (try
	 close_forest_context_up f_forest f_tree
       with
       | Move_failure Up -> f_forest,f_tree)
      
      
  let rec build_tree_aux f_forest f_tree =
    try
      let f_forest,f_tree = down f_forest f_tree in
      build_tree_aux f_forest f_tree
    with
    | Move_failure Down ->
      (try
	 let f_forest,f_tree = right f_forest f_tree in
	 build_tree_aux f_forest f_tree
       with
       | Move_failure Right ->
	 (match close_forest_context_up f_forest f_tree with
	 | ((Top _ ,_),(ZTop,_)) as res -> res
	 | (Zip _,_) as l_f_forest,((Zipper _,_) as l_f_tree) -> build_tree_aux l_f_forest l_f_tree
	 | _ -> failwith "Bug: not representing the same tree"))

  let build_tree f_forest f_tree = build_tree_aux f_forest f_tree
	

  let init = function
    | [] -> raise Not_well_defined
    | a::tl -> (Top ([],tl),a),(ZTop,simple_tree a)

end


