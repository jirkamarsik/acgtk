module AlternTrees =
struct
  type address=(int*int) list
  type relative_path=int*address

  type 'a alternatives = 'a alt_tree list
  and 'a alt_tree = Tree of 'a tree | Link_to of relative_path
  and 'a tree = Node of 'a * 'a children list
  and 'a children = 'a alternatives

  type 'a stack='a list
  type 'a list_context ='a stack

  type 'a focused_list = 'a list_context * 'a list

  exception Forward_failure
  exception Backward_failure
  exception Up_failure
  exception Down_failure
  exception Bad_address
  exception Not_well_defined

  let rec unstack = function
    | [],l ->l
    | a::tl,l -> unstack (tl,(a::l))


  let rec unstack_rev = function
    | [],l ->l
    | a::tl,l -> unstack_rev (tl,((List.rev a)::l))


  let f_list_forward = function
    | _,[] -> raise Forward_failure
    | s,a::tl -> a::s,tl

  let f_list_backward = function
    | [],_ -> raise Backward_failure
    | a::s,l-> s,a::l

  let rec f_list_up = function
    | [],l -> [],l
    | a::s,l-> f_list_up (s,a::l)

  let rec f_list_down = function
    | s,[] -> s,[]
    | s,a::tl -> f_list_down (a::s,tl)

  
  type 'a alt_tree_zipper = Top | Zip of 'a * ('a alternatives) focused_list * ('a alt_tree) focused_list * 'a alt_tree_zipper
(*  type 'a alt_tree_context = 'a tree_zipper * ('a alt_tree) focused_list *)
      
  type 'a focused_alt_tree = 'a alt_tree_zipper * 'a  alt_tree
    

  let rec move_forward n = function
    | (s,a::tl) when n=0 -> (s,tl),a
    | s,[] -> raise Bad_address
    | s,a::tl -> move_forward (n-1) (a::s,tl)

  let rec move_forward_rev n = function
    | (s,a::tl) when n=0 -> (s,tl),a
    | s,[] -> raise Bad_address
    | s,a::tl -> move_forward_rev (n-1) ((List.rev a)::s,tl)



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
    | Top,_ when back>0 -> raise Up_failure
    | z,t when back=0 -> enter addr (z,t)
    | (Zip (v,(l,r),(p,n),z)),t -> 
      let _,f_l = f_list_up (p,t::n) in
      let children=unstack_rev (l,f_l::r) in
      tree_at (back-1,addr) (z,Tree (Node (v,children)))      
    | _,_ -> raise Bad_address



  let rec forward = function
    | (z,(Link_to path)) as f_at-> 
      let _,t=forward (tree_at path f_at ) in
      z,t

    | Top as z,Tree (Node (v,children)) ->
      (match children with
      | [] -> raise Forward_failure
      | []::_ -> raise Not_well_defined
      | (a::d)::siblings -> Zip (v,([],siblings),([],d),z),a)
	
    | Zip (_,(l,[]),(s,[]),z) as z',Tree (Node (v,children)) ->
      (match children with
      | [] -> raise Forward_failure
      | []::_ -> raise Not_well_defined
      | (a::d)::siblings -> Zip (v,([],siblings),([],d),z'),a)
	
    | Zip (v,(l,next::r),(s,[]),z),f_t ->
      (match next with
      | [] -> raise Forward_failure
      | t::alts -> (Zip (v,((f_t::s)::l,r),([],alts),z)),t)

    | Zip (v,(l,r),(p,a::n),z),t -> Zip(v,(l,r),(t::p,n),z),a

(*
  let down (z,t) =
    match t with
    | Node (_,[]) -> raise Down_Failure
    | Node (_,a::tl) -> Zip ([],Focused_list.init z,tl),a

  let up (z,t) =
    match z with
    | Top -> raise Up_Failure
    | left,fz,right -> 
	(try 
	  let fz = Focused_list.backward fz in
	  
	| Focused_list.End_of_list -> fz in
      
	 


  let rec fold_aux t acc f resume =
    match t with
    | Node [] -> acc
    | Node (l,a::tl) ->
      match a with
      | Tree t ->  fold_aux t (f l acc) f
      | Link
*)      





end


