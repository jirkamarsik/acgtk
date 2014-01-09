module AlternTrees =
struct
  type address=int list
  type relative_path=int*addresse
  type 'a alternatives = 'a alt_tree list
  and 'a alt_tree = Tree of 'a tree | Link_to address
  and 'a tree = Node of 'a * 'a children list
  and 'a children = 'a alternatives

  type 'a stack='a list

  type 'a list_context ='a stack

  type 'a focused_list = 'a list_context * 'a list

  type 'a tree_zipper = Top | Zip of 'a focused_list * 'a zipper

  type 'a focused_alt_tree = 'a tree_zipper*'a tree

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
      





end


