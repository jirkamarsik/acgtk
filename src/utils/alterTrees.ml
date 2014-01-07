open Focused_list

module AlternTrees =
struct
  type address=int list
  type relative_path=int*addresse
  type 'a tree = Node of 'a*'a children list
  and 'a children = 'a alternative list
  and 'a alternative = Tree of 'a tree | Link_to address

  type 'a tree_zipper = Top | Zip of 'a children * 'a zipper Focused_list.t * 'a children

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


