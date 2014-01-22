module AlternTrees :
  sig
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
    | ZTop | Zipper of ('a simple_tree focused_list * 'a zipper)
	
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

    val extract_tree : 'a alt_tree -> 'a simple_tree*'a alt_tree

  end
