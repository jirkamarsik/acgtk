module AlternTrees :
  sig
    type address=(int*int) list * (int option)
    type relative_path=int*int option*address
    (* the 2nd argument is to move in the alternative trees at the top
       of the forest *)

    (** [diff (alt,add) (alt',add')] returns the relative path to go
	from the subtree wich occurs at address [add] in the [alt]-th
	alternative of some forest to the subtree wich occurs at
	address [add'] in the [alt']-th alternative the same
	forest. *)

    val diff : int*address -> int*address -> relative_path

    val path_to_string : relative_path -> string

    val address_to_string : address -> string
      
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
	
	
    type 'a focused_alt_tree = 'a alt_tree_zipper * 'a  tree
      
    type 'a zipper = 
    | ZTop | Zipper of ('a * 'a simple_tree focused_list * 'a zipper)
	
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

    val fold_depth_first:  (('a -> 'b) * ('b -> 'b -> 'b)) -> 'a simple_tree -> 'b

(*    val extract_tree : 'a alt_tree -> 'a simple_tree*'a alt_tree *)

    val init : 'a tree list -> ('a focused_alt_tree * 'a focused_tree) list

    val build_tree : 'a focused_alt_tree -> 'a focused_tree -> ('a focused_alt_tree * 'a focused_tree) list -> 'a focused_alt_tree * 'a focused_tree * ('a focused_alt_tree * 'a focused_tree) list
    val down : 'a focused_alt_tree -> 'a focused_tree -> ('a focused_alt_tree * 'a focused_tree) list -> 'a focused_alt_tree * 'a focused_tree * ('a focused_alt_tree * 'a focused_tree) list
    val right : 'a focused_alt_tree -> 'a focused_tree -> ('a focused_alt_tree * 'a focused_tree) list -> 'a focused_alt_tree * 'a focused_tree * ('a focused_alt_tree * 'a focused_tree) list
    val up : 'a focused_alt_tree -> 'a focused_tree -> 'a focused_alt_tree * 'a focused_tree

    val zip_up : 'a focused_tree -> 'a simple_tree

    (** [resumption resume] returns a pair [(Some t,resume')] where
	[t] is extracted from [resume], the latter being updated with
	possible alternatives met in building [t] to produce
	[resume']. It returns [(None,[])] if no tree can be
	extracted *)
    val resumption : ('a focused_alt_tree * 'a focused_tree) list -> 'a simple_tree option * ('a focused_alt_tree * 'a focused_tree) list

    val build_trees : 'a tree list -> 'a simple_tree list

  end
