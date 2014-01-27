open AlterTrees.AlternTrees

let init_f_list l = [],l

let tree0=
  Tree (Node ("tree0.(0,1)",
	      [
		init_f_list 
		  [
		    Tree (Node ("tree0.(0,1).(1,1)",[]));
		    Tree (Node ("tree0.(0,1).(1,2)",[]));
		    Tree (Node ("tree0.(0,1).(1,3)",[]));
		  ];
		init_f_list 
		  [
		    Tree (Node ("tree0.(0,1).(2,1)",[]));
		    Tree (Node ("tree0.(0,1).(2,2)",[]));
		    Tree (Node ("tree0.(0,1).(2,3)",[]));
		  ];
	      ]
  ))


let tree = 
  Tree (Node ("(0,1)",
	      [
		init_f_list 
		  [
		    Tree (Node ("(0,1).(1,1)",[]));
		    Tree (Node ("(0,1).(1,2)",[]));
		    Tree (Node ("(0,1).(1,3)",[]));
		  ];
		init_f_list
		  [
		    Tree (Node ("(0,1).(2,1)",
				[
				  init_f_list
				    [
				      Tree (Node ("(0,1).(2,1),(1,1)",[]));
				      Tree (Node ("(0,1).(2,1),(1,2)",[]));
				    ];
				  init_f_list
				    [
				      Tree (Node ("(0,1).(2,1),(2,1)",[]));
				      Link_to (2,None,[(4,2);(2,2)]);
				    ];
				]));
		    Tree (Node ("(0,1).(2,2)",[]));
		  ];
		init_f_list
		  [
		    Tree (Node ("(0,1).(3,1)",[]));
		    Tree (Node ("(0,1).(3,2)",[]));
		    Tree (Node ("(0,1).(3,3)",[]));
		    Tree (Node ("(0,1).(3,4)",[]));
		  ];
		init_f_list
		  [
		    Tree (Node ("(0,1).(4,1)",[]));
		    tree0;
		  ];
	      ]
  ))
    

let rec print_tree prefix buffer tree =
  match tree with
  | SimpleTree (v,[]) ->
    Printf.bprintf buffer "%s -- %s\n" prefix v
  | SimpleTree (v,a::children) ->
    let () = print_tree (Printf.sprintf "%s -- %s" prefix v) buffer a in
    List.iter
      (fun child -> print_tree (String.make (4+String.length prefix + String.length v) ' ') buffer child)
      children


let trees= build_trees [tree] in
let buff=Buffer.create 80 in
let () = Printf.bprintf buff "Found %d trees:\n" (List.length trees) in
let () = 
  List.iter
    (fun t ->
      let () = print_tree "" buff t in
      Printf.bprintf buff "\n\n")
    trees in
Printf.printf "%s" (Buffer.contents buff)


