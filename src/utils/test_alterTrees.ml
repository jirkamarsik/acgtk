open AlterTrees.AlternTrees

let init_f_list l = [],l

let tree0=
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
		    Tree (Node ("(0,1).(2,1)",[]));
		    Tree (Node ("(0,1).(2,2)",[]));
		    Tree (Node ("(0,1).(2,3)",[]));
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
		  ];
	      ]
  ))
    

let f_forest= init_f_list [tree]
let f_forest_init,f_tree_init = init [tree];;

let f_f_0,f_t_0= init [tree0]

let f_f_1,f_t_1=down f_f_0 f_t_0;;

let f_f_2,f_t_2=down f_f_1 f_t_1;;
let f_f_2,f_t_2=right f_f_1 f_t_1;;

let f_f_3,f_t_3=down f_f_2 f_t_2;;
let f_f_3,f_t_3=right f_f_2 f_t_2;;
let f_f_3,f_t_3=up f_f_2 f_t_2;;

let f_f_4,f_t_4=right f_f_3 f_t_3;;
let f_f_4,f_t_4=up f_f_3 f_t_3;;

build_tree f_f_0 f_t_0;;

build_tree f_forest_init f_tree_init;;
