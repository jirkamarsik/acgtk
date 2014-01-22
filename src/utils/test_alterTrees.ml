open AlterTrees.AlternTrees

let init_f_list l = [],l

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
