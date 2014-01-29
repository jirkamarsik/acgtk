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

let output_tree t = 
  let buff=Buffer.create 80 in
  let () = print_tree "" buff t in
  let () = Printf.bprintf buff "\n\n" in
  Printf.printf "%s" (Buffer.contents buff)


type inputs =
| Stop
| Next
    
let return_input s =
  match String.lowercase (String.trim s) with
  | "y" | "yes"-> Some Next
  | "n" | "no" -> Some Stop
  | "" -> Some Next
  | _ -> None
    
    
let interact_aux get_input =
  get_input (read_line ())
    
      
let rec interact message get_input =
  let () = Printf.printf "%s %!" message in
  match interact_aux get_input with
  | Some v -> v
  | None -> interact message get_input
    
let rec ask_for_next_parse f param =
  let msg = Printf.sprintf "Do you want to look for another solution?\n\ty/yes\n\tn/no\n(Default: yes):" in
  match interact msg return_input with
  | Next -> 
    let () = Printf.printf "Going to get a term\n%!" in
    (match f param with
    | None -> Printf.printf "No other returned value\n"
    | Some new_param -> ask_for_next_parse f new_param)
  | Stop -> ()

      



    
let resume= [init [tree]] in
ask_for_next_parse
  (fun res -> 
    match resumption res with
    | None,_ -> None
    | Some t,resume -> let () = output_tree t in Some resume)
  resume
