module Manager =
  DependencyManager.Make(struct
			  type t = char
			  let compare=compare
			  let to_string c = Printf.sprintf "%c" c
			end)

let manager = Manager.empty

let manager =
  List.fold_left
    (fun man (c1,c2) -> Manager.add_dependency c1 c2 man)
    manager
    ['j','l';
     'j','f';
     'l','h';
     'h','g';
     'h','e';
     'i','f';
     'i','g';
     'k','f';
     'k','a';
     'f','b';
     'f','c';
     'g','c';
     'g','d';
     'h','e';
    ]


let test elt = 
  let lst = Manager.dependencies elt manager in
  let () = Printf.printf "An appropriate dependency list from %c is:\n%s" elt (Utils.string_of_list " " (fun c -> Printf.sprintf "%c" c) lst) in
  Printf.printf "\nDone.\n%!"


let () = List.iter test ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l']
