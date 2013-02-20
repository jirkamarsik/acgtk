open ArrayTraversal

let r1 = [ 1; 2; 3; 4]

let r2 = [ 5;6]

let r2' = []

let r3 = [7;8]


let a = [r1;r2;r3]


let string_of_res res =
  Printf.sprintf
    "%s"
    (List.fold_left (fun acc s -> Printf.sprintf "%d %s" s acc) "" res)

let rec string_of_array = function
  | [] -> ""
  | r::rows -> 
    Printf.sprintf
      "%s\n%s"
      (string_of_res (List.rev r))
      (string_of_array rows)
      
      
let ()=Printf.printf "r1: %s\n" (string_of_res r1)
  
let ()=Printf.printf "a=\n%s\n" (string_of_array a)
  
let _ = ArrayTraversal.all_results (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res)) () a (fun _x -> true)

let () = print_newline()
  
let _ = ArrayTraversal.all_results (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res)) () a (fun x -> (x mod 2)=0 )
let () = print_newline()

let _ = ArrayTraversal.all_results (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res)) () a (fun x -> (x mod 2)=1 )




	
