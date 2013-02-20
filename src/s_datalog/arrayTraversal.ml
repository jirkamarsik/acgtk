module type Evaluator_TYPE =
  sig
    type state
    type cell
    val update: state -> cell -> cell option
    val collector: 'a -> state -> 'a
  end


module ArrayTraversal =
  struct
    type 'a row = 'a list
    type 'a array = 'a row list

    type ('a,'b) return = Return of ('a*('a*'b row*'b array) list) | Stop

(*    let string_of_res res =
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

    let string_of_resumption (state,row,array) =
      Printf.sprintf "Current state: %s\nCurrent row: %s\nCurrent array:\n%s\n"
	(string_of_res state)
	(string_of_res (List.rev row))
	(string_of_array array)
*)	
	

    let rec visit_row state row arr resume test =
      match row with
      | [] -> continue resume test
      | elt::remaining when test elt -> visit_array (elt::state) arr ((state,remaining,arr)::resume) test
      | _::remaining -> visit_row state remaining arr resume test
    and visit_array state arr resume test =
      match arr with
      | [] -> Return (state,resume)
      | row::remaining -> visit_row state row remaining resume test
    and continue resumption test = 
      match resumption with
      | [] -> Stop
      | (state,row,arr)::resume -> visit_row state row arr resume test
	
	
    let rec all_results_aux f acc state array resume test =
      match visit_array state array resume test with
      | Return (res,(current_state,r,arr)::resume) ->
	(* let () = Printf.printf "State: %s\n" (string_of_res res) in *)
	(* let () = Printf.printf "Continue with:\n%s"
	   (string_of_resumption (current_state,r,arr)) in*)
	all_results_aux f (f acc res) current_state (r::arr) resume test
      | Return (res,[]) -> 
	(* let () = Printf.printf "%s\n" (string_of_res res) in *)
	f acc res
      | Stop -> acc
	
    let all_results f acc array test = all_results_aux f acc [] array [] test
end

      

