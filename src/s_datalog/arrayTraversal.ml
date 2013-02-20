module type Evaluator_TYPE =
  sig
    type state
    type cell
    val update: state -> cell -> state option
  end


module Make (E:Evaluator_TYPE)=
  struct
    type row = E.cell list
    type array = row list

    type return = Return of (E.state*(E.state*row*array) list) | Stop

    let rec visit_row state row arr resume =
      match row with
      | [] -> continue resume
      | elt::remaining ->
	begin
	  match E.update state elt with
	  | Some new_state -> visit_array new_state arr ((state,remaining,arr)::resume)
	  | None -> visit_row state remaining arr resume
	end
    and visit_array state arr resume =
      match arr with
      | [] -> Return (state,resume)
      | row::remaining -> visit_row state row remaining resume
    and continue resumption = 
      match resumption with
      | [] -> Stop
      | (state,row,arr)::resume -> visit_row state row arr resume
	
	
    let rec all_results_aux f acc state array resume =
      match visit_array state array resume with
      | Return (res,(current_state,r,arr)::resume) ->
	all_results_aux f (f acc res) current_state (r::arr) resume
      | Return (res,[]) -> 
	f acc res
      | Stop -> acc
	
    let collect_results f acc init array = all_results_aux f acc init array []
end

      

