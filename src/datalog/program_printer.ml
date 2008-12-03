module Program_printer =
  struct
    open Program
    let print_variable =
      function n ->
	if n>18 then
	  String.concat "" ["v." ; string_of_int n]
	else List.nth 
	  ["i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"t";"u";"v";"w";"x";"y";"z"]
	  n

    let print_arg var= 
      let res =
	String.concat "," (List.map (print_variable) var)
      in
	"("^res^")"

    let print_pred sign=
      function Pred(k,var) ->
	let name = Signature.get_name k sign
	in name^(print_arg var)
	  
    let print_clause sign = 
      function Cl(cl,l) ->
	let rhs = 
	  String.concat 
	    " , "
	    (List.map (print_pred sign) l)
	in
	let rhs = rhs^" ."
	in
	  (print_pred sign cl)^" :- "^rhs
	

    let print_program = 
      function Prog (sign,cl) ->
	(String.concat "\n"
	  (List.map
	      (print_clause sign)
	      cl
	  ))^"\n"
  end
