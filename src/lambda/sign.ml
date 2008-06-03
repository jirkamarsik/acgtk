open Table
open Tries
open Lambda
open Abstract_syntax

module Sign =
  struct
    module Table : TABLE = Make_table (struct let b = 10 end)
	
    type sig_entry = 
      | Type_decl of (string * int * Lambda.kind)
	    (** The first parameter ([string]) is the name of the type,
		the second parameter its indexd and the last parameter is
		its kind *)
      | Type_def of (string * int * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the defined
		type, the second parameter its index and the last
		parameter is its value *)
      | Term_decl of (string * int * Abstract_sig.term_kind * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its type *)
      | Term_def of (string * int * Abstract_sig.term_kind * Lambda.term * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its value *)
	    	
    type t = Signature of string * int * sig_entry Table.t * sig_entry
	Tries.t (** The first string is the name of the
				  signature and the int is its size *)
	
    let verbose = false
	
	
    let create(name) = Signature (name,0, Table.create(), Tries.empty)
	
    let get_ind ind_assoc s =
      List.assoc s ind_assoc
	
    let size (Signature (_, n, _, _)) = n
	
    let insert_type_dcl id ki (Signature (name,size, tb, tr)) =
      let e = Type_decl (id, size , ki)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr)

    let insert_type_def id ty (Signature (name,size, tb, tr)) =
      let e = Type_def (id, size , ty)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr)

    let insert_term_dcl id tk ty (Signature (name,size, tb, tr)) =
      let e = Term_decl (id, size, tk, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr)
	
    let insert_term_def id tk te ty (Signature (name,size, tb, tr)) =
      let e = Term_def (id, size, tk, te, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr)
	
    let insert_var id tk ty (Signature (name,size, tb, tr)) =
      let e = Term_def (id, size, tk, Lambda.Var(size), ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr)
	
    let lookup i (Signature (_, _, tb, _)) = Table.lookup i tb

    let rec inc_index = function
	[] -> []
      | (s,i)::ls -> (s,i+1)::(inc_index ls)
				
    let add_assoc ind_assoc s =
      (s,1)::(inc_index ind_assoc)

    let rec cut_assoc ind_assoc s =
      match ind_assoc with 
	[] -> raise Not_found
      | (x,_)::l when x = s -> ind_assoc
      | (x,_)::l -> cut_assoc l s

    let rec cut_assoc_ind ind_assoc ind =
      match ind_assoc with 
	[] -> raise Not_found
      | x::l -> 
	  if ind = 1
	  then ind_assoc
	  else cut_assoc_ind l (ind - 1)

    let give_level = function
	Lambda.Var i -> i
      | Lambda.Const i -> i
      | t -> raise (Failure "Sign.give_level")

    let cut ind_assoc typ =
      cut_assoc_ind ind_assoc (give_level typ)

    let get_const (Signature (_, _, _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Term_decl (_, i, tk, ty) -> (i, tk, ty)
	| Term_def (_, i, tk, _, ty) -> (i, tk, ty)
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let is_decl (Signature (_, _, _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Term_decl (_, _, _, _) -> true
	| Term_def (_, _, _, _, _) -> false
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_const_ind (Signature (_, _, _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Term_decl (_, i, _, _) -> i
	| Term_def (_, i, _, _, _) -> i
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_atom (Signature (_, _ , _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Type_decl (_, i, ki) -> (i, ki)
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let get_atom_ind (Signature (_, _ , _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Type_decl (_, i, _) -> i
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let string_of_const i (Signature (_, _, tb, _)) =
      match Table.lookup i tb with
        Type_decl _ -> raise (Failure "Sign.string_of_const")
      | Term_decl (x, _, _, _) -> x
      | Term_def (x, _, _, _, _) -> x
      | _ -> raise (Failure "string_of_const Not yet implemented")

    let string_of_atom i (Signature (_, _, tb, _)) =
      match Table.lookup i tb with
        Type_decl (x, _, _) -> x 
      | Term_decl (x,_,_,_) -> print_int i;print_string x;raise (Failure "Sign.string_of_atom 1")
      | Term_def _ -> raise (Failure "Sign.string_of_atom 2")
      | _ -> raise (Failure "string_of_atom Not yet implemented")

    let is_infix id (Signature (_, _, _, _) as sg) =
      if verbose
      then 
	print_string "is_infix\n";
      try 
	let (_,tk,_) = get_const sg id in
	(tk=Abstract_sig.Infix)
      with 
	Not_found -> false
	
    let is_binder id (Signature (_, _, _, _) as sg) =
      if verbose
      then 
	print_string "is_binder\n";
      try 
	let (_,tk,_) = get_const sg id in
	(tk=Abstract_sig.Binder)
      with 
	Not_found -> false

    let is_a_cst id (Signature (_, _, _, tr)) =
      try
	match (Tries.lookup id tr) with
	| Term_decl _ -> true
	| Term_def _ -> true
	| _ -> false
      with
      | Tries.Not_found -> false


    let is_a_type id (Signature (_, _, _, tr)) =
      try
	match (Tries.lookup id tr) with
        | Type_decl _ -> true
	| _ -> false
      with
      | Tries.Not_found -> false

    let rec find level = function
	[] -> raise (Failure "Sign.find : Not_found")
      | v::l -> 
	  if verbose
	  then
	    print_string "find\n";
	  if level = 1
	  then v
	  else find (level - 1) l

    let rec inc_type = function
	Lambda.Type_atom (i,l) -> Lambda.Type_atom (i+1,l)
      | Lambda.Linear_arrow(ty1,ty2) -> Lambda.Linear_arrow(inc_type ty1,inc_type ty2)

	    


  end

module Display =
struct

  let verbose = false

  let rec term_to_string t ind_assoc (sg:Sign.t) = 
    if verbose
    then
      (print_string "term_to_string\n";
       List.iter (fun (x,i) -> print_string (x^", "^(string_of_int i)^" ; ")) ind_assoc;
      );
    match t with
    | Lambda.Var(i) -> print_int i;
	if verbose
	then
	  print_string ("Var "^(string_of_int i));
	let (x,_) = Sign.find i ind_assoc in
	if verbose
	then
	  print_string ("Var"^x);
	x
    | Lambda.LVar(i) -> 
	if verbose
	then
	  print_string ("LVar "^(string_of_int i)^"\n");
	let (x,_) = Sign.find i ind_assoc in
	if verbose
	then
	  print_string ("LVar"^x);
	x
    | Lambda.Const (i) ->
	if verbose
	then
	  print_string ("Const "^(Sign.string_of_const i sg)^"\n");
(* 	let (x,_) = find i ind_assoc in *)
(* 	x *)
	Sign.string_of_const i sg;
    | Lambda.DConst (i) ->
	if verbose
	then
	  print_string "DConst \n";
	Sign.string_of_const i sg;
    | Lambda.LAbs (s,t) ->
	if verbose
	then
	  print_string ("LAbs "^s^"\n");
	let vars,new_ind_assoc,u =
	  unfold_labs [s] (Sign.add_assoc ind_assoc s) t in
	Printf.sprintf
	  "(lambda %s. %s)"
	  (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	  (term_to_string u new_ind_assoc sg)
(*     | Lambda.App (Lambda.Const (i),(Lambda.LAbs(x,u) as t)) when is_binder (string_of_const i sg) sg -> print_string "App1 "; *)
    | Lambda.App (Lambda.Const (i),(Lambda.LAbs(x,u) as t)) when Sign.is_binder (Sign.string_of_const i sg) sg ->
	if verbose
	then
	  print_string "App1 ";
	let s = (Sign.string_of_const i sg) in
	let vars,u= unfold_binder s sg ind_assoc [x] u in
	Printf.sprintf
	  "(%s %s. %s)"
	  s
	  (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	  (term_to_string u (Sign.add_assoc ind_assoc x) sg)
    | Lambda.App (Lambda.DConst (i),(Lambda.LAbs(x,u) as t)) when Sign.is_binder (Sign.string_of_const i sg) sg ->
	if verbose
	then
	  print_string "App1bis ";
	let s = (Sign.string_of_const i sg) in
	let vars,u= unfold_binder s sg ind_assoc [x] u in
	Printf.sprintf
	  "(%s %s. %s)"
	  s
	  (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	  (term_to_string u (Sign.add_assoc ind_assoc x) sg)
(*     | Lambda.App ((Lambda.App (Lambda.Const (i),t1)),t2) when is_infix (string_of_const i sg) sg -> print_string "App2 "; *)
    | Lambda.App ((Lambda.App (Lambda.Const (i),t1)),t2) when Sign.is_infix (Sign.string_of_const i sg) sg -> 
	if verbose
	then
	  (print_string "App2 : ";
	   print_string (term_to_string t1 ind_assoc sg);
	   print_string "\n string_of_const : ";
	   print_string (Sign.string_of_const i sg);
	   print_string "\nt2 : ";
	   print_string (term_to_string t2 ind_assoc sg);
	  );
	Printf.sprintf
	  "(%s %s %s)"
	  (term_to_string t1 ind_assoc sg)
	  (Sign.string_of_const i sg)
	  (term_to_string t2 ind_assoc sg)
    | Lambda.App ((Lambda.App (Lambda.DConst (i),t1)),t2) when Sign.is_infix (Sign.string_of_const i sg) sg -> 
	if verbose
	then
	  print_string "App2bis ";
	Printf.sprintf
	  "(%s %s %s)"
	  (term_to_string t1 ind_assoc sg)
	  (Sign.string_of_const i sg)
	  (term_to_string t2 ind_assoc sg)
    | Lambda.App (t1,t2) -> 
	if verbose
	then
	  print_string " App3 ";
	Printf.sprintf
	  "(%s %s)"
	  (term_to_string t1 ind_assoc sg)
	  (term_to_string t2 ind_assoc sg)
	  (*  and unfold_abs acc = function
	      | Abs (s,t,_) -> unfold_abs (s::acc) t
	      | t -> acc,t *)
  and unfold_labs acc ind_assoc = function
    | Lambda.LAbs (s,t) -> unfold_labs (s::acc) (Sign.add_assoc ind_assoc s) t
    | t -> acc,ind_assoc,t
  and unfold_app acc = function
    | Lambda.App (t1,t2) -> unfold_app (t2::acc) t1
    | t -> acc,t
  and unfold_binder binder sg ind_assoc acc = function
    | Lambda.App (Lambda.Const (i),(Lambda.LAbs(x,u) as t)) 
      when let (s,_) = (Sign.find i ind_assoc) in (Sign.is_binder s sg)&&(s=binder) -> 
	unfold_binder binder sg (Sign.add_assoc ind_assoc x) (x::acc) u
    | t -> acc,t
	
  let rec is_atomic_type = function
    | Lambda.Type_atom _ -> true
(*    | Dep (_,t,_) -> is_atomic_type t  *)
    | _ -> false
      
  let is_arrow = function
(*    | Arrow _ -> true *)
    | Lambda.Linear_arrow _ -> true
    | _ -> false


  let rec type_def_to_string ind_assoc def sg = 
    if verbose
    then
      print_string "type_to_string\n";
    match def with
    | Lambda.Type_atom (i,terms) ->
	if verbose
	then
	  print_string ("Type_atom "^(string_of_int i)^"\n");
	(match terms with
             [] -> 
	       let v = Sign.string_of_atom i sg in
	       v
           | _  -> Printf.sprintf "%i %s" i (Utils.string_of_list " " (fun x -> Printf.sprintf "(%s)" (term_to_string x ind_assoc sg )) terms))
    | Lambda.Linear_arrow (t1,t2) -> 
	if verbose
	then
	  print_string "Linear\n";
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string ind_assoc u sg  else Printf.sprintf "(%s)" (type_def_to_string ind_assoc u sg ) in
	Printf.sprintf
	  "%s -> %s"
	  (Utils.string_of_list
	     " -> "
	     (fun x -> if is_atomic_type x then type_def_to_string ind_assoc x sg  else Printf.sprintf "(%s)" (type_def_to_string ind_assoc x sg ))
	     (List.rev arrows))
	  u_string

  and unfold_linear_arrow acc = function
    | Lambda.Linear_arrow (t1,t2) -> unfold_linear_arrow (t1::acc) t2
    | t -> acc,t
	(*  and unfold_arrow acc = function
	    | Arrow (t1,t2,_) -> unfold_arrow (t1::acc) t2
	    | t -> acc,t
	    and unfold_dep acc = function
	    | Dep ((s,_,t),t2,_) -> unfold_dep ((s,t)::acc) t2
	    | t -> acc,t
	    and unfold_type_abs acc = function
	    | Type_Abs ((s,_,t),_) -> unfold_type_abs (s::acc) t
	    | t -> acc,t *)
	

  let to_string ((Sign.Signature (name,_,_,trie)) as sg) =
    Printf.sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list
	 "\n"
	 (function
	   | Sign.Type_decl (id,_,Lambda.K types) ->     
	       if verbose
	       then
		 print_string ("Type_decl "^id^"\n");
               (match types 
               with [] -> Printf.sprintf "\t%s: type;" id
               | _  -> Printf.sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," (fun s -> type_def_to_string [] s sg) types))
	   | Sign.Type_def (id,_,value) ->  
	       if verbose
	       then
		 print_string "Type_def\n";
	       Printf.sprintf "\t%s = %s: type;" id (type_def_to_string [] value sg)
	   | Sign.Term_decl (id,_,tk,ty) ->
	       if verbose
	       then
		 print_string ("Term_decl "^id^"\n");
	       let t = 
(* match snd (Utils.StringMap.find id content.term_definitions) with *)
		 match tk with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder "in
	       Printf.sprintf "\t%s%s: %s;" t id (type_def_to_string [] ty sg)
	   | Sign.Term_def (id,_,tk,value,type_def) -> 
	       if verbose
	       then
		 print_string ("Term_def "^id^"\n");
	       let t = 
(* match snd (Utils.StringMap.find id content.term_definitions) with *)
		 match tk with
		  | Abstract_sig.Default -> ""
		  | Abstract_sig.Infix -> "infix "
		  | Abstract_sig.Prefix -> "prefix "
		  | Abstract_sig.Binder -> "binder " 
	       in
	       let te = 
		 (term_to_string value [] sg) in
	       if verbose
	       then
		 print_string ("\nTerm = "^te^"\n");
	       let ty = (type_def_to_string [] type_def sg) in
	       Printf.sprintf "\t%s%s = %s: %s;" t id te ty)
	 (Tries.content trie)
      )


  let display_assoc ind_assoc =
    print_string "\n[";
    let rec display = function 
	[] -> print_string "]\n";
      | (v,c)::l -> print_string ("("^v^","^(string_of_int c)^")");
	  display l
    in display ind_assoc
      
(* display a list of types *)
  let rec display_type_env sg = function
    | [] -> print_string "[]\n";
    | [(x,_)] -> print_string "(";
	print_string x; print_string ",";
	print_string ")]";
    | (x,_)::l -> print_string "(";
	print_string x; print_string ",";
	print_string ");"; display_type_env sg l
	  
(* display a list of variables with there types *)
  and display_var_env sg = function
    | [] -> print_string "\n";
    | [(x,y)] -> print_string "(";
	print_string x; print_string ","; display_tdef sg y;
	print_string ")]";
    | (x,y)::l -> print_string "(";
	print_string x; print_string ","; display_tdef sg y;
	print_string ");"; display_var_env sg l

(* display a term before typechecking *)
  and display_term sg = function
    | Abstract_sig.Var(s,_) -> print_string s
    | Abstract_sig.Const(s,_) -> print_string s
    | Abstract_sig.LAbs(s,t,_) -> 
	print_string "(lambda ";print_string s;print_string ". "; display_term sg t;
	print_string ")"
    | Abstract_sig.App(t1,t2,_) -> display_term sg t1;print_string " ";display_term sg t2
	  
(* display a term well typed *)
  and display_wfterm sg = function
    | Lambda.Var(i) -> print_string ("("^(string_of_int i)^")");
    | Lambda.LVar(i) -> print_string ("("^(string_of_int i)^")");
    | Lambda.Const(i) -> print_string ("("^(string_of_int i)^")");
    | Lambda.DConst(i) -> print_string ("("^(string_of_int i)^")");
(*       print_string (Sign.string_of_const i sg) *)
    | Lambda.LAbs(s,t) -> 
	print_string "(lambda ";print_string s;print_string ". "; display_wfterm sg t;
	print_string ")"
    | Lambda.App(t1,t2) -> display_wfterm sg t1;print_string " ";display_wfterm sg t2
	  
(* display a type before typechecking *)
  and display_tdef sg = function
    | Abstract_sig.Type_atom(s,_,tl) -> print_string s
    | Abstract_sig.Linear_arrow(td1,td2,_) ->
	print_string "(";display_tdef sg td1;
	print_string " -> ";display_tdef sg td2;print_string ")";
	
  and display_typ_tdef sg = function
    | Lambda.Type_atom(i,tl) -> 
	(try print_string ("Type_atom("^(Sign.string_of_atom i sg)^") ");
(* 	print_string (Sign.string_of_atom i sg) *)
	with _ -> print_int i)
    | Lambda.Linear_arrow(td1,td2) ->
	print_string "(";display_typ_tdef sg td1;
	print_string " -> ";display_typ_tdef sg td2;print_string ")";
	
(* display a list of variables associated with there de Bruijn index *)
  and display_index_assoc = function
    | [] -> print_string "]"
    | [(s,i)] -> print_string "(";print_string s; print_string ",";
	print_int i;print_string ")]"
    | (s,i)::ls -> print_string "(";print_string s; print_string ",";
	print_int i;print_string ");";display_index_assoc ls;
	

(* display a signature *)
  and display_signature sg =
    let size = Sign.size sg in
    let rec display_one i =
      match i with
	0 -> print_newline()
      | _ ->  
	  try
	    let entry = Sign.lookup i sg in
	    display_entry entry;
	    print_newline();
	    display_one (i-1)
	  with Not_found -> raise (Failure "lookup Not_found")
    and display_entry = function 
      | Sign.Type_decl(s, i, k) ->
	  print_string ("Type_decl("^s^(string_of_int i))
      | Sign.Type_def(s, i, tdef) ->
	  print_string ("Type_def("^s^(string_of_int i)^" : ");
	  display_typ_tdef sg tdef
      | Sign.Term_decl(s, i, tk, tdef) ->
	  print_string ("Term_decl("^s^(string_of_int i)^" : ");
	  display_typ_tdef sg tdef
      | Sign.Term_def(s, i, tk, t, tdef) ->
	  print_string ("Term_def("^s^(string_of_int i)^" = ");
	  display_wfterm sg t;
	  print_string " : ";
	  display_typ_tdef sg tdef
    in
    display_one (size-1)


end
