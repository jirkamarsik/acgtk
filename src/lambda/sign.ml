open Table
open Tries
open Lambda
open Abstract_syntax
(* open Syntactic_data_structures *)
open Error

module Sign =
  struct
    module Table : TABLE = Make_table (struct let b = 10 end)
	
    type term = Lambda.term

    type sig_entry =
      | Type_decl of (string * int * Lambda.kind)
	    (** The first parameter ([string]) is the name of the type,
		the second parameter its indexd and the last parameter is
		its kind *)
      | Type_def of (string * int * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the defined
		type, the second parameter its index and the last
		parameter is its value *)
      | Term_decl of (string * int * Abstract_syntax.syntactic_behavior * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its type *)
      | Term_def of (string * int * Abstract_syntax.syntactic_behavior * Lambda.term * Lambda.type_def)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its value *)
	    	
    type t = Signature of (string * Abstract_syntax.location) * int * sig_entry Table.t * sig_entry
	Tries.t (** The first string is the name of the
				  signature and the int is its size *)

    let empty (name,loc) = Signature ((name,loc),0, Table.create(), Tries.empty)
    let type_error e loc = raise (Error (Type_error (e,loc)))
	
    let name (Signature ((n,loc), _, _, _)) = n,loc
	
    let get_trie (Signature (_, _, _, trie)) = trie
	
    let is_type id (Signature (_, _, _, tr)) =
      try
	match (Tries.lookup id tr) with
        | Type_decl _ -> true
        | Type_def _ -> true
	| _ -> false
      with
      | Tries.Not_found -> false

    let is_constant id (Signature (_, _, _, tr)) =
      try
	match (Tries.lookup id tr) with
	| Term_decl (_,_,sb,_) -> true,Some sb
	| Term_def (_,_,sb,_,_) -> true,Some sb
	| _ -> false,None
      with
      | Tries.Not_found -> false,None
	    
    let add_warnings ws sg = 
      raise (Failure "Lambda.add_warnings")

    let get_warnings sg = 
      raise (Failure "Lambda.get_warnings")

    let rec inc_index = function
	[] -> []
      | (s,i)::ls -> (s,i+1)::(inc_index ls)
				
    let add_assoc ind_assoc s =
      (s,1)::(inc_index ind_assoc)

    let get_const (Signature (_, _, _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Term_decl (_, i, tk, ty) -> (i, tk, ty,true)
	| Term_def (_, i, tk, _, ty) -> (i, tk, ty,false)
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found


    let get_const_ind (Signature (_, _, _, tr)) id =
      try
	(match (Tries.lookup id tr) with
          Term_decl (_, i, _, _) -> i
	| Term_def (_, i, _, _, _) -> i
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found


    let convert t sg =
      let rec rec_convert t lin_ind_list ind_list =
	match t with
	  Abstract_syntax.Var(s,loc) -> 
	    (try
	      let s_index = List.assoc s ind_list in
	      Lambda.Var s_index
	    with 
	      Not_found ->
		try
		  let s_index = List.assoc s lin_ind_list in
		  Lambda.LVar s_index
		with
		  Not_found -> 
		    type_error (Not_defined_var s) loc)
	| Abstract_syntax.Const(s,loc) -> 
	    let (s_index,_,_,is_decl) = get_const sg s in
	    if is_decl 
	    then
	      Lambda.Const s_index
	    else 
	      Lambda.DConst s_index
	| Abstract_syntax.LAbs(s,t,loc) ->
	    let t' = rec_convert t (add_assoc lin_ind_list s) ind_list in
	    Lambda.LAbs(s,t')
	| Abstract_syntax.App(t1,t2,loc) ->
	    let t1' = rec_convert t1 lin_ind_list ind_list
	    and t2' = rec_convert t2 lin_ind_list ind_list
	    in Lambda.App(t1',t2')
      in
      rec_convert t [] []

    let verbose = false
		
	
(*     let create(name) = Signature (name,0, Table.create(), empty) *)
	
    let get_ind ind_assoc s =
      List.assoc s ind_assoc
	
    let size (Signature (_, n, _, _)) = n
	
    let insert_type_decl id ki (Signature (name,size, tb, tr)) =
      let e = Type_decl (id, size , ki)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr)

    let insert_type_def id ty (Signature (name,size, tb, tr)) =
      let e = Type_def (id, size , ty)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr)

    let insert_term_decl id tk ty (Signature (name,size, tb, tr)) =
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
	
(*** impossible kind et type_of doivent etre transfomes...*)
(*     let add_entry e (Signature (name,size, tb, tr) as sg) = *)
(*       match e with *)
(*       | Abstract_syntax.Type_decl (id,l,k) -> insert_type_decl id k sg *)
(*       | Abstract_syntax.Type_def (id,l,def) -> insert_type_def id def sg *)
(*       | Abstract_syntax.Term_decl (id,k,l,type_of) ->  *)
(* 	  insert_term_decl id k type_of sg *)
(*       | Abstract_syntax.Term_def (id,k,l,t,type_of) ->  *)
(* 	  insert_term_def id k t type_of sg *)

    let lookup i (Signature (_, _, tb, _)) = Table.lookup i tb


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

(*     let is_decl (Signature (_, _, _, tr)) id = *)
(*       try *)
(* 	(match (Tries.lookup id tr) with *)
(*           Term_decl (_, _, _, _) -> true *)
(* 	| Term_def (_, _, _, _, _) -> false *)
(* 	| _                          -> raise Not_found) *)
(*       with Tries.Not_found -> raise Not_found *)

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
	let (_,tk,_,_) = get_const sg id in
	(tk=Abstract_syntax.Infix)
      with 
	Not_found -> false
	
    let is_binder id (Signature (_, _, _, _) as sg) =
      if verbose
      then 
	print_string "is_binder\n";
      try 
	let (_,tk,_,_) = get_const sg id in
	(tk=Abstract_syntax.Binder)
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

