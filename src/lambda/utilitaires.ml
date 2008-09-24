open Table
open Tries
open Lambda
open Abstract_syntax
open Error

module Utilitaires =
  struct
    module Table : TABLE with type key = int = Make_table (struct let b = 10 end) 
	
    let verbose = false
		

    type sig_entry =
      | Type_decl of (string * int * Lambda.kind)
	    (** The first parameter ([string]) is the name of the type,
		the second parameter its indexd and the last parameter is
		its kind *)
      | Type_def of (string * int * Lambda.stype)
	    (** The first parameter ([string]) is the name of the defined
		type, the second parameter its index and the last
		parameter is its value *)
      | Term_decl of (string * int * Abstract_syntax.syntactic_behavior * Lambda.stype)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its type *)
      | Term_def of (string * int * Abstract_syntax.syntactic_behavior * Lambda.term * Lambda.stype)
	    (** The first parameter ([string]) is the name of the
		constant, the second parameter is its index and the last
		parameter is its value *)
	
    type t = Signature of (string * Abstract_syntax.location) * int * sig_entry Table.t * sig_entry
	Tries.t (** The first string is the name of the
				  signature and the int is its size *)
    let get_ind ind_assoc s =
      List.assoc s ind_assoc
	
    let get_trie (Signature (_, _, _, trie)) = trie

    let get_table (Signature (_, _, table, _)) = table

    let name (Signature ((n,loc), _, _, _)) = n,loc
	
    let empty (name,loc) = Signature ((name,loc),0, Table.empty, Tries.empty)

    let size (Signature (_, n, _, _)) = n
	
    let insert_type_decl id ki (Signature (name,size, tb, tr)) =
      let e = Type_decl (id, size , ki)
      in
      Signature (name,size+1, Table.add ~override:true size e tb, Tries.add ~override:true id e tr)

    let insert_type_def id ty (Signature (name,size, tb, tr)) =
      let e = Type_def (id, size , ty)
      in
      Signature (name,size+1, Table.add ~override:true size e tb, Tries.add ~override:true id e tr)

    let insert_term_decl id tk ty (Signature (name,size, tb, tr)) =
      let e = Term_decl (id, size, tk, ty)
      in 
      Signature (name, size+1, Table.add ~override:true size e tb, Tries.add ~override:true id e tr)
	
    let insert_term_def id tk te ty (Signature (name,size, tb, tr)) =
      let e = Term_def (id, size, tk, te, ty)
      in 
      Signature (name, size+1, Table.add ~override:true size e tb, Tries.add ~override:true id e tr)
	
    let insert_var id tk ty (Signature (name,size, tb, tr)) =
      let e = Term_def (id, size, tk, Lambda.Var(size), ty)
      in 
      Signature (name, size+1, Table.add ~override:true size e tb, Tries.add ~override:true id e tr)
	
    let lookup i (Signature (_, _, tb, _)) = Table.find i tb


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
      | t -> raise (Failure "Utilitaires.give_level")

    let cut ind_assoc typ =
      cut_assoc_ind ind_assoc (give_level typ)

    let get_atom (Signature (_, _ , _, tr) as sg) id =
      try
	(match (Tries.find id tr) with
          Type_decl (s, i, ki) -> (i,true)(*(i, ki) Type_decl(s,i,ki)*)
        | Type_def (s, i, ty) -> (i,false)(*get_atom sg i Type_def(s,i,ty)*)
(* 	  Type_decl(_,i,_) -> i,true *)
(* 	| Type_def(_,i,_) -> i,false *)
	| _ -> print_string "ici";raise (Failure "Utilitaires.get_atom"))
      with Tries.Not_found -> raise Not_found
	  
    let get_atom_ind (Signature (_, _ , _, tr)) id =
      try
	(match (Tries.find id tr) with
          Type_decl (_, i, _) -> i
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let string_of_const i (Signature (_, _, tb, _)) =
      match Table.find i tb with
      | Term_decl (x, _, _, _) -> x
      | Term_def (x, _, _, _, _) -> x
      | _ -> raise (Failure "Utilitaires.string_of_const")

    let string_of_atom i (Signature (_, _, tb, _)) =
      match Table.find i tb with
        Type_decl (x, _, _) -> x
      | Type_def (x, _, _) -> x
      |  _ -> raise (Failure "Utilitaires.string_of_atom")

    let string_of_atom_is i (Signature (_, _, tb, _)) =
      match Table.find i tb with
        Type_decl (x, i, ki) -> (*x,true *)Type_decl (x,i,ki)
      | Type_def (x, i, ty) -> (*x,false*)Type_def(x,i,ty)
      |  _ -> raise (Failure "Utilitaires.string_of_atom")

    let get_type_def typ sg =
      match  string_of_atom_is typ sg with
      | Type_def (_,_,ty) -> ty
      | _ -> raise (Failure "Utilitaires.get_type_def")
      
    let rec inc_index = function
	[] -> []
      | (s,i)::ls -> (s,i+1)::(inc_index ls)
				
    let add_assoc ind_assoc s =
      (s,1)::(inc_index ind_assoc)

    let get_const (Signature (_, _, _, tr)) id =
      try
	(match (Tries.find id tr) with
          Term_decl (_, i, tk, ty) -> (i, tk, ty,true)
	| Term_def (_, i, tk, _, ty) -> (i, tk, ty,false)
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found


    let get_const_ind (Signature (_, _, _, tr)) id =
      try
	(match (Tries.find id tr) with
          Term_decl (_, i, _, _) -> i
	| Term_def (_, i, _, _, _) -> i
	| _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found


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

    let rec find level = function
	[] -> raise (Failure "Utilitaires.find : Not_found")
      | v::l -> 
	  if verbose
	  then
	    print_string "find\n";
	  if level = 1
	  then v
	  else find (level - 1) l

end
