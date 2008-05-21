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
	    	
    type sig_content = 
      {type_definitions: Abstract_sig.type_of_definition Utils.StringMap.t;
       term_definitions: (Abstract_sig.type_of_definition*Abstract_sig.term_kind) Utils.StringMap.t}
	 
    type t = Signature of string * int * sig_entry Table.t * sig_entry
	Tries.t * int list (** The first string is the name of the
				  signature and the int is its size *)
 
 (*    let content2content (content : Abstract_sig.sig_content) = *)
(*       {type_definitions = content.Abstract_sig.type_definitions; *)
(*        term_definitions = content.Abstract_sig.term_definitions} *)
	
  let is_infix id (Signature (_, _, _, _, _(* {type_definitions=_;term_definitions=defs} *))) =
(*     try *)
(*       let _,t = Utils.StringMap.find id defs in *)
(*       (t=Abstract_sig.Infix) *)
(*     with *)
(*     | Not_found ->  *)false
	  
    let is_binder id (Signature (_, _, _, _, _(* {type_definitions=_;term_definitions=defs} *))) =
(*       try *)
(* 	let _,t = Utils.StringMap.find id defs in *)
(* 	t = Abstract_sig.Binder *)
(*       with *)
(*       | Not_found -> *) false
	    
    let create(name) = Signature (name,0, Table.create(), Tries.empty,[])
	
    let get_ind ind_assoc s =
      List.assoc s ind_assoc

    let size (Signature (_, n, _, _, _)) = n
	
    let insert_type_dcl id ki (Signature (name,size, tb, tr,content)) =
      let e = Type_decl (id, size , ki)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr, content)

    let insert_type_def id ty (Signature (name,size, tb, tr,content)) =
      let e = Type_def (id, size , ty)
      in
      Signature (name,size+1, Table.insert size e tb, Tries.insert id e tr, content)

    let insert_term_dcl id tk ty (Signature (name,size, tb, tr,content)) =
      let e = Term_decl (id, size, tk, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let insert_term_def id tk te ty 
	(Signature (name,size, tb, tr,content)) =
      let e = Term_def (id, size, tk, te, ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let insert_var id tk ty 
	(Signature (name,size, tb, tr,content)) =
      let e = Term_def (id, size, tk, Lambda.Var(size), ty)
      in 
      Signature (name, size+1, Table.insert size e tb, Tries.insert id e tr, content)
	
    let lookup i (Signature (_, _, tb, _, _)) = Table.lookup i tb

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

(*     let cut_assoc_ind ind_assoc ind = *)
(*       let rec cut_bis ind_assoc ind =  *)
(* 	match ind_assoc with  *)
(* 	  [] -> raise Not_found *)
(* 	| (_,i)::l ->  *)
(* 	    if ind = 1 *)
(* 	    then ind_assoc *)
(* 	    else cut_bis l (ind - 1) *)
(*       in *)
(*       match ind_assoc with  *)
(* 	[] -> raise Not_found *)
(*       | (_,i)::l ->  *)
(* 	  if ind = 1 || ind = i *)
(* 	  then ind_assoc *)
(* 	  else cut_bis l (ind - 1) *)

    let display_assoc ind_assoc =
      print_string "\n[";
      let rec display = function 
	  [] -> print_string "]\n";
	| (v,c)::l -> print_string ("("^v^","^(string_of_int c)^")");
	    display l
      in display ind_assoc
	
    let get_const (Signature (_, _, _, tr, _)) id =
      try
      (match (Tries.lookup id tr) with
        Term_decl (_, i, tk, ty) -> (i, tk, ty)
      | Term_def (_, i, tk, _, ty) -> (i, tk, ty)
      | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_const_ind (Signature (_, _, _, tr, _)) id =
      try
      (match (Tries.lookup id tr) with
        Term_decl (_, i, _, _) -> i
      | Term_def (_, i, _, _, _) -> i
      | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_atom (Signature (_, _ , _, tr,_)) id =
      try
      (match (Tries.lookup id tr) with
        Type_decl (_, i, ki) -> (i, ki)
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let get_atom_ind (Signature (_, _ , _, tr,_)) id =
      try
      (match (Tries.lookup id tr) with
        Type_decl (_, i, _) -> i
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found
	  
    let string_of_const i (Signature (_, _, tb, _, _)) =
      match Table.lookup i tb with
        Type_decl _ -> raise (Failure "Sign.string_of_const")
      | Term_decl (x, _, _, _) -> x
      | Term_def (x, _, _, _, _) -> x
      | _ -> raise (Failure "string_of_const Not yet implemented")

    let string_of_atom i (Signature (_, _, tb, _, _)) =
      match Table.lookup i tb with
        Type_decl (x, _, _) -> x 
      | Term_decl _ -> raise (Failure "Sign.string_of_atom")
      | Term_def _ -> raise (Failure "Sign.string_of_atom")
      | _ -> raise (Failure "string_of_atom Not yet implemented")

(*
    let kind_of_atom i (Signature (_, tb)) =
      match Table.lookup i tb with
        Type_declaration (_, ki) -> ki
      | Term_declaration _       -> raise (Failure "Sign.kind_of_atom")
  
*)  

    let is_a_cst id (Signature (_, _, _, tr, _)) =
      try
	match (Tries.lookup id tr) with
	| Term_decl _ -> true
	| Term_def _ -> true
	| _ -> false
      with
	| Tries.Not_found -> false


    let is_a_type id (Signature (_, _, _, tr, _)) =
      try
	match (Tries.lookup id tr) with
          | Type_decl _ -> true
	  | _ -> false
      with
	| Tries.Not_found -> false

    let rec find level = function
	[] -> raise Not_found
      | v::l -> 
	  if level = 1
	  then v
	  else find (level - 1) l

    let rec inc_type = function
	Lambda.Type_atom (i,l) -> Lambda.Type_atom (i+1,l)
      | Lambda.Linear_arrow(ty1,ty2) -> Lambda.Linear_arrow(inc_type ty1,inc_type ty2)

(* let pretty_print (Signature (name,size,_,trie,content) as sg) = *)
(*   let list_decl = Tries.Tries.content trie in *)
(*   Printf.printf "Sign \"%s\": [\n" name; *)
(*   List.iter *)
(*     (fun x -> (match x with *)
(*       Sign.Sign.Term_def(s,i,kd,wf,typ) ->  *)
(* 	print_string (s^" =PP "); *)
(* 	print_string (Sign.Sign.pretty_print wf); *)
(* 	print_string " : "; *)
(* 	Typechecker.display_typ_tdef t typ; *)
(* 	print_string " ;\n "; *)
(*     | Sign.Sign.Term_decl(s,i,kd,typ) ->  *)
(* 	print_string (s^" : "); *)
(* 	Typechecker.display_typ_tdef t typ; *)
(* 	print_string " ;\n "; *)
(*     | Sign.Sign.Type_def(s,i,typ) ->  *)
(* 	print_string (s^" : "); *)
(* 	Typechecker.display_typ_tdef t typ; *)
(* 	print_string " ;\n "; *)
(*     | Sign.Sign.Type_decl *)
(* 	(s,i,Lambda.Lambda.K tdl) ->  *)
(* 	  print_string (s^" : K["); *)
(* 	  List.iter (Typechecker.display_typ_tdef t) tdl; *)
(* 	  print_string "] ;\n ";)) *)
(*     list_decl *)


(*     let rec pretty_print_sg = *)
(*       let rec pprint level ind_list tm = *)
(* 	match tm with *)
(* 	  Type_decl(s,i,k) ->  *)
(* 	    Printf.sprintf *)
(* 	      "(Type_decl %s %i %s)" s i k(pprint (level + 1) (x::ind_list) t) *)
(* 	| Type_def *)
(* 	| Term_decl *)
(* 	| Term_def *)
(*       in  *)
(*       pprint 0 [] entry *)

    let rec pretty_print l_const tm =  
      let rec pprint level ind_list tm =
        match tm with
          Lambda.Var i        -> 
(* 	    print_string "Var : ";  *)
	    (find i ind_list)
        | Lambda.Const i      ->  
(* 	    print_string ("Const : "^(string_of_int i)^" [");  *)
(* 	    print_list ind_list; *)
	    (find i ind_list)
        | Lambda.LAbs (x, t)  -> 
(* 	    print_string "LAbs : ";  *)
	    Printf.sprintf
	      "(lambda %s.%s)" x (pprint (level + 1) (x::ind_list) t)
        | Lambda.App (t1, t2) -> 
(* 	    print_string "App : ";  *)
	    Printf.sprintf
	      "(%s %s)" 
	      (pprint level ind_list t1)
              (pprint level ind_list t2)
      and print_list ind_list =
	match ind_list with
	  [] -> print_string "]\n"
	| x::l -> print_string (x^";"); print_list l
      in 
      pprint 0 l_const tm

 

  let rec term_to_string t ind_assoc (sg:t) = 
    match t with
    | Lambda.Var(i) -> 
	let (x,_) = find i ind_assoc in
	print_string ("Var"^x); x
    | Lambda.Const (i) -> 
	let (x,_) = find i ind_assoc in
	(*print_string ("Const "^x);*) x
	  (*    | Abs (s,t,_) -> 
		let vars,u=unfold_abs [s] t in
		Printf.sprintf
		"Lambda %s. %s"
		(Utils.string_of_list " " (fun x -> x) (List.rev vars))
		(term_to_string u) *)
    | Lambda.LAbs (s,t) -> 
	let vars,u=unfold_labs [s] t in
	Printf.sprintf
	  "(lambda %s. %s)"
	  (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	  (term_to_string u ind_assoc sg)
    | Lambda.App (Lambda.Const (i),(Lambda.LAbs(x,u) as t)) when is_binder (string_of_const i sg) sg ->
	let s = string_of_int i in
	let vars,u= unfold_binder s sg [x] u in
	Printf.sprintf
	  "(%s %s. %s)"
	  s
	  (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	  (term_to_string u ind_assoc sg) 
    | Lambda.App ((Lambda.App (Lambda.Const (i),t1)),t2) when is_infix (string_of_const i sg) sg ->
	Printf.sprintf
	  "(%s %s %s)"
	  (term_to_string t1 ind_assoc sg)
	  (string_of_int i)
	  (term_to_string t2 ind_assoc sg)
    | Lambda.App (t1,t2) ->
	Printf.sprintf
	  "(%s %s)"
	  (term_to_string t1 ind_assoc sg)
	  (term_to_string t2 ind_assoc sg)
	  (*  and unfold_abs acc = function
	      | Abs (s,t,_) -> unfold_abs (s::acc) t
	      | t -> acc,t *)
  and unfold_labs acc = function
    | Lambda.LAbs (s,t) -> unfold_labs (s::acc) t
    | t -> acc,t
  and unfold_app acc = function
    | Lambda.App (t1,t2) -> unfold_app (t2::acc) t1
    | t -> acc,t
  and unfold_binder binder sg acc = function
    | Lambda.App (Lambda.Const (i),(Lambda.LAbs(x,u) as t)) when let s = (string_of_const i sg) in (is_binder s sg)&&(s=binder) -> unfold_binder binder sg (x::acc) u
    | t -> acc,t
	
  let rec is_atomic_type = function
    | Lambda.Type_atom _ -> true
(*    | Dep (_,t,_) -> is_atomic_type t  *)
    | _ -> false
      
  let is_arrow = function
(*    | Arrow _ -> true *)
    | Lambda.Linear_arrow _ -> true
    | _ -> false




  let rec type_def_to_string ind_assoc def sg = match def with
    | Lambda.Type_atom (i,terms) ->
	(match terms with
             [] -> (*string_of_int i*)
	       let (v,n) = find i ind_assoc in
	       v(*^(string_of_int n)^"-"^(string_of_int i)*)
           | _  -> Printf.sprintf "%i %s" i (Utils.string_of_list " " (fun x -> Printf.sprintf "(%s)" (term_to_string x ind_assoc sg )) terms))
    | Lambda.Linear_arrow (t1,t2) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string ind_assoc u sg  else Printf.sprintf "(%s)" (type_def_to_string ind_assoc u sg ) in
	Printf.sprintf
	  "%s -> %s"
	  (Utils.string_of_list
	     " -> "
	     (fun x -> if is_atomic_type x then type_def_to_string ind_assoc x sg  else Printf.sprintf "(%s)" (type_def_to_string ind_assoc x sg ))
	     (List.rev arrows))
	  u_string
(*    | Lambda.Arrow (t1,t2,_) -> 
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string ind_assoc u sg else Printf.sprintf "(%s)" (type_def_to_string ind_assoc u sg ) in
	  Printf.sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if (is_atomic_type x) then type_def_to_string ind_assoc x else Printf.sprintf "(%s)" (type_def_to_string ind_assoc x))
	       (List.rev arrows))
	    u_string
    | Dep ((s,_,ty1),ty2,_) ->
	let deps,u = unfold_dep [(s,ty1)] ty2 in
	Printf.sprintf 
	  "(%s) %s"
	  (Utils.string_of_list ", " (fun (id,t) -> Printf.sprintf "%s:%s" id (type_def_to_string ind_assoc t)) (List.rev deps))
          (if is_atomic_type u
	   then
	     type_def_to_string ind_assoc u
           else
	     Printf.sprintf "(%s)" (type_def_to_string ind_assoc u))
    | Type_Abs ((s,_,ty),_) -> 
	let abs,u = unfold_type_abs [s] ty in
	  Printf.sprintf
	    "lambda %s.(%s)"
	    (Utils.string_of_list " " (fun x -> x) (List.rev abs))
	    (type_def_to_string ind_assoc ty) *)
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
	

  let to_string ind_assoc ((Signature (name,_,_,trie,content)) as sg) =
    Printf.sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list_rev
	 "\n"
	 (function
	   | Type_decl (id,_,Lambda.K types) -> 
                (match types 
                 with [] -> Printf.sprintf "\t%s: type;" id
                   | _  -> Printf.sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," (fun s -> type_def_to_string ind_assoc s sg) types))
	    | Type_def (id,_,value) -> Printf.sprintf "\t%s = %s: type;" id (type_def_to_string ind_assoc value sg)
	    | Term_decl (id,_,_,ty) -> 
		let t = ""(* match snd (Utils.StringMap.find id content.term_definitions) with *)
(* 		  | Abstract_sig.Default -> "" *)
(* 		  | Abstract_sig.Infix -> "infix " *)
(* 		  | Abstract_sig.Prefix -> "prefix " *)
(* 		  | Abstract_sig.Binder -> "binder "  *)in
		  Printf.sprintf "\t%s%s: %s;" t id (type_def_to_string ind_assoc ty sg)
	    | Term_def (id,_,_,value,type_of) -> 
		let t = ""(* match snd (Utils.StringMap.find id content.term_definitions) with *)
(* 		  | Abstract_sig.Default -> "" *)
(* 		  | Abstract_sig.Infix -> "infix " *)
(* 		  | Abstract_sig.Prefix -> "prefix " *)
(* 		  | Abstract_sig.Binder -> "binder " *) in
		  Printf.sprintf "\t%s%s = %s: %s;" t id (term_to_string value ind_assoc sg) (type_def_to_string ind_assoc type_of sg))
	 (Tries.content trie))


  end
