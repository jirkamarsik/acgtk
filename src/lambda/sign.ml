open Table
open Tries
open Lambda
open Abstract_syntax
(* open Syntactic_data_structures *)
open Error
open Utilitaires

module Sign =
  struct
    type t = Utilitaires.t

    type entry = Utilitaires.sig_entry
	    	
    type term = Lambda.term

    type stype = Lambda.stype

    let empty = Utilitaires.empty

    let name = Utilitaires.name
	
    let add_entry e sg =
(*       match e with *)
(*       | Abstract_syntax.Type_decl (id,_,((Abstract_syntax.K tdefs) as k)) ->  *)
(* 	  let new_kd = Typechecker.typecheck_kind sg tdefs in *)
(* 	  let lambda_kind =  *)
(* 	    List.fold_right (fun t k -> Lambda.Depend (t,k))  new_kd Lambda.Type in *)
(* 	  Utilitaires.insert_type_decl id lambda_kind sg *)

(*       | Abstract_syntax.Type_def (id,_,typ,((Abstract_syntax.K tdefs) as k)) ->  *)
(* 	  let new_kd = Typechecker.typecheck_kind sg tdefs in *)
(* 	  let new_type = Typechecker.typecheck_type sg typ in *)
(* 	  Utilitaires.insert_type_def id new_type sg *)

(*       | Abstract_syntax.Term_decl (id,sb,_,typ) -> *)
(* 	  let new_type = Typechecker.typecheck_type sg typ in *)
(* 	  Utilitaires.insert_term_decl id sb new_type sg *)

(*       | Abstract_syntax.Term_def (id,sb,l,t,typ) -> *)
(* 	  let new_type = Typechecker.typecheck_type sg typ in  *)
(* 	  let (wfterm,_,_(\*new_sg*\)) =  *)
(* 	    Typechecker.typecheck_term t new_type [] sg [] in *)
(* 	  Utilitaires.insert_term_def id sb wfterm new_type (\*new_*\)sg *)
      Typechecker.typecheck_entry sg e

    let is_type id sg =
      let tr = Utilitaires.get_trie sg in
      try
	match (Tries.find id tr) with
        | Utilitaires.Type_decl _ -> true
        | Utilitaires.Type_def _ -> true
	| _ -> false
      with
      | Tries.Not_found -> false

    let is_constant id sg =
      let tr = Utilitaires.get_trie sg in
      try
	match (Tries.find id tr) with
	| Utilitaires.Term_decl (_,_,sb,_) -> true,Some sb
	| Utilitaires.Term_def (_,_,sb,_,_) -> true,Some sb
	| _ -> false,None
      with
      | Tries.Not_found -> false,None
	    
    let id_to_string sg i =
      let tb = Utilitaires.get_table sg in
      try
	match Utilitaires.Table.find i tb with
          Utilitaires.Type_decl (x, _, _) -> Abstract_syntax.Default,x
	| Utilitaires.Type_def (x, _, _) -> Abstract_syntax.Default,x
	| Utilitaires.Term_decl (x, _, sb, _) -> sb,x
	| Utilitaires.Term_def (x, _, sb, _, _) -> sb,x
      with Utilitaires.Table.Not_found -> 
	raise (Failure "Sign.id_to_string Not_found")
	  
    let unfold_type_definition i sg =
      let tb = Utilitaires.get_table sg in
      try
	match Utilitaires.Table.find i tb with
	| Utilitaires.Type_def (_, _, typ) -> typ
	| _ -> raise (Failure "Bug")
      with Utilitaires.Table.Not_found -> 
	raise (Failure "Sign.unfold_type_definition Not_found")
	  
    let unfold_term_definition i sg =
      let tb = Utilitaires.get_table sg in
      try
	match Utilitaires.Table.find i tb with
	| Utilitaires.Term_def (_, _, _, t, _) -> t
	| _ -> raise (Failure "Bug")
      with Utilitaires.Table.Not_found -> 
	raise (Failure "Sign.unfold_term_definition Not_found")

    let add_warnings ws sg = 
      raise (Failure "Lambda.add_warnings")

    let get_warnings sg = 
      raise (Failure "Lambda.get_warnings")

    let to_string sg =
      raise (Failure "Not yet implemented")

    let term_to_string t sg =
      Lambda.term_to_string t (id_to_string sg)

    let type_to_string t sg =
      Lambda.type_to_string t (id_to_string sg)

    let convert_term t typ sg =
      let new_type = Typechecker.typecheck_type sg typ in
      let (new_term,_,_) = Typechecker.typecheck_term t new_type [] sg [] in
      new_term,new_type

    let convert_type typ sg =
      Typechecker.typecheck_type sg typ

    let type_of_constant id sg =
      let tr = Utilitaires.get_trie sg in
      try
	match Tries.find id tr with
	| Utilitaires.Type_def (_, _, typ) -> typ
	| Utilitaires.Term_decl (_, _, _, typ) -> typ
	| Utilitaires.Term_def (_, _, _, _, typ) -> typ
	| _ -> raise (Failure "Sign.type_of_constant")
      with
      | Tries.Not_found ->  raise (Failure "Sign.type_of_constant")
	    
    let typecheck t wftype sg =
      let (new_term,_,_) = Typechecker.typecheck_term t wftype [] sg [] in
      new_term


    let fold f a sg =
      let tb = Utilitaires.get_table sg in
      Utilitaires.Table.fold (fun _ -> f) a tb

    let get_binder_argument_functional_type id sg =
      let tr = Utilitaires.get_trie sg in
      try
	match Tries.find id tr with
	| Utilitaires.Term_def (_, _, _, t, _) -> 
	    (match t with
	    | Lambda.Abs _ -> Some Abstract_syntax.Non_linear
	    | Lambda.LAbs _ -> Some Abstract_syntax.Linear
	    | _ -> None)
	| _ -> None
      with
      | Tries.Not_found ->  None
     

(*     let convert t sg = *)
(*       let rec rec_convert t lin_ind_list ind_list = *)
(* 	match t with *)
(* 	  Abstract_syntax.Var(s,loc) ->  *)
(* 	    (try *)
(* 	      let s_index = List.assoc s ind_list in *)
(* 	      Lambda.Var s_index *)
(* 	    with  *)
(* 	      Not_found -> *)
(* 		try *)
(* 		  let s_index = List.assoc s lin_ind_list in *)
(* 		  Lambda.LVar s_index *)
(* 		with *)
(* 		  Not_found ->  *)
(* 		    Typechecker.type_error (Not_defined_var s) loc) *)
(* 	| Abstract_syntax.Const(s,loc) ->  *)
(* 	    let (s_index,_,_,is_decl) = Utilitaires.get_const sg s in *)
(* 	    if is_decl  *)
(* 	    then *)
(* 	      Lambda.Const s_index *)
(* 	    else  *)
(* 	      Lambda.DConst s_index *)
(* 	| Abstract_syntax.LAbs(s,t,loc) -> *)
(* 	    let t' = rec_convert t (Utilitaires.add_assoc lin_ind_list s) ind_list in *)
(* 	    Lambda.LAbs(s,t') *)
(* 	| Abstract_syntax.Abs (s,t,loc) -> failwith "Not yet implemented" *)
(* 	| Abstract_syntax.App(t1,t2,loc) -> *)
(* 	    let t1' = rec_convert t1 lin_ind_list ind_list *)
(* 	    and t2' = rec_convert t2 lin_ind_list ind_list *)
(* 	    in Lambda.App(t1',t2') *)
(*       in *)
(*       rec_convert t [] [] *)


  end

