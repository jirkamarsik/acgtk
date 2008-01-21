open Utils
open Printf
open Tries
open Table
open Abstract_syntax
open Error

exception Typing_error of string
exception Not_yet_implemented of string
      
 module Table = Make_table (struct let b = 10 end)
 
type symbol = string 
      
type type_env = (symbol * Abstract_sig.kind) list
      
type rec_term = 
    {term: Abstract_sig.term;
     mutable wfterm: Abstract_typ.term option;
     mutable typeofterm: Abstract_typ.type_def option;
   }

type new_type_list = (int * Abstract_typ.type_def option) list ref

let type_error e = raise (Error (Type_error e))


let type_nb = ref 100
let new_type_list : new_type_list = ref []
     
let  type_env = []
    
(* add an term elt of type typ in the environment *)
let add_env elt (pos1,pos2) typ env =
  try List.assoc elt env;
    type_error (Error.Already_defined_var (elt,pos1,pos2))
  with Not_found -> (elt,typ)::env



let rec typecheck_sig sig_new = function
  | [] -> sig_new
  | last_entry::entries -> 
      let sig_new2 = 
	typecheck_sig sig_new entries
      in
      let sig_new3 =
      typecheck_entry sig_new2 last_entry
      in
      sig_new3
	
(* equiv tdef trepl returns a type_def*)
(* replace tdef by the type_def trepl if tdef is not defined *)
and equiv tdef trepl =
  match tdef with
    Abstract_typ.Type_atom(s,_) ->
      (try
	let typ = List.assoc s !new_type_list
	in 
	match typ with
	  None -> trepl
	| Some t -> 
	    new_type_list := List.remove_assoc s !new_type_list;
	    new_type_list := (s,Some trepl) :: !new_type_list;
	    equiv t trepl
      with Not_found -> tdef)
  | _ -> tdef

(* check that t1 and t2 are equivalent types *)
and eq_typ t1 t2 = 
  match (equiv t1 t2, equiv t2 t1) with
    (Abstract_typ.Type_atom(s,tl),Abstract_typ.Type_atom(sbis,tlbis)) -> (s = sbis) && (tl = tlbis)
  | (Abstract_typ.Linear_arrow(td1,td2),Abstract_typ.Linear_arrow(td1bis,td2bis)) ->
      (eq_typ td1 td1bis) && (eq_typ td2 td2bis)
  | _ -> false


(* increments indexes *)
and inc_index = function
    [] -> []
  | (s,i)::ls -> (s,i+1)::(inc_index ls)

(* display a list of types *)
and display_type_env sg = function
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
  | Abstract_typ.Var(i) -> print_string ("("^(string_of_int i)^")");
      (try
	print_string (Signature.string_of_const i sg)
      with _ -> raise (Typing_error(string_of_int i)))
  | Abstract_typ.Const(i) -> print_string ("("^(string_of_int i)^")");
      print_string (Signature.string_of_const i sg)
  | Abstract_typ.LAbs(s,t) -> 
      print_string "(lambda ";print_string s;print_string ". "; display_wfterm sg t;
      print_string ")"
  | Abstract_typ.App(t1,t2) -> display_wfterm sg t1;print_string " ";display_wfterm sg t2

(* display a type before typechecking *)
and display_tdef sg = function
  | Abstract_sig.Type_atom(s,_,tl) -> print_string s
  | Abstract_sig.Linear_arrow(td1,td2,_) ->
      print_string "(";display_tdef sg td1;
      print_string " -> ";display_tdef sg td2;print_string ")";

and display_typ_tdef sg = function
  | Abstract_typ.Type_atom(i,tl) -> 
      (try print_string ("("^(string_of_int i)^")");
	print_string (Signature.string_of_atom i sg)
      with _ -> print_int i)
  | Abstract_typ.Linear_arrow(td1,td2) ->
      print_string "(";display_typ_tdef sg td1;
      print_string " -> ";display_typ_tdef sg td2;print_string ")";

(* display a list of variables associated with there de Bruijn index *)
and display_index_assoc = function
  | [] -> print_string "]"
  | [(s,i)] -> print_string "(";print_string s; print_string ",";
      print_int i;print_string ")]"
  | (s,i)::ls -> print_string "(";print_string s; print_string ",";
      print_int i;print_string ");";display_index_assoc ls;

(* display a record characteristic of a term *)
and display_rec_term sg rec_term =
  print_string "\n{term = ";
  display_term sg rec_term.term;
  print_string "\nwfterm = ";
  (match rec_term.wfterm with
    None -> print_string "None"
  | Some t -> print_string "Some ";
      display_wfterm sg t);
  print_string ";\n typeofterm = ";
  (match rec_term.typeofterm with
    None -> print_string "None"
  | Some t -> print_string "Some ";
      display_typ_tdef sg t);
  print_string "}\n";

(*and chg_synt_kd = function 
  | Abstract_sig.K of tdl -> 
      Abstract_typ.K of (List.map chg_synt_td tdl)
*)
and chg_synt_td index_assoc sg = function
  | Abstract_sig.Type_atom(s,_,tl) ->
	  Abstract_typ.Type_atom((Signature.get_atom_ind sg s),(List.map (chg_synt_t index_assoc sg) tl))
  | Abstract_sig.Linear_arrow(td1,td2,_) ->
      Abstract_typ.Linear_arrow((chg_synt_td index_assoc sg td1),
				(chg_synt_td index_assoc sg td2))

and chg_synt_t index_assoc sg = function
    | Abstract_sig.Var(s,_) -> 
	let s_index = List.assoc s index_assoc in
	Abstract_typ.Var s_index
    | Abstract_sig.Const(s,_) -> 
	  Abstract_typ.Const (Signature.get_const_ind sg s)
    | Abstract_sig.LAbs(s,t,_) ->
	Abstract_typ.LAbs(s,chg_synt_t index_assoc sg t)
    | Abstract_sig.App (t1,t2,_) ->
	Abstract_typ.App((chg_synt_t index_assoc sg t1),
			 (chg_synt_t index_assoc sg t2))

(* and chg_synt_tk = function *)
(*     | Abstract_sig.Default -> Abstract_typ.Default  *)
(*     | Abstract_sig.Prefix ->Abstract_typ.Prefix *)
(*     | Abstract_sig.Infix  -> Abstract_typ.Infix *)
(*     | Abstract_sig.Binder -> Abstract_typ.Binder *)

(* typecheck a definition or a declaration of a type or a term *)
and typecheck_entry sg = function 
  | Abstract_sig.Type_decl(s,loc,((Abstract_sig.K tdefs) as k)) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_kd = typecheck_kind sg tdefs in
      let new_sg = Signature.insert_type_dcl s 
	  (Abstract_typ.K new_kd) sg in
      new_sg

  | Abstract_sig.Term_decl(s,tk,loc,typ) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_td = 
	typecheck_type sg typ in
      let new_sg = Signature.insert_term_dcl s tk new_td sg in
      new_sg

  | Abstract_sig.Term_def(s,tk,loc,t,typ) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_td = 
	typecheck_type (*new_*)sg(*???*) typ 
	  in 
      let rec_term = 
	{term = t;
	 wfterm = None;
	 typeofterm = Some new_td
       } in
      let (new_rec,new_sg) = typecheck_term  rec_term sg in
      (match new_rec.wfterm with
	None -> type_error(Other loc)
      | Some wf -> 
(*	  let new_sg = 
	    Signature.insert_var s Abstract_sig.Default new_td sg 
*)
	  let new2_sg = 
	    Signature.insert_term_def s tk wf new_td new_sg in
	  new2_sg)	
(* 	  (rec_term.type_env, *)
(* 	   (add_env s loc typ rec_term.var_env), *)
(* 	   Abstract_typ.Term_def(s,new_tk,wf, *)
(* 				 (chg_synt_td new_rec.index_assoc typ)))) *)
  | _ -> raise(Not_yet_implemented "typecheck_entry")
	    
(* typecheck a kind *)
and typecheck_kind sg = function
  | [] ->  []
	(** WF kinds : axiom *)
  | tdef::tdefs ->
      (** WF kinds : kind formation *)
      let new_tdef = 
	typecheck_type sg tdef in
      let res = typecheck_kind sg tdefs in
      new_tdef::res


(* typecheck a type *)      	
and typecheck_type sg = function
  | Abstract_sig.Type_atom (s,(pos1,pos2),[]) -> 
      (** WF types : type constant *)
      (try 
	let (_,type_s) = Signature.get_atom sg s
(*List.assoc s type_env*) 
	in 
	if type_s = Abstract_typ.K []
	then 
	      Abstract_typ.Type_atom((Signature.get_atom_ind sg s),[])
	else type_error (Not_well_kinded_type(s,pos1,pos2))
      with Not_found -> 
	type_error (Not_defined_var(s,pos1,pos2)))
  | Abstract_sig.Linear_arrow(tdef1,tdef2,_) -> 
      (** WF types : linear function *)
            let new_td1 = 
	      typecheck_type sg tdef1 in
            let new_td2 = 
	     typecheck_type sg tdef2 in
	     Abstract_typ.Linear_arrow(new_td1,new_td2)

  | _ -> raise(Not_yet_implemented "typecheck_type")


(* typecheck a term *)
and typecheck_term rec_term sg = 
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) ->       
      (** WT terms : variable *)
      (try 
	let (_,_,type_s) = Signature.get_const sg s
(*List.assoc s rec_term.var_env*) in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    if eq_typ type_s ty
	    then 
(* 	      let n_td = (\*?? *\) *)
(* 		typecheck_type  *)
(* (\*		  (List.remove_assoc s rec_term.var_env) *\) *)
(* 		  sg  *)
(* 		  ty *)
(* 	      in *)
	      try
		let s_index = Signature.get_const_ind sg s
(*List.assoc s rec_term.index_assoc*)
		in
		rec_term.wfterm <- Some (Abstract_typ.Var s_index);
		(rec_term,sg)
	      with Not_found -> 
		type_error(Not_defined_var(s,pos1,pos2))
	    else type_error(Not_well_typed_term(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))
	
  | Abstract_sig.Const(s,(pos1,pos2)) ->
      (** WT terms : constant *)
      (try 
	let (_,_,type_s) = Signature.get_const sg s
	(*let type_s = List.assoc s rec_term.var_env*) in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    if eq_typ type_s ty
	    then 
	      (
	       rec_term.wfterm <- 
		 Some (Abstract_typ.Const (Signature.get_const_ind sg s));
	       (rec_term,sg))
	    else type_error (Not_well_typed_term(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.LAbs(s,t,loc) ->   
      (** WT terms linear abstraction *)
      (match rec_term.typeofterm with
	None -> type_error (Other loc)
      | Some (Abstract_typ.Type_atom(styp,terml)) -> 
	  raise(Typing_error "typecheck_term Cas LAbs")
      | Some (Abstract_typ.Linear_arrow(tdef1,tdef2)) -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = Some tdef2
	   }
	  in
	  let new_sg = 
	    Signature.insert_var s Abstract_sig.Default tdef1 sg 
	  in
	  let (res,new2_sg) = typecheck_term new_rec new_sg
	  in      
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      res.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  (res,(*new2_*)sg)
      )

  | Abstract_sig.App(t1,t2,loc) ->
      let (type_t2, new_rec_term) = 
	typeinf_term {term = t2;
		      wfterm = None;
		      typeofterm = None
		      } None sg
      in new_rec_term.typeofterm <- Some (type_t2); (* new_rec a mettre dans le nouveau record? *)
      match rec_term.typeofterm with 
	None -> type_error (Other loc)
      | Some type_app -> 
          let (res,new_sg) = 
	    typecheck_term {term = t1;
			    wfterm = None;
			    typeofterm = Some (Abstract_typ.Linear_arrow(type_t2,type_app))
			  }
	      sg
	  in
	  (match (res.wfterm,new_rec_term.wfterm) with
	    (None,_) -> type_error (Other loc)
	  | (_,None) -> type_error (Other loc)
	  | (Some r1,Some r2) ->
	      res.wfterm <- Some (Abstract_typ.App(r1,r2)));
	  (res,new_sg)
(*raise(Not_yet_implemented "typecheck_term Cas App")*)
	    
and typeinf_term rec_term typelabs sg =
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) -> 
      (try 
	let (_,_,type_s) = Signature.get_const sg s
(*let type_s = List.assoc s rec_term.var_env *)in 
	try
	  let s_index = Signature.get_const_ind sg s
(*List.assoc s rec_term.index_assoc*)
	  in
	  rec_term.wfterm <- Some (Abstract_typ.Var s_index);
	  (type_s,rec_term)
	with Not_found ->
	  type_error(Not_defined_var(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.Const(s,(pos1,pos2)) ->
      (try 
	let (_,_,type_s) = Signature.get_const sg s
(*	let type_s = List.assoc s rec_term.var_env*) in 
	rec_term.wfterm <- Some (Abstract_typ.Const (Signature.get_const_ind sg s));
	(type_s,rec_term)
      with Not_found ->
	type_error (Not_defined_const(s,pos1,pos2)))

  | Abstract_sig.LAbs(s,t,loc) ->  
      (match typelabs with
	None -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = None}
	  in
	  let new_sg = 
	    try 	    
	      Signature.insert_var s Abstract_sig.Default (new_type loc) sg 
	    with _ -> raise (Typing_error "errr")
	  in
	  let (td,res) = typeinf_term new_rec None (* ? *) new_sg
	  in
(* 	  let new_td =  *)
(* 	    typecheck_type sg td *)
(* 	  in  *)
	  (match res.wfterm with 
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      rec_term.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  let s_type =
	    (try  let (_,_,tdef)= Signature.get_const new_sg s  in tdef
	    with Not_found -> new_type loc)
	  in
	  (Abstract_typ.Linear_arrow(s_type,td),rec_term)
      | Some s_type -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = None} in
	  let new_sg = 
	    try Signature.insert_var s Abstract_sig.Default s_type sg 
	    with _ -> raise (Typing_error "errr")
	  in
	  let (td,res) = typeinf_term new_rec None (* ? *) new_sg in
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      rec_term.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  (Abstract_typ.Linear_arrow(s_type,td),rec_term)
      )    
  | Abstract_sig.App(t1,t2,((pos1,pos2) as loc)) -> 
      let (t2_type, new_rec_t2) = 
	typeinf_term {term = t2;
		      wfterm = None;
		      typeofterm = None
		      } None (* ? *) sg
      in new_rec_t2.typeofterm <- Some(t2_type);
      let (t1_type,new_rec_t1) =
	typeinf_term {term = t1;
		      wfterm = None;
		      typeofterm = None
		      } (Some t2_type) sg in
      (match (new_rec_t2.wfterm,new_rec_t1.wfterm) with
	(None,_) -> type_error (Other loc)
      | (_,None) -> type_error (Other loc)
      | (Some r1,Some r2) ->
	  new_rec_t1.wfterm <- Some (Abstract_typ.App(r2,r1));
	      
	  match t1_type with
	  | Abstract_typ.Type_atom(_,_) -> type_error (Other loc)
	  | Abstract_typ.Linear_arrow(td1,td2) ->
(* 	    let new_td1 =  *)
(* 	      typecheck_type sg td1 *)
(* 	    in  *)
(* 	    let new_t2 =  *)
(* 	      typecheck_type sg t2_type *)
(* 	    in  *)
	      if eq_typ td1 t2_type
	      then ((td2,new_rec_t1))
	      else type_error (Not_well_typed_term((Abstract_typ.term_to_string r2 sg),pos1,pos2)))
	

(* create a fresh type at the location loc *)
and new_type loc =
  type_nb := !type_nb + 1;
  let s = !type_nb in
  new_type_list := (s,None) :: !new_type_list; 
  Abstract_typ.Type_atom(s,[]) 

(* typecheck entries in sig_entries with empty type env., variable env *)
(* and ? *)
let typecheck (sig_name,content :string * Abstract_sig.sig_content) = 
  typecheck_sig (*[] [] *)
    (Signature.create(sig_name,
    (* (Abstract_typ.Signature (sig_name,0,Table.create(),Tries.empty, *)
(* 	       {Abstract_typ.type_definitions = *)
(* 		content.Abstract_sig.type_definitions; *)
(* 		Abstract_typ.term_definitions = *)
(* 		content.Abstract_typ.term_definitions})) *)
		      (Abstract_typ.content2content content)))
    content.Abstract_sig.entries
      

;;
