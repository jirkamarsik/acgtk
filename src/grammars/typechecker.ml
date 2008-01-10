open Utils
open Printf
open Tries
open Abstract_syntax
open Error

exception Typing_error of string
exception Not_yet_implemented of string
    
type symbol = string 
      
type type_env = (symbol * Abstract_sig.kind) list
      
type lin_var_env = (symbol * symbol) list
      
type var_env = (symbol * Abstract_sig.type_def) list
 
type index_assoc = (symbol * int) list

type rec_term = 
    {term: Abstract_sig.term;
     mutable wfterm: Abstract_typ.term option;
     mutable typeofterm: Abstract_sig.type_def option;
     mutable type_env: type_env;
     mutable var_env: var_env;
     mutable index_assoc: index_assoc;
     n: int}

type new_type_list = (symbol * Abstract_sig.type_def option) list ref

let type_error e = raise (Error (Type_error e))


let type_nb = ref 0
let new_type_list : new_type_list = ref []
     
let  type_env = []
    
(*let lin_var_env = ref []*)
    
let var_env = []
    
let index_assoc = [] 

(* add an term elt of type typ in the environment *)
let add_env elt (pos1,pos2) typ env =
  try List.assoc elt env;
    type_error (Error.Already_defined_var (elt,pos1,pos2))
  with Not_found -> (elt,typ)::env



let rec typecheck_sig type_env var_env sig_ent = function (* sg.entries *) 
  | [] -> (type_env,var_env,sig_ent)
  | last_entry::entries -> 
      let (new_type_env,new_var_env,sig_ent) = 
	typecheck_sig type_env var_env sig_ent entries
      in
      let (nt,nv,new_entry(*wfterm*)) =
      typecheck_entry new_type_env new_var_env entries last_entry
      in
      (nt,nv,List.append sig_ent [new_entry])
	
(* equiv tdef trepl returns a type_def*)
(* replace tdef by the type_def trepl if tdef is not defined *)
and equiv tdef trepl =
  match tdef with
    Abstract_sig.Type_atom(s,_,_) ->
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
    (Abstract_sig.Type_atom(s,_,tl),Abstract_sig.Type_atom(sbis,_,tlbis)) -> (s = sbis) && (tl = tlbis)
  | (Abstract_sig.Linear_arrow(td1,td2,_),Abstract_sig.Linear_arrow(td1bis,td2bis,_)) ->
      (eq_typ td1 td1bis) && (eq_typ td2 td2bis)
  | _ -> false


(* increments indexes *)
and inc_index = function
    [] -> []
  | (s,i)::ls -> (s,i+1)::(inc_index ls)

(* display a list of types *)
and display_type_env = function
  | [] -> print_string "[]\n";
  | [(x,_)] -> print_string "(";
      print_string x; print_string ",";
      print_string ")]";
  | (x,_)::l -> print_string "(";
      print_string x; print_string ",";
      print_string ");"; display_type_env l

(* display a list of variables with there types *)
and display_var_env = function
  | [] -> print_string "\n";
  | [(x,y)] -> print_string "(";
      print_string x; print_string ","; display_tdef y;
      print_string ")]";
  | (x,y)::l -> print_string "(";
      print_string x; print_string ","; display_tdef y;
      print_string ");"; display_var_env l

(* display a term before typechecking *)
and display_term = function
  | Abstract_sig.Var(s,_) -> print_string s
  | Abstract_sig.Const(s,_) -> print_string s
  | Abstract_sig.LAbs(s,t,_) -> 
      print_string "(lambda ";print_string s;print_string ". "; display_term t;
      print_string ")"
  | Abstract_sig.App(t1,t2,_) -> display_term t1;print_string " ";display_term t2

(* display a term well typed *)
and display_wfterm = function
  | Abstract_typ.Var(i) -> print_int i
  | Abstract_typ.Const(s) -> print_string s
  | Abstract_typ.LAbs(s,t) -> 
      print_string "(lambda ";print_string s;print_string ". "; display_wfterm t;
      print_string ")"
  | Abstract_typ.App(t1,t2) -> display_wfterm t1;print_string " ";display_wfterm t2

(* display a type before typechecking *)
and display_tdef = function
  | Abstract_sig.Type_atom(s,_,tl) -> print_string s
  | Abstract_sig.Linear_arrow(td1,td2,_) ->
      print_string "(";display_tdef td1;
      print_string " -> ";display_tdef td2;print_string ")";

and display_typ_tdef = function
  | Abstract_typ.Type_atom(s,tl) -> print_string s
  | Abstract_typ.Linear_arrow(td1,td2) ->
      print_string "(";display_typ_tdef td1;
      print_string " -> ";display_typ_tdef td2;print_string ")";

(* display a list of variables associated with there de Bruijn index *)
and display_index_assoc = function
  | [] -> print_string "]"
  | [(s,i)] -> print_string "(";print_string s; print_string ",";
      print_int i;print_string ")]"
  | (s,i)::ls -> print_string "(";print_string s; print_string ",";
      print_int i;print_string ");";display_index_assoc ls;

(* display a record characteristic of a term *)
and display_rec_term rec_term =
  print_string "\n{term = ";
  display_term rec_term.term;
  print_string "\nwfterm = ";
  (match rec_term.wfterm with
    None -> print_string "None"
  | Some t -> print_string "Some ";
      display_wfterm t);
  print_string ";\n typeofterm = ";
  (match rec_term.typeofterm with
    None -> print_string "None"
  | Some t -> print_string "Some ";
      display_tdef t);
  print_string ";\n type_env = [";
  display_type_env rec_term.type_env;
  print_string ";\n var_env = [";
  display_var_env rec_term.var_env;
  print_string ";\n index_assoc = [";
  display_index_assoc rec_term.index_assoc;
  print_string ";\n n = ";
  print_int rec_term.n;
  print_string "}\n";

(*and chg_synt_kd = function 
  | Abstract_sig.K of tdl -> 
      Abstract_typ.K of (List.map chg_synt_td tdl)
*)
and chg_synt_td i = function
  | Abstract_sig.Type_atom(s,_,tl) ->
      Abstract_typ.Type_atom(s,(List.map (chg_synt_t i) tl))
  | Abstract_sig.Linear_arrow(td1,td2,_) ->
      Abstract_typ.Linear_arrow((chg_synt_td i td1),(chg_synt_td i td2))

and chg_synt_t i = function
    | Abstract_sig.Var(s,_) -> 
	let s_index = List.assoc s i in
	Abstract_typ.Var s_index
    | Abstract_sig.Const(s,_) -> Abstract_typ.Const s
    | Abstract_sig.LAbs(s,t,_) ->
	Abstract_typ.LAbs(s,chg_synt_t i t)
    | Abstract_sig.App (t1,t2,_) ->
	Abstract_typ.App((chg_synt_t i t1),(chg_synt_t i t2))

and chg_synt_tk = function
    | Abstract_sig.Default -> Abstract_typ.Default 
    | Abstract_sig.Prefix ->Abstract_typ.Prefix
    | Abstract_sig.Infix  -> Abstract_typ.Infix
    | Abstract_sig.Binder -> Abstract_typ.Binder

(* typecheck a definition or a declaration of a type or a term *)
and typecheck_entry type_env var_env sg = function 
  | Abstract_sig.Type_decl(s,loc,((Abstract_sig.K tdefs) as k)) -> 
      let new_kd = typecheck_kind type_env var_env tdefs in
      let (new_type_env,new_var_env) = 
	((add_env s loc k type_env), var_env) in
      (*(new_type_env,new_var_env,[])*)
      (new_type_env,new_var_env,Abstract_typ.Type_decl(s,Abstract_typ.K new_kd))

  | Abstract_sig.Term_decl(s,tk,loc,typ) -> 
      let (new_type_env,new_var_env,new_td) = 
	typecheck_type type_env var_env typ in
      (*(type_env,(add_env s typ var_env),[])*)
      let new_tk = chg_synt_tk tk in
      (type_env,(add_env s loc typ var_env),Abstract_typ.Term_decl(s,new_tk,new_td))

  | Abstract_sig.Term_def(s,tk,loc,t,typ) -> 
      let rec_term = 
	{term = t;
	 wfterm = None;
	 typeofterm = Some typ;
	 type_env = type_env;
	 var_env = var_env;
	 index_assoc = [];
	 n = 0} in
      let new_rec = typecheck_term  rec_term in
      (match new_rec.wfterm with
	None -> type_error(Other loc)
      | Some wf -> 
	  let new_tk = chg_synt_tk tk in
	  (rec_term.type_env,
	   (add_env s loc typ rec_term.var_env),
	   Abstract_typ.Term_def(s,new_tk,wf,
				 (chg_synt_td new_rec.index_assoc typ))))
      | _ -> raise(Not_yet_implemented "typecheck_entry")
	    
(* typecheck a kind *)
and typecheck_kind type_env var_env = function
  | [] ->  []
	(** WF kinds : axiom *)
  | tdef::tdefs ->
      (** WF kinds : kind formation *)
      let (new_type_env,new_var_env,new_tdef) = 
	typecheck_type type_env var_env tdef in
      let res = typecheck_kind new_type_env new_var_env tdefs in
      new_tdef::res


(* typecheck a type *)      	
and typecheck_type type_env var_env = function
  | Abstract_sig.Type_atom (s,(pos1,pos2),[]) -> 
      (** WF types : type constant *)
      (try 
	let type_s = List.assoc s type_env in 
	if type_s = Abstract_sig.K []
	then (type_env,var_env,Abstract_typ.Type_atom(s,[]))
	else type_error (Not_well_kinded_type(s,pos1,pos2))
      with Not_found ->
	type_error (Not_defined_var(s,pos1,pos2)))
  | Abstract_sig.Linear_arrow(tdef1,tdef2,_) -> 
      (** WF types : linear function *)
            let (new_type_env,new_var_env,_) = 
	      typecheck_type type_env var_env tdef1 in
	    typecheck_type new_type_env new_var_env tdef2
  | _ -> raise(Not_yet_implemented "typecheck_type")


(* typecheck a term *)
and typecheck_term rec_term  = 
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) ->       
      (** WT terms : variable *)
      (try 
	let type_s = List.assoc s rec_term.var_env in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    if eq_typ type_s ty
	    then 
	      let (new_type_env, new_var_env,_) =
		typecheck_type 
		  rec_term.type_env 
		  (List.remove_assoc s rec_term.var_env) 
		  ty
	      in
	      rec_term.type_env <- new_type_env;
	      rec_term.var_env <- new_var_env;
	      try
		let s_index = List.assoc s rec_term.index_assoc
		in
		rec_term.wfterm <- Some (Abstract_typ.Var s_index);
		rec_term
	      with Not_found -> 
		type_error(Not_defined_var(s,pos1,pos2))
	    else type_error(Not_well_typed_term(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))
	
  | Abstract_sig.Const(s,(pos1,pos2)) ->
      (** WT terms : constant *)
      (try 
	let type_s = List.assoc s rec_term.var_env in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    if eq_typ type_s ty
	    then 
	      (rec_term.wfterm <- Some (Abstract_typ.Const s);
	       rec_term)
	    else 
	      type_error (Not_well_typed_term(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.LAbs(s,t,loc) ->   
      (** WT terms linear abstraction *)
      (match rec_term.typeofterm with
	None -> type_error (Other loc)
      | Some (Abstract_sig.Type_atom(styp,_,terml)) -> 
	  raise(Typing_error "typecheck_term Cas LAbs")
      | Some (Abstract_sig.Linear_arrow(tdef1,tdef2,loc)) -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = Some tdef2;
	     type_env = rec_term.type_env;
	     var_env = (add_env s loc tdef1 rec_term.var_env);
	     index_assoc = ((s,1)::(inc_index rec_term.index_assoc));
	     n = rec_term.n + 1}
	  in
	  let res = typecheck_term new_rec
	  in      
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      res.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  res
      )

  | Abstract_sig.App(t1,t2,loc) ->
      let (tfound, new_rec_term) = 
	typeinf_term {term = t2;
		      wfterm = None;
		      typeofterm = None;
		      type_env = rec_term.type_env;
		      var_env = rec_term.var_env;
		      index_assoc = (rec_term.index_assoc);
		      n = rec_term.n} None
      in new_rec_term.typeofterm <- Some (tfound); (* new_rec a mettre dans le nouveau record? *)
      match rec_term.typeofterm with 
	None -> type_error (Other loc)
      | Some ty -> 
          let res = 
	    typecheck_term {term = t1;
			    wfterm = None;
			    typeofterm = Some (Abstract_sig.Linear_arrow(tfound,ty,loc));
			    type_env = rec_term.type_env;
			    var_env = rec_term.var_env;
			    index_assoc = (rec_term.index_assoc);
			    n = rec_term.n}
	  in
	  (match (res.wfterm,new_rec_term.wfterm) with
	    (None,_) -> type_error (Other loc)
	  | (_,None) -> type_error (Other loc)
	  | (Some r1,Some r2) ->
	      res.wfterm <- Some (Abstract_typ.App(r1,r2)));
	  res
(*raise(Not_yet_implemented "typecheck_term Cas App")*)
	    
and typeinf_term rec_term typelabs =
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) -> 
      (try 
	let type_s = List.assoc s rec_term.var_env in 
	try
	  let s_index = List.assoc s rec_term.index_assoc
	  in
	  rec_term.wfterm <- Some (Abstract_typ.Var s_index);
	  (type_s,rec_term)
	with Not_found ->
	  type_error(Not_defined_var(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.Const(s,(pos1,pos2)) ->
      (try 
	let type_s = List.assoc s rec_term.var_env in 
	rec_term.wfterm <- Some (Abstract_typ.Const s);
	(type_s,rec_term)
      with Not_found ->
	type_error (Not_defined_const(s,pos1,pos2)))

  | Abstract_sig.LAbs(s,t,loc) ->  
      (match typelabs with
	None -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = None;
	     type_env = rec_term.type_env;
	     var_env = (add_env s loc (new_type loc) rec_term.var_env);
	     index_assoc = ((s,1)::(inc_index rec_term.index_assoc));
	     n = rec_term.n + 1}
	  in
	  let (td,res) = typeinf_term new_rec None (* ? *)
	  in
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      res.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  let s_type =
	    (try List.assoc s var_env
	    with Not_found -> new_type(loc))
	  in
	  (Abstract_sig.Linear_arrow(s_type,td,loc),res)
      | Some ty -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = None;
	     type_env = rec_term.type_env;
	     var_env = (add_env s loc ty rec_term.var_env);
	     index_assoc = ((s,1)::(inc_index rec_term.index_assoc));
	     n = rec_term.n + 1}
	  in
	  (match new_rec.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      new_rec.wfterm <- Some (Abstract_typ.LAbs(s,wfterm))
	  );
	  typeinf_term new_rec None (* ? *)
      )    
  | Abstract_sig.App(t1,t2,loc) -> 
      let (t2_type, new_rec_t2) = 
	typeinf_term {term = t2;
		      wfterm = None;
		      typeofterm = None;
		      type_env = rec_term.type_env;
		      var_env = rec_term.var_env;
		      index_assoc = (rec_term.index_assoc);
		      n = rec_term.n} None (* ? *)
      in new_rec_t2.typeofterm <- Some(t2_type);
      let (t1_type,new_rec_t1) =
	typeinf_term {term = t1;
		      wfterm = None;
		      typeofterm = None;
		      type_env = rec_term.type_env;
		      var_env = rec_term.var_env;
		      index_assoc = (rec_term.index_assoc);
		      n = rec_term.n} (Some t2_type) in
      (match (new_rec_t2.wfterm,new_rec_t1.wfterm) with
	(None,_) -> type_error (Other loc)
      | (_,None) -> type_error (Other loc)
      | (Some r1,Some r2) ->
	  new_rec_t1.wfterm <- Some (Abstract_typ.App(r2,r1)));
      
      match t1_type with
      | Abstract_sig.Type_atom(_,loc,_) -> type_error (Other loc)
      | Abstract_sig.Linear_arrow(td1,td2,(pos1,pos2)) ->
	  if eq_typ td1 t2_type
	  then ((td2,new_rec_t1))
	  else type_error (Not_well_typed_term("",pos1,pos2))


(* create a fresh type at the location loc *)
and new_type(loc) =
  type_nb := !type_nb + 1;
  let s = "newtype"^(string_of_int !type_nb) in
  new_type_list := (s,None) :: !new_type_list; 
  Abstract_sig.Type_atom(s,loc,[]) 

(* typecheck entries in sig_entries with empty type env., variable env *)
(* and ? *)
let typecheck sig_entries = typecheck_sig [] [] [] sig_entries

;;
