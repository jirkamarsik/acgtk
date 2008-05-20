open Utils
open Printf
open Tries
open Table
open Abstract_syntax
open Error
open Lambda
open Sign

exception Typing_error of string
exception Not_yet_implemented of string
      
 module Table = Make_table (struct let b = 10 end)
 
type symbol = string 
      
type type_env = (symbol * Abstract_sig.kind) list
      
type rec_term = 
    {term: Abstract_sig.term;
     mutable wfterm: Lambda.term option;
     mutable typeofterm: Lambda.type_def option;
     mutable ind_assoc: (string * int) list;
   }

type new_type_list = (int * Lambda.type_def option) list ref

let type_error e = raise (Error (Type_error e))


let type_nb = ref 100
let new_type_list : new_type_list = ref []
     
let  type_env = []
    
(* add a term elt of type typ in the environment *)
let add_env elt (pos1,pos2) typ env =
  try List.assoc elt env;
    type_error (Error.Already_defined_var (elt,pos1,pos2))
  with Not_found -> (elt,typ)::env


let rec term_to_string sg = function
  | Abstract_sig.Var(s,_) -> s
  | Abstract_sig.Const(s,_) -> s
  | Abstract_sig.LAbs(s,t,_) -> 
      "(lambda "^s^". "^( term_to_string sg t)^")"
  | Abstract_sig.App(t1,t2,_) -> (term_to_string sg t1)^" "^(term_to_string sg t2)
	    

let rec typecheck_sig ind_assoc sig_new = function
  | [] -> (sig_new,ind_assoc)
  | last_entry::entries -> 
      let (sig_new2,ind_assoc2) = 
	typecheck_sig ind_assoc sig_new entries
      in
      typecheck_entry ind_assoc2 sig_new2 last_entry

	
(* equiv tdef trepl returns a type_def*)
(* replace tdef by the type_def trepl if tdef is not defined *)
and equiv tdef trepl =
  match tdef with
    Lambda.Type_atom(s,_) ->
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
and eq_typ t1 as1 t2 as2 sg = 
  print_string "EQ_TYP\n";
      print_string "t1 : ";
      print_string (Sign.type_def_to_string as1 t1 sg);
      print_string "\nt2 : ";
      print_string (Sign.type_def_to_string as2 t2 sg);
  match (equiv t1 t2, equiv t2 t1) with
    (Lambda.Type_atom(s,tl),Lambda.Type_atom(sbis,tlbis)) -> 
(*       let ind_assoc1 = Sign.cut_assoc as1 s *)
(*       and ind_assoc2 = Sign.cut_assoc as2 sbis in *)
      print_string "t1 : ";
      let s1 = (Sign.type_def_to_string as1 t1 sg) in
      print_string (Sign.type_def_to_string as1 t1 sg);
      print_string "\nt2 : ";
      let s2 = (Sign.type_def_to_string as2 t2 sg) in
      print_string (Sign.type_def_to_string as2 t2 sg);
      (s1 == s2)
(*       (s = sbis) && (tl = tlbis) *)
  | (Lambda.Linear_arrow(td1,td2),Lambda.Linear_arrow(td1bis,td2bis)) ->
      (eq_typ td1 as1 td1bis as2 sg) && (eq_typ td2 as1 td2bis as2 sg)
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
  | Lambda.Var(i) -> print_string ("("^(string_of_int i)^")");
(*       (try *)
(* 	print_string (Sign.string_of_const i sg) *)
(*       with _ -> raise (Typing_error(string_of_int i))) *)
  | Lambda.Const(i) -> print_string ("("^(string_of_int i)^")");
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
      (try print_string ("("^(string_of_int i)^")");
	print_string (Sign.string_of_atom i sg)
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

(*and chg_synt_kd = function 
  | Abstract_sig.K of tdl -> 
      Lambda.K of (List.map chg_synt_td tdl)
*)
and chg_synt_td index_assoc sg = function
  | Abstract_sig.Type_atom(s,_,tl) ->
	  Lambda.Type_atom((Sign.get_atom_ind sg s),(List.map (chg_synt_t index_assoc sg) tl))
  | Abstract_sig.Linear_arrow(td1,td2,_) ->
      Lambda.Linear_arrow((chg_synt_td index_assoc sg td1),
				(chg_synt_td index_assoc sg td2))

and chg_synt_t index_assoc sg = function
    | Abstract_sig.Var(s,_) -> 
	let s_index = List.assoc s index_assoc in
	Lambda.Var s_index
    | Abstract_sig.Const(s,_) -> 
	  Lambda.Const (Sign.get_const_ind sg s)
    | Abstract_sig.LAbs(s,t,_) ->
	Lambda.LAbs(s,chg_synt_t index_assoc sg t)
    | Abstract_sig.App (t1,t2,_) ->
	Lambda.App((chg_synt_t index_assoc sg t1),
			 (chg_synt_t index_assoc sg t2))

(* and chg_synt_tk = function *)
(*     | Abstract_sig.Default -> Abstract_typ.Default  *)
(*     | Abstract_sig.Prefix ->Abstract_typ.Prefix *)
(*     | Abstract_sig.Infix  -> Abstract_typ.Infix *)
(*     | Abstract_sig.Binder -> Abstract_typ.Binder *)

(* typecheck a definition or a declaration of a type or a term *)
and typecheck_entry ind_assoc sg = function 
  | Abstract_sig.Type_decl(s,loc,((Abstract_sig.K tdefs) as k)) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_kd = 
	typecheck_kind new_ind_assoc sg tdefs in
      let new_sg = Sign.insert_type_dcl s 
	  (Lambda.K new_kd) sg in
      (new_sg,new_ind_assoc)

  | Abstract_sig.Term_decl(s,tk,loc,typ) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_td = 
	typecheck_type new_ind_assoc sg typ in
      let new_sg = Sign.insert_term_dcl s tk new_td sg in
      (new_sg,new_ind_assoc)

  | Abstract_sig.Term_def(s,tk,loc,t,typ) -> 
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_td = 
	typecheck_type new_ind_assoc (*new_*)sg(*???*) typ 
	  in 
      let rec_term = 
	{term = t;
	 wfterm = None;
	 typeofterm = Some new_td;
	 ind_assoc = [];
       } in
(*       print_string "appel typecheck_term 1\n"; *)
      let (new_rec,new_sg) = typecheck_term new_ind_assoc rec_term sg in
(*       print_string "fin appel 1\n"; *)
      (match new_rec.wfterm with
	None -> type_error(Other loc)
      | Some wf -> 
(*	  let new_sg = 
	    Sign.insert_var s Abstract_sig.Default new_td sg 
*)
	  let new2_sg = 
	    Sign.insert_term_def s tk wf new_td new_sg in
(*	  display_signature new2_sg;*)
	  (new2_sg,new_ind_assoc))	
(* 	  (rec_term.type_env, *)
(* 	   (add_env s loc typ rec_term.var_env), *)
(* 	   Abstract_typ.Term_def(s,new_tk,wf, *)
(* 				 (chg_synt_td new_rec.index_assoc typ)))) *)
  | _ -> raise(Not_yet_implemented "typecheck_entry")
	    
(* typecheck a kind *)
and typecheck_kind ind_assoc sg = function
  | [] ->  []
	(** WF kinds : axiom *)
  | tdef::tdefs ->
      (** WF kinds : kind formation *)
      let new_tdef = 
	typecheck_type ind_assoc sg tdef in
      let res = typecheck_kind ind_assoc sg tdefs in
      new_tdef::res


(* typecheck a type *)      	
and typecheck_type ind_assoc sg = function
  | Abstract_sig.Type_atom (s,(pos1,pos2),[]) -> 
      (** WF types : type constant *)
      (try 
	let (_,type_s) = Sign.get_atom sg s
(*List.assoc s type_env*) 
	in 
	if type_s = Lambda.K []
	then 
	      Lambda.Type_atom((Sign.get_ind ind_assoc s),[])
	else type_error (Not_well_kinded_type(s,pos1,pos2))
      with Not_found -> 
	type_error (Not_defined_var(s,pos1,pos2)))
  | Abstract_sig.Linear_arrow(tdef1,tdef2,_) -> 
      (** WF types : linear function *)
            let new_td1 = 
	      typecheck_type ind_assoc sg tdef1 in
            let new_td2 = 
	     typecheck_type ind_assoc sg tdef2 in
	     Lambda.Linear_arrow(new_td1,new_td2)

  | _ -> raise(Not_yet_implemented "typecheck_type")


(* typecheck a term *)
and typecheck_term ind_assoc rec_term sg = 
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) ->       
      (** WT terms : variable *)
      (try 
	let (_,_,type_s) = Sign.get_const sg s
(*List.assoc s rec_term.var_env*) in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    if eq_typ type_s ind_assoc ty rec_term.ind_assoc sg
	    then 
(* 	      let n_td = (\*?? *\) *)
(* 		typecheck_type  *)
(* (\*		  (List.remove_assoc s rec_term.var_env) *\) *)
(* 		  sg  *)
(* 		  ty *)
(* 	      in *)
	      try
		let s_index = Sign.get_ind ind_assoc s
(*List.assoc s rec_term.index_assoc*)
		in
		rec_term.wfterm <- Some (Lambda.Var s_index);
		(rec_term,sg)
	      with Not_found -> 
		type_error(Not_defined_var(s,pos1,pos2))
	    else 
	      (print_string "erreur 1"; 
(* 	       Sign.display_assoc ind_assoc; *)
	       let old_ind_assoc = Sign.cut_assoc ind_assoc s in
	       type_error(Not_well_typed_term(s,Sign.type_def_to_string old_ind_assoc type_s sg,pos1,pos2)))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))
	
  | Abstract_sig.Const(s,(pos1,pos2)) ->
      (** WT terms : constant *)
      (try 
	let (_,_,type_s) = Sign.get_const sg s
	(*let type_s = List.assoc s rec_term.var_env*) in 
	match rec_term.typeofterm with 
	  None -> type_error (Other(pos1,pos2))
	| Some ty -> 
	    print_string "AVANT eq_typ : ";
	    display_typ_tdef sg type_s;
	    Sign.display_assoc (Sign.cut_assoc ind_assoc s);
	    print_newline();
	    display_typ_tdef sg ty;
	    Sign.display_assoc ind_assoc;
	    print_newline();
	    if eq_typ type_s (Sign.cut_assoc ind_assoc s) ty ind_assoc sg
	    then 
	      (
	       rec_term.wfterm <- 
		 Some (Lambda.Const (Sign.get_ind ind_assoc s));
	       (rec_term,sg))
	    else 
	      (print_string "erreur 2 : "; 
	       display_typ_tdef sg type_s;
	       print_newline();
	       display_typ_tdef sg ty;
	       print_newline();
	       print_string s;
(* 	       Sign.display_assoc ind_assoc; *)
	       let old_ind_assoc = Sign.cut_assoc ind_assoc s in
(* 	       display_rec_term sg rec_term; *)
(* 	       print_string s; *)
(* 	       print_string "\ntype_s :\n"; *)
(* 	       display_typ_tdef sg type_s; *)
(* 	       print_string "\nty :\n"; *)
(* 	       display_typ_tdef sg ty; *)
(* 	       print_newline(); *)
	       type_error (Not_well_typed_term(s,Sign.type_def_to_string old_ind_assoc type_s sg,pos1,pos2)))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.LAbs(s,t,loc) -> 
      print_string "LABS : lambda ";
      print_string s;
      print_string ". ";
      display_term sg t;
      print_newline();
      (** WT terms linear abstraction *)
      (match rec_term.typeofterm with
	None -> type_error (Other loc)
      | Some (Lambda.Type_atom(styp,terml)) -> 
	  let (pos1,pos2) = loc in
 	  print_string "erreur 3"; 
	  type_error (Not_well_typed_term((term_to_string sg rec_term.term),("a' -> b'"),pos1,pos2))
(* 	  (match rec_term.wfterm with *)
(* 	    None -> type_error (Not_well_typed_term(s,("a' -> b'"),pos1,pos2)) *)
(* 	  | Some wft -> *)
(* 	      type_error (Not_well_typed_term((Sign.term_to_string wft ind_assoc sg),("a' -> b'"),pos1,pos2))) *)
(*raise(Typing_error "abstraction without coherent type")*)
      | Some (Lambda.Linear_arrow(tdef1,tdef2)) -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = Some (Sign.inc_type tdef2);
	     ind_assoc = ind_assoc;
	   }
	  in
	  print_string "\t\tAFFICHE tdef1 LABS\n";
	  display_typ_tdef sg (Sign.inc_type tdef1);
(*	  display_signature sg;*)
	  let new_ind_assoc = Sign.add_assoc ind_assoc s in
	  
	  let new_sg = 
	    Sign.insert_var s Abstract_sig.Default (Sign.inc_type tdef1) sg 
	  in
(* 	  Sign.display_assoc new_ind_assoc; *)
	  print_string "\t\tAFFICHE NEW_SG\n";
(*	  display_signature new_sg;*)
	  Sign.display_assoc new_ind_assoc;
 	  print_string "appel typecheck_term 2\n"; 
	  display_rec_term new_sg new_rec;
	  let (_,_,type_s2) = Sign.get_const new_sg s in
	  let i2 = Sign.get_ind new_ind_assoc s in
	  print_string (s^" index : ");
	  print_int i2;
	  print_string " : ";
	  display_typ_tdef new_sg type_s2;
	  print_newline();
(* 	  let (i,_,type_s) = Sign.get_const sg s in *)
(* 	  print_int i; *)
(* 	  print_string " : "; *)
(* 	  display_typ_tdef sg type_s; *)
(* 	  print_newline(); *)
	  let (res,new2_sg) = typecheck_term new_ind_assoc new_rec new_sg
	  in      
 	  print_string "fin appel 2\n"; 
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      res.wfterm <- Some (Lambda.LAbs(s,wfterm))
	  );
	  (res,(*new2_*)sg)
      )

  | Abstract_sig.App(t1,t2,loc) ->
(*       let (i,_,type_s) = Sign.get_const sg "x" in *)
(*       print_int i; *)
(*       print_string " : "; *)
(*       display_typ_tdef sg type_s; *)
(*       print_newline(); *)
(*      print_string "t1 : \n"; *)
(*       display_term sg t1; *)
(*       print_string "\nt2 :\n"; *)
(*       display_term sg t2; *)
(*       print_newline(); *)
      let (type_t2, new_rec_term) = 
	typeinf_term ind_assoc {term = t2;
				wfterm = None;
				typeofterm = None;
				ind_assoc = [];
			      } None sg
      in 
      print_string "new_rec_term2 : ";
      display_rec_term sg new_rec_term;
      display_rec_term sg rec_term;
      
      new_rec_term.typeofterm <- Some (type_t2); (* new_rec a mettre dans le nouveau record? *)
      new_rec_term.ind_assoc <- ind_assoc;
      match rec_term.typeofterm with 
	None -> type_error (Other loc)
      | Some type_app -> 
(* 	  print_string "appel typecheck_term 3\n"; *)
          let (res,new_sg) = 
	    typecheck_term 
	      ind_assoc 
	      {term = t1;
	       wfterm = None;
	       typeofterm = Some (Lambda.Linear_arrow(type_t2,type_app));
	       ind_assoc = ind_assoc
	     }
	      sg
	  in
(* 	  print_string "fin appel 3\n"; *)
	  (match (res.wfterm,new_rec_term.wfterm) with
	    (None,_) -> type_error (Other loc)
	  | (_,None) -> type_error (Other loc)
	  | (Some r1,Some r2) ->
	      res.wfterm <- Some (Lambda.App(r1,r2)));
	  print_string "\n RES = ";
	  display_rec_term new_sg res;
	  (res,new_sg)
(*raise(Not_yet_implemented "typecheck_term Cas App")*)
	    
and typeinf_term ind_assoc rec_term typelabs sg =
  match rec_term.term with
  | Abstract_sig.Var(s,(pos1,pos2)) -> 
      print_string ("Var : "^s^"\n");
      (try 
	let (_,_,type_s) = Sign.get_const sg s
(*let type_s = List.assoc s rec_term.var_env *)in 
	let i = Sign.get_ind ind_assoc s in
	print_string s;
	print_int i;
	print_string " : ";
	display_typ_tdef sg type_s;
	print_newline();
	
	try
	  let s_index = Sign.get_ind ind_assoc s
(*List.assoc s rec_term.index_assoc*)
	  in
	  rec_term.wfterm <- Some (Lambda.Var s_index);
	  (type_s,rec_term)
	with Not_found ->
	  type_error(Not_defined_var(s,pos1,pos2))
      with Not_found -> 
	type_error(Not_defined_var(s,pos1,pos2)))

  | Abstract_sig.Const(s,(pos1,pos2)) ->
      print_string ("Const : "^s^"\n");
      (try 
	let (_,_,type_s) = Sign.get_const sg s
(*	let type_s = List.assoc s rec_term.var_env*) in 
	display_typ_tdef sg type_s;
	rec_term.wfterm <- Some (Lambda.Const (Sign.get_ind ind_assoc s));
	display_rec_term sg rec_term;
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
	     ind_assoc = [];}
	  in
	  let new_sg = 
	    try 	    
	      Sign.insert_var s Abstract_sig.Default (new_type loc) sg 
	    with _ -> raise (Typing_error "errr")
	  in
	  let new_ind_assoc = Sign.add_assoc ind_assoc s in
(* 	  Sign.display_assoc new_ind_assoc; *)
	  let (td,res) = typeinf_term new_ind_assoc new_rec None (* ? *) new_sg
	  in
(* 	  let new_td =  *)
(* 	    typecheck_type sg td *)
(* 	  in  *)
	  (match res.wfterm with 
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      rec_term.wfterm <- Some (Lambda.LAbs(s,wfterm))
	  );
	  let s_type =
	    (try  let (_,_,tdef)= Sign.get_const new_sg s  in tdef
	    with Not_found -> new_type loc)
	  in
	  (Lambda.Linear_arrow(s_type,td),rec_term)
      | Some s_type -> 
	  let new_rec = 
	    {term = t;
	     wfterm = None;
	     typeofterm = None;
	     ind_assoc = [];} in
	  let new_sg = 
	    try Sign.insert_var s Abstract_sig.Default s_type sg 
	    with _ -> raise (Typing_error "errr")
	  in
	  let new_ind_assoc = Sign.add_assoc ind_assoc s in
(* 	  Sign.display_assoc new_ind_assoc; *)
	  let (td,res) = typeinf_term new_ind_assoc new_rec None (* ? *) new_sg in
	  (match res.wfterm with
	    None -> type_error (Other loc)
	  | Some wfterm -> 
	      rec_term.wfterm <- Some (Lambda.LAbs(s,wfterm))
	  );
	  (Lambda.Linear_arrow(s_type,td),rec_term)
      )    
  | Abstract_sig.App(t1,t2,((pos1,pos2) as loc)) -> 
      print_string "APP : \n";
      display_term sg t1;
      display_term sg t2;
      print_newline();
      let (t2_type, new_rec_t2) = 
	typeinf_term ind_assoc {term = t2;
				wfterm = None;
				typeofterm = None;
				ind_assoc = [];
			      } None (* ? *) sg
      in 
      new_rec_t2.typeofterm <- Some(t2_type);
      new_rec_t2.ind_assoc <- ind_assoc;
      let (t1_type,new_rec_t1) =
	typeinf_term ind_assoc {term = t1;
				wfterm = None;
				typeofterm = None;
				ind_assoc = [];
			      } (Some t2_type) sg in
      (match (new_rec_t2.wfterm,new_rec_t1.wfterm) with
	(None,_) -> type_error (Other loc)
      | (_,None) -> type_error (Other loc)
      | (Some r1,Some r2) ->
	  print_string "APP SUITE : \n";
	  display_wfterm sg r1;
	  print_string " ; ";
	  display_term sg new_rec_t2.term;
	  print_string " ; ";
	  display_wfterm sg r2;
	  print_string " ; ";
	  display_term sg new_rec_t1.term;
	  print_newline();
	  new_rec_t1.wfterm <- Some (Lambda.App(r2,r1));
	      
	  match t1_type with
	  | Lambda.Type_atom(_,_) -> type_error (Other loc)
	  | Lambda.Linear_arrow(td1,td2) ->
(* 	    let new_td1 =  *)
(* 	      typecheck_type sg td1 *)
(* 	    in  *)
(* 	    let new_t2 =  *)
(* 	      typecheck_type sg t2_type *)
(* 	    in  *)
	      let r2_assoc = 
		try (Sign.cut ind_assoc r2) 
		with _ -> ind_assoc in
	      let r1_assoc = 
		try (Sign.cut ind_assoc r1)
		with _ -> ind_assoc in
	      print_string "AVANT eq_typ 2 : ";
	      display_typ_tdef sg td1;
	      print_int (Sign.give_level r2);
	      Sign.display_assoc r2_assoc;
	      display_typ_tdef sg t2_type;	      
	      Sign.display_assoc ind_assoc;
	      display_wfterm sg r2;
	      display_wfterm sg r1;
	      Sign.display_assoc r1_assoc;
	      print_newline();
	      if eq_typ td1 r2_assoc t2_type r1_assoc sg
(* 	    if eq_typ td1 (Sign.cut_assoc ind_assoc s) t2_type ind_assoc sg *)
	      then (
		print_string "JUSTE AVANT\n";
		display_typ_tdef sg td2;
		display_rec_term sg new_rec_t1;
(* 		new_rec_t1.typeofterm <- Some (Lambda.Linear_arrow(td1,td2)); *)
		(t2_type,new_rec_t1))
	      else 
		(print_string "\nerreur 4\n"; 
		 type_error (Not_well_typed_term(
			     (Sign.term_to_string r2 ind_assoc sg),
			     Sign.type_def_to_string (Sign.cut ind_assoc r1) 
			       t2_type sg,pos1,pos2))))
	

(* create a fresh type at the location loc *)
and new_type loc =
  type_nb := !type_nb + 1;
  let s = !type_nb in
  new_type_list := (s,None) :: !new_type_list; 
  Lambda.Type_atom(s,[]) 

(* typecheck entries in sig_entries with empty type env., variable env *)
(* and ? *)
let typecheck (sig_name,content :string * Abstract_sig.sig_content) = 
  typecheck_sig [](*[] [] *)
    (Sign.create(sig_name,
    (* (Abstract_typ.Signature (sig_name,0,Table.create(),Tries.empty, *)
(* 	       {Abstract_typ.type_definitions = *)
(* 		content.Abstract_sig.type_definitions; *)
(* 		Abstract_typ.term_definitions = *)
(* 		content.Abstract_typ.term_definitions})) *)
		      (Sign.content2content content)))
    content.Abstract_sig.entries
      

;;
