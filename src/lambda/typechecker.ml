open Utils
open Printf
open Tries
open Table
open Abstract_syntax
open Error
open Lambda
open Sign
open Display
exception Typing_error of string
exception Not_yet_implemented of string
      
(* module Table = Make_table (struct let b = 10 end) *)

let verbose = false
 
type symbol = string 
      
type type_env = (symbol * Abstract_sig.kind) list
      
type rec_term = Sign.rec_term
(*     {term: Abstract_sig.term; *)
(*      mutable wfterm: Lambda.term option; *)
(*      mutable typeofterm: Lambda.type_def option; *)
(*      mutable ind_assoc: (string * int) list; *)
(*    } *)

type new_type_list = (int * Lambda.type_def option) list ref

let type_error e loc = raise (Error (Type_error (e,loc)))


let type_nb = ref 100
let new_type_list : new_type_list = ref []
     
let  type_env = []
    
(* add a term elt of type typ in the environment *)
let add_env elt loc typ env =
  try List.assoc elt env;
    type_error (Error.Already_defined_var elt) loc
  with Not_found -> (elt,typ)::env


let rec term_to_string sg = function
  | Abstract_sig.Var(s,_) -> s
  | Abstract_sig.Const(s,_) -> s
  | Abstract_sig.LAbs(s,t,_) -> 
      "(lambda "^s^". "^( term_to_string sg t)^")"
  | Abstract_sig.App(t1,t2,_) -> (term_to_string sg t1)^" "^(term_to_string sg t2)
	    

let rec typecheck_sig ind_assoc sig_new = function
  | [] -> 
      (sig_new,ind_assoc)
  | last_entry::entries -> 
      let (sig_new2,ind_assoc2) = 
	typecheck_sig ind_assoc sig_new entries
      in
      let (sig_new3,ind_assoc3) = 
	typecheck_entry ind_assoc2 sig_new2 last_entry
      in
      (sig_new3,ind_assoc3)
     

	
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
  if (verbose)
  then (
    print_string "EQ_TYP\n";
    print_string "t1 : ";
    print_string (Sign.type_def_to_string as1 t1 sg);
    print_string "\nt2 : ";
    print_string (Sign.type_def_to_string as2 t2 sg););
  match (equiv t1 t2, equiv t2 t1) with
    (Lambda.Type_atom(s,tl),Lambda.Type_atom(sbis,tlbis)) -> 
(*       let ind_assoc1 = Sign.cut_assoc as1 s *)
(*       and ind_assoc2 = Sign.cut_assoc as2 sbis in *)
      let s1 = (Sign.type_def_to_string as1 t1 sg) in
      let s2 = (Sign.type_def_to_string as2 t2 sg) in
      if (verbose)
      then(
	print_string "t1 : ";
	print_string (Sign.type_def_to_string as1 t1 sg);
	print_string "\nt2 : ";
	print_string (Sign.type_def_to_string as2 t2 sg););
      (s1 == s2)
(*       (s = sbis) && (tl = tlbis) *)
  | (Lambda.Linear_arrow(td1,td2),Lambda.Linear_arrow(td1bis,td2bis)) ->
      (eq_typ td1 as1 td1bis as2 sg) && (eq_typ td2 as1 td2bis as2 sg)
  | _ -> false


(* typecheck a definition or a declaration of a type or a term *)
and typecheck_entry ind_assoc sg = function 
  | Abstract_sig.Type_decl(s,loc,((Abstract_sig.K tdefs) as k)) -> 
      if (verbose)
      then(
	print_string ("suiv "^s^"\n");
       );
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_kd = 
	typecheck_kind new_ind_assoc sg tdefs in
      let new_sg = Sign.insert_type_dcl s 
	  (Lambda.K new_kd) sg in
      (new_sg,new_ind_assoc)

  | Abstract_sig.Type_def(s,loc,tdef) -> 
      if (verbose)
      then(
	print_string ("Type_def("^s^" : ");
	display_tdef sg tdef;
	print_string ")\n");
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_tdef = 
	typecheck_type new_ind_assoc sg tdef in
      let new_sg = Sign.insert_type_def s 
	  new_tdef sg in
      (new_sg,new_ind_assoc)

(*_ -> raise(Not_yet_implemented "typecheck_entry")*)

  | Abstract_sig.Term_decl(s,tk,loc,typ) -> 
      if (verbose)
      then(
	print_string ("suiv "^s^"\n");
       );
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_td = 
	typecheck_type new_ind_assoc sg typ in
      let new_sg = Sign.insert_term_dcl s tk new_td sg in
      (new_sg,new_ind_assoc)

  | Abstract_sig.Term_def(s,tk,loc,t,typ) -> 
      if (verbose)
      then(
	print_string ("suiv "^s^"\n");
       );
(*      print_string (Abstract_typ.to_string sg);*)
      let new_ind_assoc = Sign.add_assoc ind_assoc s in
(*       Sign.display_assoc new_ind_assoc; *)
      let new_td = 
	typecheck_type new_ind_assoc (*new_*)sg(*???*) typ 
	  in 
      let rec_term = Sign.update_rec_term t None (Some new_td) new_ind_assoc
(* 	{Sign.term = t; *)
(* 	 wfterm = None; *)
(* 	 typeofterm = Some new_td; *)
(* 	 ind_assoc = []; *)
(*        } *) in
(*       print_string "appel typecheck_term 1\n"; *)
      let (new_rec,new_sg) = typecheck_term new_ind_assoc rec_term sg [] in
(*       print_string "fin appel 1\n"; *)
      (match new_rec.Sign.wfterm with
	None -> type_error Other loc
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
  | Abstract_sig.Type_atom (s,loc,[]) -> 
      (** WF types : type constant *)
      (try 
	let (_,type_s) = Sign.get_atom sg s
(*List.assoc s type_env*) 
	in 
	if type_s = Lambda.K []
	then 
	      Lambda.Type_atom((Sign.get_ind ind_assoc s),[])
	else  type_error (Not_well_kinded_type s) loc
      with Not_found -> 
	type_error (Not_defined_var s) loc)
  | Abstract_sig.Linear_arrow(tdef1,tdef2,_) -> 
      (** WF types : linear function *)
            let new_td1 = 
	      typecheck_type ind_assoc sg tdef1 in
            let new_td2 = 
	     typecheck_type ind_assoc sg tdef2 in
	     Lambda.Linear_arrow(new_td1,new_td2)

  | _ -> raise(Not_yet_implemented "typecheck_type")


(* typecheck a term *)
and typecheck_term ind_assoc rec_term sg lvar_list = 
  match rec_term.Sign.term with
  | Abstract_sig.Var(s,loc) ->       
      (** WT terms : variable *)
      (try 
	let (_,_,type_s) = Sign.get_const sg s
	in 
	match rec_term.Sign.typeofterm with 
	  None -> type_error Other loc
	| Some ty -> 
	    if eq_typ type_s ind_assoc ty rec_term.Sign.ind_assoc sg
	    then 
	      (if List.mem s lvar_list
	      then
		try
		  let s_index = Sign.get_ind rec_term.Sign.ind_assoc s
		  in
		  rec_term.Sign.wfterm <- Some (Lambda.Var s_index);
(* enlever x de ind_assoc car ne peut etre utilise qu une fois*)
		  if verbose
		  then print_string "remove 1\n";
		  rec_term.Sign.ind_assoc <- 
		    List.remove_assoc s rec_term.Sign.ind_assoc;
		  (rec_term,sg)
		with Not_found -> 
		  print_string "premier\n";
		  type_error (Not_linear_LAbs s) loc
	      else 
		try
		  let s_index = Sign.get_ind rec_term.Sign.ind_assoc s
		  in
		  rec_term.Sign.wfterm <- Some (Lambda.Var s_index);
		  (rec_term,sg)
		with Not_found -> 
		  type_error (Not_defined_var s) loc
	      )
	    else 
	      (if (verbose)
	      then (print_string "erreur 1"; 
(* 	       Sign.display_assoc ind_assoc; *));
(* 	       let old_ind_assoc =  *)
	       rec_term.Sign.ind_assoc <-
		 Sign.cut_assoc rec_term.Sign.ind_assoc s ;
	       type_error (Not_well_typed_term(s, Sign.type_def_to_string rec_term.Sign.ind_assoc type_s sg)) loc)
      with Not_found -> 
	type_error (Not_defined_var s) loc)
	
  | Abstract_sig.Const(s,loc) ->
      (** WT terms : constant *)
      (try 
	let (_,_,type_s) = Sign.get_const sg s
	(*let type_s = List.assoc s rec_term.var_env*) in 
	match rec_term.Sign.typeofterm with 
	  None -> type_error Other loc
	| Some ty -> 
	    if (verbose)
	    then(
	      print_string "AVANT eq_typ : ";
	      display_typ_tdef sg type_s;
	      Sign.display_assoc (Sign.cut_assoc rec_term.Sign.ind_assoc s);
	      print_newline();
	      display_typ_tdef sg ty;
	      Sign.display_assoc rec_term.Sign.ind_assoc;
	      print_newline(););
	    if eq_typ type_s (Sign.cut_assoc rec_term.Sign.ind_assoc s) 
		ty rec_term.Sign.ind_assoc sg
	    then 
	      (
	       rec_term.Sign.wfterm <- 
		 Some (Lambda.Const (Sign.get_ind rec_term.Sign.ind_assoc s));
	       (rec_term,sg))
	    else 
	      (if (verbose)
	      then(
		print_string ("erreur 2 : "^s^" : "); 
		display_typ_tdef sg type_s;
		print_newline();
		display_typ_tdef sg ty;
		print_newline();
(* 	       Sign.display_assoc ind_assoc; *));
(* 	       let old_ind_assoc =  *)
	       rec_term.Sign.ind_assoc <- 
		 Sign.cut_assoc rec_term.Sign.ind_assoc s ;
(* 	       display_rec_term sg rec_term; *)
(* 	       print_string s; *)
(* 	       print_string "\ntype_s :\n"; *)
(* 	       display_typ_tdef sg type_s; *)
(* 	       print_string "\nty :\n"; *)
(* 	       display_typ_tdef sg ty; *)
(* 	       print_newline(); *)
		   
	       type_error (Not_well_typed_term(s,Sign.type_def_to_string rec_term.Sign.ind_assoc type_s sg)) loc)
      with Not_found -> 
	type_error (Not_defined_var s) loc)

  | Abstract_sig.LAbs(s,t,loc) -> 
      if (verbose)
      then(
	print_string "LABS : lambda ";
	print_string s;
	print_string ". ";
	display_term sg t;
	print_newline(););
      (** WT terms linear abstraction *)
      (match rec_term.Sign.typeofterm with
	None -> type_error Other loc
      | Some (Lambda.Type_atom(styp,terml)) -> 
 	  print_string "erreur 3"; 
	  type_error (
	  Not_well_typed_term(
	  (term_to_string sg rec_term.Sign.term),
	  ("a' -> b'"))) loc
(*raise(Typing_error "abstraction without coherent type")*)
      | Some (Lambda.Linear_arrow(tdef1,tdef2)) -> 
	  let new_rec = Sign.update_rec_term t None (Some (Sign.inc_type tdef2)) rec_term.Sign.ind_assoc
(* 	    {term = t; *)
(* 	     wfterm = None; *)
(* 	     typeofterm = Some (Sign.inc_type tdef2); *)
(* 	     ind_assoc = ind_assoc; *)
(* 	   } *)
	  in
	  if (verbose)
	  then(
	    print_string "\t\tAFFICHE tdef1 LABS\n";
	    display_typ_tdef sg (Sign.inc_type tdef1);
(*	  display_signature sg;*));
(* 	  let new_ind_assoc =  *)
	  new_rec.Sign.ind_assoc <- Sign.add_assoc rec_term.Sign.ind_assoc s ;

	  let new_sg = 
	    Sign.insert_var s Abstract_sig.Default (Sign.inc_type tdef1) sg 
	  in
	  if (verbose)
	  then(
(* 	  Sign.display_assoc new_ind_assoc; *)
	    print_string "\t\tAFFICHE NEW_SG\n";
(*	  display_signature new_sg;*)
	    Sign.display_assoc rec_term.Sign.ind_assoc;
 	    print_string "appel typecheck_term 2\n"; 
	    display_rec_term new_sg new_rec;
	    let (_,_,type_s2) = Sign.get_const new_sg s in
(* 	    let i2 = Sign.get_ind rec_term.Sign.ind_assoc s in *)
	    print_string (s^" index : ");
(* 	    print_int i2; *)
	    print_string " : ";
	    display_typ_tdef new_sg type_s2;
	    print_newline();
(* 	  let (i,_,type_s) = Sign.get_const sg s in *)
(* 	  print_int i; *)
(* 	  print_string " : "; *)
(* 	  display_typ_tdef sg type_s; *)
(* 	  print_newline(); *));
	  let (res,new2_sg) = 
	    typecheck_term rec_term.Sign.ind_assoc new_rec new_sg (s::lvar_list)
	  in      
(*  	  print_string "fin appel 2\n";  *)
	  (match res.Sign.wfterm with
	    None -> type_error Other loc
	  | Some wfterm -> 
	      res.Sign.wfterm <- Some (Lambda.LAbs(s,wfterm))
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
	typeinf_term rec_term.Sign.ind_assoc (Sign.update_rec_term t2 None None rec_term.Sign.ind_assoc)
(* {term = t2; *)
(* 				wfterm = None; *)
(* 				typeofterm = None; *)
(* 				ind_assoc = []; *)
(* 			      } *) None sg lvar_list
      in 
      if (verbose)
      then(
	print_string "new_rec_term2 : ";
	display_rec_term sg new_rec_term;
	display_rec_term sg rec_term;);
      
      new_rec_term.Sign.typeofterm <- Some (type_t2); (* new_rec a mettre dans le nouveau record? *)
      new_rec_term.Sign.ind_assoc <- rec_term.Sign.ind_assoc;
      match rec_term.Sign.typeofterm with 
	None -> type_error Other loc
      | Some type_app -> 
(* 	  print_string "appel typecheck_term 3\n"; *)
          let (res,new_sg) = 
	    typecheck_term 
	      rec_term.Sign.ind_assoc 
	      (Sign.update_rec_term t1 None (Some (Lambda.Linear_arrow(type_t2,type_app))) rec_term.Sign.ind_assoc)
(* 	      {term = t1; *)
(* 	       wfterm = None; *)
(* 	       typeofterm = Some (Lambda.Linear_arrow(type_t2,type_app)); *)
(* 	       ind_assoc = ind_assoc *)
(* 	     } *)
	      sg lvar_list
	  in
(* 	  print_string "fin appel 3\n"; *)
	  (match (res.Sign.wfterm,new_rec_term.Sign.wfterm) with
	    (None,_) -> type_error Other loc
	  | (_,None) -> type_error Other loc
	  | (Some r1,Some r2) ->
	      res.Sign.wfterm <- Some (Lambda.App(r1,r2)));
	  if (verbose)
	  then(
	    print_string "\n RES = ";
	    display_rec_term new_sg res;);
	  (res,new_sg)
(*raise(Not_yet_implemented "typecheck_term Cas App")*)
	    
and typeinf_term ind_assoc rec_term typelabs sg lvar_list =
  match rec_term.Sign.term with
  | Abstract_sig.Var(s,loc) -> 
      if (verbose)
      then(
	print_string ("Var : "^s^"\n");
	print_string "Affichage ind_assoc\n";
	List.iter (fun (x,_) -> print_string (x^", ")) rec_term.Sign.ind_assoc;
	print_newline();
       );
	(if List.mem s lvar_list
	then
	  (try 
	    let (_,_,type_s) = Sign.get_const sg s in
	    try
	      let s_index = Sign.get_ind rec_term.Sign.ind_assoc s
	      in
	      rec_term.Sign.wfterm <- Some (Lambda.Var s_index);
	      if verbose
	      then print_string "remove 2\n";
	      rec_term.Sign.ind_assoc <- 
		List.remove_assoc s rec_term.Sign.ind_assoc;
	      (type_s,rec_term)
	    with Not_found ->
	      print_string "deuxieme\n";
	      type_error (Not_linear_LAbs s) loc
	  with Not_found -> 
	    print_string "troisieme\n";
	    type_error (Not_linear_LAbs s) loc)
	else
	  (try 
	    let (_,_,type_s) = Sign.get_const sg s in
	    try
	      let s_index = Sign.get_ind rec_term.Sign.ind_assoc s
	      in
	      rec_term.Sign.wfterm <- Some (Lambda.Var s_index);
	      (type_s,rec_term)
	    with Not_found ->
	      type_error (Not_defined_var s) loc
	  with Not_found -> 
	    type_error (Not_defined_var s) loc)
	)

  | Abstract_sig.Const(s,loc) ->
      if (verbose)
      then(
      print_string ("Const : "^s^"\n"););
      (try 
	let (_,_,type_s) = Sign.get_const sg s
(*	let type_s = List.assoc s rec_term.var_env*) in 
	if (verbose)
	then(
	display_typ_tdef sg type_s;);
	rec_term.Sign.wfterm <- Some (Lambda.Const (Sign.get_ind rec_term.Sign.ind_assoc s));
	if (verbose)
	then (
	display_rec_term sg rec_term;);
	(type_s,rec_term)
      with Not_found ->
	type_error (Not_defined_const s) loc)

  | Abstract_sig.LAbs(s,t,loc) ->  
      (match typelabs with
	None -> 
	  let new_rec = Sign.update_rec_term t None None rec_term.Sign.ind_assoc
(* 	    {term = t; *)
(* 	     wfterm = None; *)
(* 	     typeofterm = None; *)
(* 	     ind_assoc = [];} *)
	  in
	  let new_sg = 
	    try 	    
	      Sign.insert_var s Abstract_sig.Default (new_type loc) sg 
	    with _ -> raise (Typing_error "errr")
	  in
(* 	  let new_ind_assoc =  *)
	  rec_term.Sign.ind_assoc <- Sign.add_assoc rec_term.Sign.ind_assoc s ;
(* 	  Sign.display_assoc new_ind_assoc; *)
	  let (td,res) = typeinf_term rec_term.Sign.ind_assoc new_rec None (* ? *) new_sg (s::lvar_list)
	  in
(* 	  let new_td =  *)
(* 	    typecheck_type sg td *)
(* 	  in  *)
	  (match res.Sign.wfterm with 
	    None -> type_error Other loc
	  | Some wfterm -> 
	      rec_term.Sign.wfterm <- Some (Lambda.LAbs(s,wfterm))
	  );
	  let s_type =
	    (try  let (_,_,tdef)= Sign.get_const new_sg s  in tdef
	    with Not_found -> new_type loc)
	  in
	  (Lambda.Linear_arrow(s_type,td),rec_term)
      | Some s_type -> 
	  let new_rec = Sign.update_rec_term t None None rec_term.Sign.ind_assoc in
(* 	    {term = t; *)
(* 	     wfterm = None; *)
(* 	     typeofterm = None; *)
(* 	     ind_assoc = [];} in *)
	  let new_sg = 
	    try Sign.insert_var s Abstract_sig.Default s_type sg 
	    with _ -> raise (Typing_error "errr")
	  in
(* 	  let new_ind_assoc =  *)
	  rec_term.Sign.ind_assoc <- Sign.add_assoc rec_term.Sign.ind_assoc s ;
(* 	  Sign.display_assoc new_ind_assoc; *)
	  let (td,res) = typeinf_term rec_term.Sign.ind_assoc new_rec None (* ? *) new_sg (s::lvar_list) in
	  (match res.Sign.wfterm with
	    None -> type_error Other loc
	  | Some wfterm -> 
	      rec_term.Sign.wfterm <- Some (Lambda.LAbs(s,wfterm))
	  );
	  (Lambda.Linear_arrow(s_type,td),rec_term)
      )    
  | Abstract_sig.App(t1,t2,loc) -> 
      if (verbose)
      then (
	print_string "APP : \n";
	display_term sg t1;
	display_term sg t2;
	print_newline(););
      let (t2_type, new_rec_t2) = 
	typeinf_term rec_term.Sign.ind_assoc (Sign.update_rec_term t2 None None rec_term.Sign.ind_assoc)
(* 	  {term = t2; *)
(* 	   wfterm = None; *)
(* 	   typeofterm = None; *)
(* 	   ind_assoc = []; *)
(* 	 } *) None (* ? *) sg lvar_list
      in 
      new_rec_t2.Sign.typeofterm <- Some(t2_type);
      new_rec_t2.Sign.ind_assoc <- rec_term.Sign.ind_assoc;
      let (t1_type,new_rec_t1) =
	typeinf_term rec_term.Sign.ind_assoc (Sign.update_rec_term t1 None None rec_term.Sign.ind_assoc)
(* 	  {term = t1; *)
(* 	   wfterm = None; *)
(* 	   typeofterm = None; *)
(* 	   ind_assoc = []; *)
(* 	 } *) (Some t2_type) sg lvar_list in
      (match (new_rec_t2.Sign.wfterm,new_rec_t1.Sign.wfterm) with
	(None,_) -> type_error Other loc
      | (_,None) -> type_error Other loc
      | (Some r1,Some r2) ->
	  if (verbose)
	  then (
	    print_string "APP SUITE : \n";
	    display_wfterm sg r1;
	    print_string " ; ";
	    display_term sg new_rec_t2.Sign.term;
	    print_string " ; ";
	    display_wfterm sg r2;
	    print_string " ; ";
	    display_term sg new_rec_t1.Sign.term;
	    print_newline(););
	  new_rec_t1.Sign.wfterm <- Some (Lambda.App(r2,r1));
	      
	  match t1_type with
	  | Lambda.Type_atom(_,_) -> type_error Other loc
	  | Lambda.Linear_arrow(td1,td2) ->
(* 	    let new_td1 =  *)
(* 	      typecheck_type sg td1 *)
(* 	    in  *)
(* 	    let new_t2 =  *)
(* 	      typecheck_type sg t2_type *)
(* 	    in  *)
	      let r2_assoc = 
		try (Sign.cut rec_term.Sign.ind_assoc r2) 
		with _ -> rec_term.Sign.ind_assoc in
	      let r1_assoc = 
		try (Sign.cut rec_term.Sign.ind_assoc r1)
		with _ -> rec_term.Sign.ind_assoc in
	      if (verbose)
	      then(
		print_string "AVANT eq_typ 2 : ";
		display_typ_tdef sg td1;
		print_int (Sign.give_level r2);
		Sign.display_assoc r2_assoc;
		display_typ_tdef sg t2_type;	      
		Sign.display_assoc rec_term.Sign.ind_assoc;
		display_wfterm sg r2;
		display_wfterm sg r1;
		Sign.display_assoc r1_assoc;
		print_newline(););
	      if eq_typ td1 r2_assoc t2_type r1_assoc sg
(* 	    if eq_typ td1 (Sign.cut_assoc ind_assoc s) t2_type ind_assoc sg *)
	      then (
		if (verbose)
		then (
		  print_string "JUSTE AVANT\n";
		  display_typ_tdef sg td2;
		  display_rec_term sg new_rec_t1;);
(* 		new_rec_t1.typeofterm <- Some (Lambda.Linear_arrow(td1,td2)); *)
		(t2_type,new_rec_t1))
	      else 
		(print_string "\nerreur 4\n"; 
		 type_error (Not_well_typed_term(
			     (Sign.term_to_string r2 rec_term.Sign.ind_assoc sg),
			     Sign.type_def_to_string (Sign.cut rec_term.Sign.ind_assoc r1) 
			       t2_type sg))
			       loc))
	

(* create a fresh type at the location loc *)
and new_type loc =
  type_nb := !type_nb + 1;
  let s = !type_nb in
  new_type_list := (s,None) :: !new_type_list; 
  Lambda.Type_atom(s,[]) 

(* typecheck entries in sig_entries with empty type env., variable env *)
(* and ? *)
(* let typecheck (sig_name,content :string * Abstract_sig.sig_content) =  *)
(*   typecheck_sig [](\*[] [] *\) *)
(*     (Sign.create(sig_name, *)
(*     (\* (Abstract_typ.Signature (sig_name,0,Table.create(),Tries.empty, *\) *)
(* (\* 	       {Abstract_typ.type_definitions = *\) *)
(* (\* 		content.Abstract_sig.type_definitions; *\) *)
(* (\* 		Abstract_typ.term_definitions = *\) *)
(* (\* 		content.Abstract_typ.term_definitions})) *\) *)
(* 		      (Sign.content2content content))) *)
(*     content.Abstract_sig.entries *)
      
let typecheck (sig_name,content :string * Abstract_sig.sig_content) = 
  let res = typecheck_sig []
      (Sign.create(sig_name))
      content.Abstract_sig.entries
  in
  print_string "\nTyping done.\n";
  res

;;
