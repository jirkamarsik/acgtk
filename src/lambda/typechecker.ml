(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open Utils
open Printf
open Tries
open Table
open Abstract_syntax
open Error
open Lambda
open Utilitaires
open Display.Display
exception Typing_error of string
exception Not_yet_implemented of string
      
(* module Table = Make_table (struct let b = 10 end) *)

let verbose = true
 
type symbol = string 
      
type new_type_list = (int * Lambda.stype option) list ref

let type_error e loc = raise (Error (Type_error (e,loc)))


(* let type_nb = ref 100 *)
(* let new_type_list : new_type_list = ref [] *)
     
let rec abstr_synt_term_to_string sg = function
  | Abstract_syntax.Var(s,_) -> s
  | Abstract_syntax.Const(s,_) -> s
  | Abstract_syntax.LAbs(s,_,t,_) ->
      Printf.sprintf
	"(lambda° %s. %s)"
	s
	( abstr_synt_term_to_string sg t)
  | Abstract_syntax.Abs(s,_,t,_) ->
      Printf.sprintf
	"(lambda %s. %s)"
	s
	( abstr_synt_term_to_string sg t)
  | Abstract_syntax.App(t1,t2,_) -> 
      Printf.sprintf
	"%s %s"
	(abstr_synt_term_to_string sg t1)
	(abstr_synt_term_to_string sg t2)

(*let type_def_to_string _ _ _ = "La fonction a disparue. Elle est remplacée par celle-ci pour que cela compile quand même"

let term_to_string _ _ _ = "La fonction a disparue. Elle est remplacée par celle-ci pour que cela compile quand même"
*)



let rec typecheck_sig sig_new sg =
  Syntactic_data_structures.Abstract_sig.fold (fun e a_sg -> typecheck_entry a_sg e) sig_new sg

(* function
  | [] -> sig_new
  | last_entry::entries -> 
      let sig_new2 = 
	typecheck_sig sig_new entries
      in
      let sig_new3 = 
	typecheck_entry sig_new2 last_entry
      in
      sig_new3
*)   

	
(* equiv tdef trepl returns a type_def*)
(* replace tdef by the type_def trepl if tdef is not defined *)
(* and equiv tdef trepl = *)
(*   match tdef with *)
(*     Lambda.Type_atom(s,_) -> *)
(*       (try *)
(* 	let typ = List.assoc s !new_type_list *)
(* 	in  *)
(* 	match typ with *)
(* 	  None -> trepl *)
(* 	| Some t ->  *)
(* 	    new_type_list := List.remove_assoc s !new_type_list; *)
(* 	    new_type_list := (s,Some trepl) :: !new_type_list; *)
(* 	    equiv t trepl *)
(*       with Not_found -> tdef) *)
(*   | _ -> tdef *)


(* check that t1 and t2 are equivalent types *)
and eq_typ t1 t2 sg = 
  if (verbose)
  then (
(*     print_string "EQ_TYP\n"; *)
   );
  match ((*equiv*) t1 (*t2*), (*equiv*) t2 (*t1*)) with
    (Lambda.Atom s ,Lambda.Atom sbis ) -> 
(*      if (verbose)
      then (display_typ_tdef sg t1;print_newline();display_typ_tdef sg t2
       );*)
      (s = sbis)(* && (tl = tlbis)*)
  | (Lambda.DAtom s, typ) |  (typ, Lambda.DAtom s) ->
      let s1 = Utilitaires.get_type_def s sg in
      eq_typ s1 typ sg
  | (Lambda.LFun(td1,td2),Lambda.LFun(td1bis,td2bis)) -> 
      (eq_typ td1 td1bis sg) && (eq_typ td2 td2bis sg)
  | _ -> (* display_typ_tdef sg t1;print_newline();display_typ_tdef sg t2; *)
       print_string "false ";false


(* typecheck a definition or a declaration of a type or a term *)
and typecheck_entry sg = function 
  | Abstract_syntax.Type_decl(s,loc,((Abstract_syntax.K tdefs) as k)) -> 
      if (verbose)
      then(
	print_string ("Type_decl("^s^")\n");
	); 
      let new_kd = 
	typecheck_kind sg tdefs in
      let lambda_kind = List.fold_right (fun t k -> Lambda.Depend (t,k))  new_kd Lambda.Type in
(*      let new_sg = Utilitaires.insert_type_decl s (Lambda.K new_kd) sg in*)
      let new_sg = Utilitaires.insert_type_decl s lambda_kind sg in
      new_sg

  | Abstract_syntax.Type_def(s,loc,tdef,((Abstract_syntax.K tdefs) as k)) -> 
      if (verbose)
      then(
	print_string ("Type_def("^s^" = ");
	display_tdef sg tdef;
	print_string ")\n"); 
      let new_kd = 
	typecheck_kind sg tdefs in
      let new_tdef = 
	typecheck_type sg tdef in
      let new_sg = Utilitaires.insert_type_def s new_tdef sg in
      new_sg

  | Abstract_syntax.Term_decl(s,tk,loc,typ) -> 
      if (verbose)
      then(
	print_string ("Term_decl("^s^" : ");
	display_tdef sg typ;
	print_string ")\n"); 
      let new_td = 
	typecheck_type sg typ in
      let new_sg = Utilitaires.insert_term_decl s tk new_td sg in
      new_sg

  | Abstract_syntax.Term_def(s,tk,loc,t,typ) -> 
      if (verbose)
      then(
	print_string ("Term_def("^s^" = ");
	display_term sg t;
	print_string " : ";
	display_tdef sg typ;
	print_string ")\n"); 
      let new_td = 
	typecheck_type sg typ 
	  in 
      let (wfterm,_,_(*new_sg*)) = typecheck_term t new_td [] sg [] in
      let new2_sg = 
	Utilitaires.insert_term_def s tk wfterm new_td (*new_*)sg in
      new2_sg
	    
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
  | Abstract_syntax.Type_atom (s,loc,[]) -> 
      (try 
	match Utilitaires.get_atom sg s with
	  (i,true) ->
	      Lambda.Atom i
	| (i,false) ->
	    Lambda.DAtom i
      with Not_found -> print_string "1\n";
	type_error (Not_defined_var s) loc)
  | Abstract_syntax.Linear_arrow(tdef1,tdef2,_) -> 
            let new_td1 = 
	      typecheck_type sg tdef1 in
            let new_td2 = 
	     typecheck_type sg tdef2 in
	     Lambda.LFun(new_td1,new_td2)
  | Abstract_syntax.Arrow(tdef1,tdef2,_) -> 
            let new_td1 = 
	      typecheck_type sg tdef1 in
            let new_td2 = 
	      typecheck_type sg tdef2 in
	    Lambda.Fun(new_td1,new_td2)
  | _ -> raise(Not_yet_implemented "typecheck_type")

(* typecheck a term *)
and typecheck_term term wftype ind_assoc sg lvar_list = 
  match term with
  | Abstract_syntax.Var(s,loc) ->
      if (verbose)
      then(print_string ("\n\n\tcheck Var "^s^"\n"););
      (try 
	let (_,_,type_s,_) = Utilitaires.get_const sg s
	in 
	if eq_typ type_s wftype sg
	then 
	  (	
	    if List.mem s lvar_list
	    then
	      (* x is linear *)
	      try
		let s_index = Utilitaires.get_ind ind_assoc s
		in
		let wfterm = Lambda.LVar s_index in
		if verbose
		then print_string "remove 1\n";
                (* x is linear and used once *)
		let new_ind_assoc = List.remove_assoc s ind_assoc in
		(wfterm,new_ind_assoc,sg)
	      with Not_found -> 
		print_string "premier\n";
		type_error (Non_linear_var s) loc
	    else 
	      (* x is non linear *)
	      try
		let s_index = Utilitaires.get_ind ind_assoc s in
		let wfterm = Lambda.Var s_index in
		(wfterm,ind_assoc,sg)
	      with Not_found -> print_string "2\n";
		type_error (Not_defined_var s) loc
	   )
	else 
	  (if (verbose)
	  then (print_string "erreur 1"; );
	   let new_ind_assoc =
	     Utilitaires.cut_assoc ind_assoc s in
	   type_error
	     (Not_well_typed_term_plus(s, 
				       type_def_to_string new_ind_assoc type_s sg,
				       type_def_to_string new_ind_assoc wftype sg)) 
	     loc)
      with Not_found -> print_string "3\n";
	type_error (Not_defined_var s) loc)
	
  | Abstract_syntax.Const(s,loc) ->
      if (verbose)
      then(print_string ("\n\n\tcheck Const "^s^"\n");
	 );
      (try 
	let (s_index,_,type_s,is_decl) = Utilitaires.get_const sg s in
	if eq_typ type_s wftype sg
	then 
	  (let wfterm = 
	    if is_decl
	    then
	      Lambda.Const(s_index)
	    else
	      Lambda.DConst(s_index)
	  in
	  (wfterm,ind_assoc,sg))
	else 
	  type_error
	    (Not_well_typed_term_plus(s,
				      type_def_to_string ind_assoc type_s sg,
				      type_def_to_string ind_assoc wftype sg))
	    loc
      with Not_found -> print_string "4\n";
	type_error (Not_defined_var s) loc)

  | Abstract_syntax.LAbs(s,_,t,loc) -> 
      if (verbose)
      then(print_string "check LAbs\n";);
      if (verbose)
      then(
(* 	print_string "LABS : lambda "; *)
(* 	print_string s; *)
(* 	print_string ". "; *)
(* 	display_term sg t; *)
(* 	print_newline(); *));
      (match wftype with
      | Lambda.Atom styp -> 
	  print_string "erreur 3"; 
	  type_error(Not_well_typed_term(
		     (abstr_synt_term_to_string sg term),
		     ("'a -> 'b"))) loc
      | Lambda.DAtom styp -> 
	  let ty = Utilitaires.get_type_def styp sg in
	  typecheck_term term ty ind_assoc sg lvar_list
      | Lambda.LFun(tdef1,tdef2) -> 
	  if (verbose)
	  then(
(* 	    print_string "\t\tAFFICHE tdef1 LABS\n"; *)
(* 	    display_typ_tdef sg tdef1; *)
	   );
	  let new_ind_assoc = Utilitaires.add_assoc ind_assoc s in

	  let new_sg =
	    Utilitaires.insert_var s Abstract_syntax.Default tdef1 sg 
	  in
	  if (verbose)
	  then(
(* 	    print_string "\t\tAFFICHE NEW_SG\n"; *)
(* 	    Utilitaires.display_assoc ind_assoc; *)
(*  	    print_string "appel typecheck_term 2\n";  *)
(* 	    let (_,_,type_s2) = Utilitaires.get_const new_sg s in *)
(* 	    print_string (s^" index : "); *)
(* 	    print_string " : "; *)
(* 	    display_typ_tdef new_sg type_s2; *)
(* 	    print_newline(); *)
	   );
	  let (wfterm2,_,new_sg2) = 
	    typecheck_term t tdef2 new_ind_assoc new_sg (s::lvar_list)
	  in      
	  (Lambda.LAbs(s,wfterm2),ind_assoc,sg)
      | Lambda.Fun(tdef1,tdef2) -> type_error (Linear_var s) loc
      | _ -> raise (Not_yet_implemented "Typechecker.typecheck_term cas LAbs. type atomique")
      )
  | Abstract_syntax.Abs(s,_,t,loc) -> 
      if (verbose)
      then(print_string "check LAbs\n";);
      if (verbose)
      then(
(* 	print_string "LABS : lambda "; *)
(* 	print_string s; *)
(* 	print_string ". "; *)
(* 	display_term sg t; *)
(* 	print_newline(); *));
      (match wftype with
      | Lambda.Atom styp -> 
	  print_string "erreur 3"; 
	  type_error(Not_well_typed_term(
		     (abstr_synt_term_to_string sg term),
		     ("'a -> 'b"))) loc
      | Lambda.DAtom styp -> 
	  let ty = Utilitaires.get_type_def styp sg in
	  typecheck_term term ty ind_assoc sg lvar_list
      | Lambda.Fun(tdef1,tdef2) -> 
	  let new_ind_assoc = Utilitaires.add_assoc ind_assoc s in

	  let new_sg =
	    Utilitaires.insert_var s Abstract_syntax.Default tdef1 sg 
	  in
	  let (wfterm2,_,new_sg2) = 
	    typecheck_term t tdef2 new_ind_assoc new_sg (s::lvar_list)
	  in      
	  (Lambda.Abs(s,wfterm2),ind_assoc,sg)
      | Lambda.LFun(tdef1,tdef2) -> type_error (Non_linear_var s) loc
      | _ -> raise (Not_yet_implemented "Typechecker.typecheck_term cas LAbs. type atomique")
      )

  | Abstract_syntax.App(t1,t2,loc) ->
      if (verbose)
      then(print_string "check App\n";
	display_term sg t1;
	print_newline();
	display_term sg t2;
	print_newline();); 

      let (type_t1,wfterm1,_)=
	typeinf_term t1 None ind_assoc sg lvar_list
      in
      let (wfterm2,_,sg) =
	(match type_t1 with
	  Lambda.LFun(typ1,typ2) -> 
	    typecheck_term t2 typ1 ind_assoc sg lvar_list
	| _ -> raise (Not_yet_implemented "Typechecker.typecheck_term cas App match type_t1")
	)
      in
      let wfterm_app = Lambda.App(wfterm1,wfterm2) in
      (wfterm_app,ind_assoc,sg)
(*       let (type_t2,wfterm2,ind_assoc2) =  *)
(* 	typeinf_term t2 None ind_assoc sg lvar_list *)
(*       in  *)
(*       if verbose  *)
(*       then 	   print_string "\nappel typecheck_term 3\n";   *)
(*       let new_type = (Lambda.LFun(type_t2,wftype)) in *)
(* (\*      if verbose *)
(*       then ( *)
(* 	display_typ_tdef sg new_type;);*\) *)
      
(*       let (wfterm1,_,new_sg) =  *)
(* 	typecheck_term t1 new_type ind_assoc *)
(* 	  sg lvar_list *)
(*       in *)
(*      let wfterm_app = *)
(* 	Lambda.App(wfterm1,wfterm2) *)
(*       in *)
(*       if (verbose) *)
(*       then( *)
(* (\* 	    print_string "\n RES = "; *\) *)
(*        ); *)
(*       (wfterm_app,ind_assoc,(\*new_*\)sg) *)

(*   | _ -> raise (Not_yet_implemented "Typechecker.typecheck_term match principal") *)
	
and typeinf_term term typelabs ind_assoc sg lvar_list =
  match term with
  | Abstract_syntax.Var(s,loc) -> 
      if (verbose)
      then(
(* 	print_string ("Var : "^s^"\n"); *)
(* 	print_string "Affichage ind_assoc\n"; *)
(* 	List.iter (fun (x,_) -> print_string (x^", ")) ind_assoc; *)
(* 	print_newline(); *)
       );
      (try 
	let (_,_,type_s,_) = Utilitaires.get_const sg s in
	(match typelabs,type_s with
	  None,_ -> ()
	| Some type_arg,Lambda.LFun(type_s1,type_s2) -> 
(* 	    print_string "\n\n\n\t\t\tA verifier (Var)\n\n\n"; *)
	    if not (eq_typ type_s1 type_arg sg)
	    then 
	      let new_ind_assoc =
		Utilitaires.cut_assoc ind_assoc s in
	      type_error
		(Not_well_typed_term_plus(s, 
					  type_def_to_string new_ind_assoc type_s sg,
					  type_def_to_string new_ind_assoc (Lambda.LFun(type_arg,type_s2)) sg)) 
		loc
	| _ -> type_error Other loc);
	
	if List.mem s lvar_list
	then
	  (* x is linear *)
	  try
	    let s_index = Utilitaires.get_ind ind_assoc s
	    in
	    let wfterm = Lambda.LVar s_index in
	    if verbose
	    then print_string "remove 2\n";
	    let new_ind_assoc =
	      List.remove_assoc s ind_assoc in
	    (type_s,wfterm,new_ind_assoc)
	  with Not_found ->
	    print_string "deuxieme\n";
	    type_error (Non_linear_var s) loc
	else
	  (* x is non linear *)
	  try
	    let s_index = Utilitaires.get_ind ind_assoc s
	    in
	    let wfterm = Lambda.Var s_index in
	    (type_s,wfterm,ind_assoc)
	  with Not_found ->print_string "5\n";
	    type_error (Not_defined_var s) loc
      with Not_found -> print_string "6\n";
	type_error (Not_defined_var s) loc)
  | Abstract_syntax.Const(s,loc) ->
      if (verbose)
      then(
      print_string ("Const : "^s^" "););
      (try 
	let (i,_,type_s,is_decl) = Utilitaires.get_const sg s in
	(match typelabs,type_s with
	  None,_ -> ()
	| Some type_arg,Lambda.LFun(type_s1,type_s2) ->
(* 	    print_string "\n\n\n\t\t\tA verifier (Const) \n\n\n"; *)
	    if not (eq_typ type_s1 type_arg sg)
	    then 
	      type_error
		(Not_well_typed_term_plus(s, 
					  type_def_to_string ind_assoc type_s sg,
					  type_def_to_string ind_assoc (Lambda.LFun(type_arg,type_s2)) sg)) 
		loc
	| _ -> type_error Other loc);
	if (verbose)
	then(
(* 	  print_int i; *)
(* 	  print_newline(); *)
(* 	  display_typ_tdef sg type_s; *)
	 );
	let wfterm =
	  if is_decl
	  then
	    Lambda.Const(i)
	  else
	    Lambda.DConst(i)
	in
	if (verbose)
	then (
	);
	(type_s,wfterm,ind_assoc)
      with Not_found ->print_string "7\n";
	type_error (Not_defined_const s) loc)

  | Abstract_syntax.LAbs(s,_,t,_) ->  
      display_term sg term;
      raise (Failure "term is supposed to be normalized")
(*       if (verbose) *)
(*       then(print_string "inf LAbs\n";); *)
(*       (match typelabs with *)
(* 	None ->  *)
(* 	  let new_sg =  *)
(* 	    try 	     *)
(* 	      Utilitaires.insert_var s Abstract_syntax.Default (new_type loc) sg  *)
(* 	    with _ -> raise (Typing_error "errr") *)
(* 	  in *)
(* 	  let new_ind_assoc = Utilitaires.add_assoc ind_assoc s in  *)
(* 	  let (type2,wfterm2,ind_assoc2) =  *)
(* 	    typeinf_term t None new_ind_assoc new_sg (s::lvar_list) *)
(* 	  in *)
(* 	  let wfterm_labs =  *)
(* 	    Lambda.LAbs(s,wfterm2) *)
(* 	  in *)
(* 	  let s_type = *)
(* 	    (try  let (_,_,tdef,_)= Utilitaires.get_const new_sg s  in tdef *)
(* 	    with Not_found -> new_type loc) *)
(* 	  in *)
(* 	  (Lambda.Linear_arrow(s_type,type2),wfterm_labs,ind_assoc) *)
(*       | Some s_type ->  *)
(* 	  let new_sg =  *)
(* 	    try Utilitaires.insert_var s Abstract_syntax.Default s_type sg  *)
(* 	    with _ -> raise (Typing_error "errr") *)
(* 	  in *)
(* 	  let new_ind_assoc = Utilitaires.add_assoc ind_assoc s in *)
(* 	  let (type2,wfterm2,ind_assoc2) =  *)
(* 	    typeinf_term t None new_ind_assoc new_sg (s::lvar_list) in *)
(* 	  let wfterm_labs = *)
(* 	    Lambda.LAbs(s,wfterm2) *)
(* 	  in *)
(* 	  (Lambda.Linear_arrow(s_type,type2),wfterm_labs,ind_assoc) *)
(*       )     *)
  | Abstract_syntax.App(t1,t2,loc) -> 
      if (verbose)
      then(print_string "inf App\n";);
      if (verbose)
      then (
	print_string "APP : \n";
	display_term sg t1;
	print_newline();
	display_term sg t2;
	print_newline(););
      let (t2_type,wfterm2,ind_assoc2) = 
	typeinf_term t2 None ind_assoc sg lvar_list
      in 
(*       print_string "\n\twfterm2 = "; *)
	(*       print_string (term_to_string wfterm2 ind_assoc2 sg); *)
	(*       print_newline(); *)
      let (t1_type,wfterm1,ind_assoc1) =
	typeinf_term t1 (Some t2_type) ind_assoc  sg lvar_list in
	(*       print_string "\n\twfterm1 = "; *)
	(*       print_string (term_to_string wfterm1 ind_assoc1 sg); *)
	(*       print_newline(); *)
	if (verbose)
	then (print_string "passage infapp\n";
	);
	let wfterm_app = Lambda.App(wfterm1,wfterm2) in
      
	  (match t1_type with
	  | Lambda.Atom styp -> 
	      print_string "erreur 3"; 
	      type_error(Not_well_typed_term(
			 (abstr_synt_term_to_string sg term),
			 ("'a -> 'b"))) loc
	  | Lambda.DAtom styp -> 
	      let ty = Utilitaires.get_type_def styp sg in
	      typeinf_term term (Some ty) ind_assoc sg lvar_list
	  | Lambda.LFun(td1,td2) ->
	      let r2_assoc = 
		try (Utilitaires.cut ind_assoc wfterm2) 
		with _ -> ind_assoc in
	      let r1_assoc = 
		try (Utilitaires.cut ind_assoc wfterm1)
		  with _ -> ind_assoc in
	      if (verbose)
	      then(
(* 		print_string "AVANT eq_typ 2 : "; *)
(* 		display_typ_tdef sg td1; *)
(* 		print_int (Utilitaires.give_level r2); *)
(* 		Utilitaires.display_assoc r2_assoc; *)
(* 		display_typ_tdef sg t2_type;	       *)
(* 		Utilitaires.display_assoc ind_assoc; *)
(* 		display_wfterm sg r2; *)
(* 		display_wfterm sg r1; *)
(* 		Utilitaires.display_assoc r1_assoc; *)
(* 		print_newline(); *)
	       );
	      if eq_typ td1 t2_type sg
	      then (
		if (verbose)
		then (
(* 		  print_string "JUSTE AVANT\n"; *)
(* 		  display_typ_tdef sg td2; *)
		 );
		(td2,wfterm_app,ind_assoc))
	      else 
		(print_string "\nerreur 4\n"; 
		 type_error (Not_well_typed_term(
			     (term_to_string wfterm2 ind_assoc sg),
			     type_def_to_string (Utilitaires.cut ind_assoc wfterm1) 
			       t2_type sg))
		   loc)
	  | _ -> raise (Not_yet_implemented "Typechecker.typeinf_term cas App match t1_type"))
  | _ -> raise (Not_yet_implemented "Typechecker.typecheck_inf cas autre")
	

(* create a fresh type at the location loc *)
(* and new_type loc = *)
(*   type_nb := !type_nb + 1; *)
(*   let s = !type_nb in *)
(*   new_type_list := (s,None) :: !new_type_list;  *)
(*   Lambda.Type_atom(s,[])  *)


(*      
let typecheck (sig_name,sig_loc,content :string * Abstract_sig.sig_content) = 
  let res = typecheck_sig 
      (Utilitaires.empty(sig_name,sig_loc))
      content.Abstract_sig.entries
  in
  print_string "\nTyping done.\n";
  res

*)
let typecheck (sig_name,sig_loc,sg) =
  let res = typecheck_sig 
    (Utilitaires.empty(sig_name,sig_loc))
    sg
  in
    print_string "\nTyping done.\n";
    res
  

;;
