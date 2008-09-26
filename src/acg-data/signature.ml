open Lambda
open Abstract_syntax

type sig_entry =
  | Type_declaration of string * int * Lambda.kind
  | Type_definition of string * int * Lambda.kind * Lambda.stype
  | Term_declaration of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype
  | Term_definition of string * int * Abstract_syntax.syntactic_behavior * Lambda.stype * Lambda.term


module type Table_sig = 
sig
  exception Not_found
  exception Conflict
  type 'a t
  type key
  val empty : 'a t
  val add : ?override:bool -> key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end

let get_location = function
  | Abstract_syntax.Var (_,l) -> l
  | Abstract_syntax.Const (_,l) -> l
  | Abstract_syntax.Abs (_,_,_,l) -> l 
  | Abstract_syntax.LAbs (_,_,_,l) -> l
  | Abstract_syntax.App (_,_,l) -> l



module Make(Symbols:Table_sig with type key=String.t)(Id:Table_sig with type key=int) =
struct

  exception Not_functional_type
  exception Functional_type of Abstract_syntax.abstraction
  exception Not_normal_term
  exception Vacuous_abstraction of (string * Abstract_syntax.location * Abstract_syntax.location)
  exception Non_empty_linear_context of (string*Abstract_syntax.location)
  exception Not_linear of (Abstract_syntax.location * Abstract_syntax.location)
  exception Type_mismatch of (Abstract_syntax.location * Lambda.stype * Lambda.stype)

  exception Duplicate_type_definition
  exception Duplicate_term_definition

  type entry = sig_entry

  type t = {name:string*Abstract_syntax.location;
	    size:int;
	    terms:entry Symbols.t;
	    types:entry Symbols.t;
	    ids:entry Id.t}

  type term = Lambda.term

  type stype = Lambda.stype

  let id_to_string {ids=ids} i =
    match Id.find i ids with
      | Type_declaration(s,_,_) -> Abstract_syntax.Default,s
      | Type_definition(s,_,_,_) -> Abstract_syntax.Default,s
      | Term_declaration(s,_,behavior,_) -> behavior,s
      | Term_definition(s,_,behavior,_,_) -> behavior,s


  let empty n = {name=n;size=0;terms=Symbols.empty;types=Symbols.empty;ids=Id.empty}

  let name {name=n} = n

  let find_atomic_type s ({types=syms} as sg) = 
    try
      match Symbols.find s syms with
	| Type_declaration (x,id,_) when x=s -> Lambda.Atom id
	| Type_declaration _ -> failwith "Bug"
	| Type_definition (x,id,_,_) when x=s -> Lambda.DAtom id
	| Type_definition _ -> failwith "Bug"
	| Term_declaration _ -> failwith "Bug"
	| Term_definition _ -> failwith "Bug"
    with
      | Not_found -> failwith "Bug"


  let rec convert_type ty ({types=syms} as sg) = 
    match ty with
      | Abstract_syntax.Type_atom (s,l,args) -> find_atomic_type s sg
      | Abstract_syntax.Linear_arrow (ty1,ty2,l) -> Lambda.LFun (convert_type ty1 sg,convert_type ty2 sg)
      | Abstract_syntax.Arrow (ty1,ty2,l) -> Lambda.Fun (convert_type ty1 sg,convert_type ty2 sg)

  let abstract_on_dependent_types lst sg=
    List.fold_right
      (fun x acc -> Lambda.Depend(convert_type x sg,acc))
      lst
      Lambda.Type

  let add_sig_type t e ({size=s;types=syms;ids=ids} as sg) =
    try
      (* First perform addition on the functional data structure *)
      let new_symbols = Symbols.add t e syms in
      {sg with size=s+1;types=new_symbols;ids=Id.add s e ids}
    with
      | Symbols.Conflict -> raise Duplicate_type_definition
      | Id.Conflict  -> raise Duplicate_type_definition

  let add_sig_term t e ({size=s;terms=syms;ids=ids} as sg) =
    try
      (* First perform addition on the functional data structure *)
      let new_symbols = Symbols.add t e syms in
      {sg with size=s+1;terms= new_symbols;ids=Id.add s e ids}
    with
      | Symbols.Conflict -> raise Duplicate_term_definition
      | Id.Conflict  -> raise Duplicate_term_definition


  let rec expand_type ty ({ids=ids} as sg) = 
    match ty with
      | Lambda.Atom _ -> ty
      | Lambda.DAtom i ->
	  (match Id.find i ids with
	     | Type_definition (_,_,_,ty1) -> expand_type ty1 sg
	     | _ -> failwith "Bug")
      | Lambda.LFun (ty1,ty2) -> Lambda.LFun(expand_type ty1 sg,expand_type ty2 sg)
      | Lambda.Fun (ty1,ty2) -> Lambda.Fun(expand_type ty1 sg,expand_type ty2 sg)
      | _ -> failwith "Not yet implemented"

  let unfold_type_definition i ({ids=ids} as sg) = 
    match Id.find i ids with
      | Type_definition (_,_,_,ty1) -> expand_type ty1 sg
      | _ -> failwith "Bug"


  let rec expand_term t ({ids=ids} as sg) = 
    match t with
      | (Lambda.Var _| Lambda.LVar _ | Lambda.Const _) -> t
      | Lambda.DConst i ->
	  (match Id.find i ids with
	     | Term_definition (_,_,_,_,u) -> expand_term u sg
	     | _ -> failwith "Bug")
      | Lambda.Abs (x,u) -> Lambda.Abs (x,expand_term u sg)
      | Lambda.LAbs (x,u) -> Lambda.LAbs (x,expand_term u sg)
      | Lambda.App (u,v) -> Lambda.App (expand_term u sg,expand_term v sg)
      | _ -> failwith "Not yet implemented"

  let unfold_term_definition i ({ids=ids} as sg) = 
    match Id.find i ids with
      | Term_definition (_,_,_,_,t) -> expand_term t sg
      | _ -> failwith "Bug"
	  
	  

  let rec decompose_functional_type ty ({ids=ids} as sg) =
    match ty with
      | Lambda.LFun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Linear
      | Lambda.Fun (ty1,ty2) -> ty1,ty2,Abstract_syntax.Non_linear
      | Lambda.DAtom i ->
	  (match Id.find i ids with
	     | Type_definition (_,_,_,ty1) -> decompose_functional_type ty1 sg
	     | _ -> failwith "Bug")
      | _ -> raise Not_functional_type


  let rec get_binder_argument_functional_type x ({terms=terms} as sg) =
    let ty = 
      match Symbols.find x terms with
	| Term_declaration (_,_,_,ty) -> ty
	| Term_definition (_,_,_,ty,_) -> ty
	| _ -> failwith (Printf.sprintf "Bug: Request of the type of the non constant \"%s\"" x) in
      try
	let ty1,ty2,_ = decompose_functional_type ty sg in
	let _,_,lin = decompose_functional_type ty1 sg in
	  Some lin
      with
	| Not_functional_type -> None


	  
	  
  (* [get_typing x loc typing_env] returns [l,t,e] where [l] is the
     level of [x] and [t] its type in the typing environment
     [typing_env] when [x] is a variable located at [loc]. [e] is the
     new environment where [x] as been marcked as used at location
     [l]. If [x] has been used alread once (this is marked by the [Some
     l], 3rd projection in the association list), the function raises
     [Not_linear l] where [l] is the location of the former usage of
     [x]. If [x] is not in the typing environment [typing_env], it
     raises Not_found *)
	  
  let get_typing x loc lst =
    let rec get_typing_aux lst k =
      match lst with
	| [] -> raise Not_found
	| (s,(level,ty,None))::tl when s=x->
	    k(level,ty,((s,(level,ty,Some loc))::tl))
	| (s,(_,_,Some l))::_ when s=x -> raise (Not_linear (l,loc))
	| hd::tl -> get_typing_aux tl (fun (level,ty,r) -> k (level,ty,(hd::r))) in
      get_typing_aux lst (fun (level,ty,env) -> (level,ty,env))
	
	
  let put_env x l lst =
    let rec change_env_aux lst k =
      match lst with
	| [] -> raise Not_found
	| (s,_)::tl when s=x -> k ((s,l)::tl)
	| hd::tl -> change_env_aux tl (fun r -> k (hd::r)) in
      change_env_aux lst (fun x -> x)
	
	
  let var i = function
    | Abstract_syntax.Linear -> Lambda.LVar i
    | Abstract_syntax.Non_linear -> Lambda.Var i 


  let print_env (l_env,env,lin) f =
    let () = Printf.printf "Linear environment:\n%s\n" (Utils.string_of_list "\n" (fun (x,(l,ty,u)) -> Printf.sprintf "%s (%d): %s (%s)" x l (Lambda.type_to_string ty f) (match u with | None -> "Not used" | Some _ -> "Used")) l_env) in
    let () = Printf.printf "Non linear environment:\n%s\n" (Utils.string_of_list "\n" (fun (x,(l,ty)) -> Printf.sprintf "%s (%d): %s" x l (Lambda.type_to_string ty f) ) env) in
      Printf.printf "Next usage:\n%s\n" (Utils.string_of_list "\n" (fun (x,ab) -> Printf.sprintf "%s : %s" x (match ab with | Abstract_syntax.Linear -> "Linear" | _ -> "Non Linear")) lin)
      
  type typing_env_content = 
    | Delay of (int -> int -> Abstract_syntax.location -> Lambda.term)
    | Eval of (int -> int -> Abstract_syntax.location -> (Lambda.term * typing_env_content))

  type typing_environment =
      {linear_level:int; (* The depth of the term with respect to the
			    number of linear abstraction. Starting at 0 *)
       level:int; (* The depth of the term with respect to the number
		     of non linear abstraction. Starting at 0 *)
       env : (typing_env_content*Lambda.stype*Abstract_syntax.abstraction) Utils.StringMap.t;
       wrapper : (Abstract_syntax.location*Lambda.stype*Abstract_syntax.location) option}


  let remove_lin_context ({env=e} as tenv) =
    {tenv with
       env=Utils.StringMap.fold
	(fun k ((_,_,abs) as v) acc ->
	   match abs with
	     | Abstract_syntax.Linear -> acc
	     | Abstract_syntax.Non_linear -> Utils.StringMap.add k v acc)
	e
	Utils.StringMap.empty}

  let insert_lin_var (ty:Lambda.stype) (env:typing_environment) =
    Eval (fun l_level _ loc -> 
(*	    let i = (l_level - 1 - env.linear_level) in
	    let () = Printf.printf "Inserting variable %d - 1 - %d = %d\n%!" l_level env.linear_level i in *)
	      Lambda.LVar (l_level - 1 - env.linear_level),
	    Delay (fun _ _ l -> raise (Not_linear (l,loc)))),
    ty,
    Abstract_syntax.Linear

  let insert_non_lin_var (ty:Lambda.stype) (env:typing_environment) =
    Delay (fun  _ level _ -> Lambda.Var (level - 1 - env.level)),
    ty,
    Abstract_syntax.Non_linear

  let compute (k:typing_env_content) (env:typing_environment) (l:Abstract_syntax.location) =
    match k with
      | Delay f -> f env.linear_level env.level l,Delay f
      | Eval f -> f env.linear_level env.level l


  let get_binding x map = 
    try
      Some (Utils.StringMap.find x map)
    with
      | Not_found -> None

  let replace_binding x v map =
    match v with
      | None -> Utils.StringMap.remove x map
      | Some b -> Utils.StringMap.add x b map

  let typecheck t ty ({terms=syms} as sg) =
    let local_expand ty = expand_type ty sg in
    let rec typecheck_aux t ty (tenv:typing_environment) =
      match t with
	| Abstract_syntax.Var (x,l) -> 
	    (try
	       let f_var,var_type,lin = Utils.StringMap.find x tenv.env in
	       let var,new_f=compute f_var tenv l in
		 match ty with
		   | None ->
		       var,
		       var_type,
		       {tenv with env=Utils.StringMap.add x (new_f,var_type,lin) tenv.env}
		   | Some l_ty when l_ty=local_expand var_type ->
		       var,
		       var_type,
		       {tenv with env=Utils.StringMap.add x (new_f,var_type,lin) tenv.env}
		   | Some l_ty -> raise (Type_mismatch (l,l_ty,var_type))
	     with
	       | Not_found -> raise (Non_empty_linear_context (x,l)))
	| Abstract_syntax.Const (x,l) ->
	    (try
	       match Symbols.find x syms with
		 | Term_declaration (y,id,_,const_type) when y=x->
		     (match ty with
			| None -> Lambda.Const id,const_type,tenv
			| Some l_ty when (local_expand const_type)=l_ty -> Lambda.Const id,const_type,tenv
			| Some l_ty -> raise (Type_mismatch (l,l_ty,const_type)))
		 | Term_definition (y,id,_,const_type,_) when y=x->
		     (match ty with
			| None -> Lambda.DConst id,const_type,tenv
			| Some l_ty when (local_expand const_type)=l_ty -> Lambda.DConst id,const_type,tenv
			| Some l_ty -> raise (Type_mismatch (l,l_ty,const_type)))
		 | _ -> failwith "Bug"
	     with
	       | Symbols.Not_found -> failwith "Bug")
	| Abstract_syntax.LAbs (x,l_x,u,l_u) ->
	    (match ty with
	       | None -> raise Not_normal_term
	       | Some l_ty ->
		   let b = get_binding x tenv.env in
		     (try
			let ty1,ty2,lin = decompose_functional_type l_ty sg in
			  match lin with
			    | Abstract_syntax.Linear ->
				let u_term,u_type,new_typing_env = 
				  typecheck_aux
				    u
				    (Some (local_expand ty2))
				    {tenv with
				       linear_level=tenv.linear_level+1;
				       env=Utils.StringMap.add x (insert_lin_var ty1 tenv) tenv.env} in
				let f_var,_,_ = Utils.StringMap.find x new_typing_env.env in
				  (try
				     let _ = compute f_var new_typing_env l_x in 
				       (* if the Not_linear exception is not raised, it means the variable
					  was not
					  used in
					  u_term *)
				       raise (Vacuous_abstraction (x,l_x,get_location u))
				   with
				     | Not_linear _ -> Lambda.LAbs(x,u_term),l_ty,{new_typing_env with env= replace_binding x b new_typing_env.env ; linear_level=tenv.linear_level})
			    | Abstract_syntax.Non_linear as l -> raise (Functional_type l)
		      with
			| Not_functional_type 
			| Functional_type Abstract_syntax.Non_linear  ->
			    raise (Error.Error (Error.Type_error (Error.Is_Used (Lambda.type_to_string l_ty (id_to_string sg),"\"'a -> 'b\" (linear abstraction)" ),l_u)))))
	| Abstract_syntax.Abs (x,l_x,u,l_u) ->
	    (match ty with
	       | None -> raise Not_normal_term
	       | Some l_ty ->
		   let b = get_binding x tenv.env in
		     (try
			let ty1,ty2,lin = decompose_functional_type l_ty sg in
			  match lin with
			    | Abstract_syntax.Non_linear ->
				let u_term,u_type,new_typing_env = 
				  typecheck_aux
				    u
				    (Some (local_expand ty2))
				    {tenv with
				       level=tenv.level+1;
				       env=Utils.StringMap.add x (insert_non_lin_var ty1 tenv) tenv.env} in
				  Lambda.Abs(x,u_term),l_ty,{new_typing_env with env= replace_binding x b new_typing_env.env;level=tenv.level}
			    | Abstract_syntax.Linear as l -> raise (Functional_type l)
		      with
			| Not_functional_type 
			| Functional_type _ -> raise (Error.Error (Error.Type_error (Error.Is_Used (Lambda.type_to_string l_ty (id_to_string sg),"\"'a => 'b\" (non-linear abstraction)"),l_u)))))
	| Abstract_syntax.App (u,v,l) ->
	    let u_term,u_type,new_typing_env = 
	      try
		typecheck_aux u None tenv
	      with
		| Not_normal_term -> raise (Error.Error (Error.Type_error (Error.Not_normal,l))) in
	    let ty1,ty2,lin = 
	      try 
		decompose_functional_type u_type sg
	      with 
		| Not_functional_type -> let u_loc = get_location u in
		    raise (Error.Error (Error.Type_error (Error.Is_Used (Lambda.type_to_string u_type (id_to_string sg),"\"'a -> 'b\" or \"'a => 'b\" in order to enable application"),(fst u_loc,snd u_loc)))) in
	      try
		let v_term,_,new_new_typing_env = 
		  match lin with
		    | Abstract_syntax.Linear -> typecheck_aux v (Some (local_expand ty1)) new_typing_env
		    | Abstract_syntax.Non_linear ->
			let non_lin_env = remove_lin_context new_typing_env in
(*			let () = Printf.printf "Inserting wrapper\n%!" in *)
			let v_t,v_ty,{wrapper=w} = typecheck_aux v (Some (local_expand ty1)) {non_lin_env with wrapper=Some (get_location u,u_type,get_location v)} in
			  v_t,v_ty,{new_typing_env with wrapper=w} in
		  match ty with
		    | None -> Lambda.App (u_term,v_term),ty2,new_new_typing_env
		    | Some l_ty when l_ty=local_expand ty2 -> Lambda.App (u_term,v_term),l_ty,new_new_typing_env
		    | Some l_ty -> raise (Type_mismatch (l,l_ty,ty2)) 
	      with
		| Non_empty_linear_context (x,l) ->
		    let func_loc,func_st,v_loc = match tenv.wrapper with
		      | None -> failwith "Bug"
		      | Some a  ->a in
(*		    let v_loc = get_location v in*)
		      raise (Error.Error (Error.Type_error (Error.Non_empty_context (x,l,func_loc,Lambda.type_to_string func_st (id_to_string sg)),v_loc))) in 
      try
	let t_term,t_type,(_:typing_environment) = 
	  typecheck_aux t (Some (local_expand ty)) {linear_level=0;level=0;env=Utils.StringMap.empty;wrapper=None} in    
	  t_term
      with
	| Type_mismatch ((p1,p2),t1,t2) -> raise (Error.Error (Error.Type_error (Error.Is_Used (Lambda.type_to_string t1 (id_to_string sg),Printf.sprintf "\"%s\"" (Lambda.type_to_string t2 (id_to_string sg))),(p1,p2))))
	| Not_linear ((s1,e1),(s2,e2)) -> raise (Error.Error (Error.Type_error (Error.Two_occurrences_of_linear_variable (s2,e2),(s1,s1))))
	| Vacuous_abstraction (x,l1,l2) -> raise (Error.Error (Error.Type_error (Error.Vacuous_abstraction (x,l2),l1)))
	    
	    
	    
  let add_entry e ({size=s} as sg) =
    match e with
      | Abstract_syntax.Type_decl (t,_,Abstract_syntax.K k) -> 
	  add_sig_type t (Type_declaration (t,s,abstract_on_dependent_types k sg)) sg
      | Abstract_syntax.Type_def (t,_,ty,Abstract_syntax.K k) ->
	  add_sig_type t (Type_definition (t,s,abstract_on_dependent_types k sg,convert_type ty sg)) sg
      | Abstract_syntax.Term_decl (t,behavior,_,ty) ->
	  let t_type = convert_type ty sg in
	    add_sig_term t (Term_declaration (t,s,behavior,convert_type ty sg)) sg
      | Abstract_syntax.Term_def (t,behavior,_,term,ty) ->
	  let t_type = convert_type ty sg in
	  let t_term = typecheck term t_type sg in
	    add_sig_term t (Term_definition (t,s,behavior,t_type,t_term)) sg
	      
  let is_type s {types=syms} =
    try
      match Symbols.find s syms with
	| Type_declaration _ -> true
	| Type_definition _ -> true
	| _ -> false
    with
      | Symbols.Not_found -> false
	  
  let is_constant s {terms=syms} =
    try
      match Symbols.find s syms with
	| Term_declaration (_,_,behavior,_) -> true,Some behavior
	| Term_definition (_,_,behavior,_,_) -> true,Some behavior
	| _ -> false,None
    with
      | Symbols.Not_found -> false,None

  let add_warnings _ sg = sg

  let get_warnings _  = []

  let type_to_string ty sg = Lambda.type_to_string ty (id_to_string sg)

  let term_to_string t sg = 
    Lambda.term_to_string
      t
      (id_to_string sg)
      (*      (fun i -> 
	      match Id.find i ids with
	      | Term_declaration(s,_,_) -> Abstract_syntax.Default,s
	      | Term_definition(s,_,_,_) -> Abstract_syntax.Default,s
	      | _ -> failwith "Call a term on a type")*)

  let raw_to_string t = Lambda.raw_to_string t

  let behavior_to_string = function
    | Abstract_syntax.Default -> ""
    | Abstract_syntax.Prefix -> "prefix "
    | Abstract_syntax.Infix -> "infix "
    | Abstract_syntax.Binder -> "binder "
      
  let entry_to_string f = function
    | Type_declaration(s,_,k) -> Printf.sprintf "\t%s : %s;" s (Lambda.kind_to_string k f)
    | Type_definition(s,_,k,ty) -> Printf.sprintf "\t%s = %s : %s;" s (Lambda.type_to_string ty f) (Lambda.kind_to_string k f)
    | Term_declaration(s,_,behavior,ty) -> Printf.sprintf "\t%s%s : %s;" (behavior_to_string behavior) s (Lambda.type_to_string ty f)
    | Term_definition(s,_,behavior,ty,t) -> Printf.sprintf "\t%s%s = %s : %s;" (behavior_to_string behavior) s (Lambda.term_to_string t f) (Lambda.type_to_string ty f)

  let to_string ({name=n;ids=ids} as sg) =
    Printf.sprintf "signature %s = \n%send\n"
      (fst n)
      (fst (Id.fold 
	 (fun _ e (acc,b) ->
	    match b with
	      | true -> Printf.sprintf "%s%s\n" acc (entry_to_string (id_to_string sg) e),true
	      | false -> Printf.sprintf "%s\n" (entry_to_string (id_to_string sg) e),true)
	 ("",false)
	 ids))

  let convert_term t ty sg =
    let t_type = convert_type ty sg in
      typecheck t t_type sg,t_type

  let type_of_constant x ({terms=syms} as sg) =
    try
      match Symbols.find x syms with
      | Term_declaration (s,_,_,ty) when x = s -> ty
      | Term_definition (s,_,_,ty,_) when x = s -> ty
      | _ -> failwith "Bug"
    with
      | Symbols.Not_found -> failwith "Bug"

  let fold f a {ids=ids} =
    Id.fold
      (fun _ att acc -> f att acc)
      a
      ids

  let is_declared e _ =
    match e with
      | Type_declaration (s,_,_) -> Some s
      | Term_declaration (s,_,_,_) -> Some s
      | _ -> None

end	  
  
module Table = Table.Make_table(struct let b = 10 end)
(*module Table =
struct
  module IntMap = Map.Make(struct type t=int let compare i j = i-j end)
  type 'a t = 'a IntMap.t
  type key = int

  exception Conflict
  let empty = IntMap.empty
  let add ?(override=false) k v t = 
    try 
      let _ = IntMap.find k t in
	if override then IntMap.add k v t else raise Conflict
    with
      | Not_found -> IntMap.add k v t


  exception Not_found

  let find k t = 
    try
      IntMap.find k t
    with
      | Not_found -> raise Not_found
  let fold f acc t = IntMap.fold f t acc
end
*)

module Sylvains_signature = Make(Tries.Tries)(Table)


