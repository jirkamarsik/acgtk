open Abstract_syntax
module Lambda = 

  struct

    exception Not_yet_implemented
    

    type kind = 
      Type 
    | Depend of stype * kind  (* the kind of a dependant type *)


    and  stype =
      Atom of int                       (* atomic type *)
    | DAtom of int                      (* defined atomic type *)
    | LFun of stype * stype             (* linear functional type *)
    | Fun of stype * stype              (* non linear functional type *)
    | Dprod of string * stype * stype   (* dependant product *)
    | Record of int * stype list        (* records *)
    | Variant of int * stype list       (* variants *) 
    | TAbs of string * stype            (* type abstraction *)
    | TApp of stype * term              (* type application *)

    
    and  term =
      Var of int                (* lambda variable *)
    | LVar of int               (* linear lambda variable *)
    | Const of int              (* constant *)
    | DConst of int             (* defined constant *)
    | Abs of string * term      (* lambda-abstraction *)
    | LAbs of string * term     (* linear lambda abstraction *)
    | App of term * term        (* application *)
    | Rcons of int * term list  (* record constructor:         *)
                                (* - the integer is the tag of *)
                                (*   the corresponding type.   *)
    | Proj of int * int *term   (* projection:                        *)
                                (* - the first integer is the tag of  *)
                                (*   the corresponding type;          *)
                                (* - the second integer is the index  *)
                                (*   of the projection.               *)
    | Vcons of int * int * term (* variant constructor:               *)
                                (* - the first integer is the tag of  *)
                                (*   the corresponding type;          *)
                                (* - the second integer is the number *)
                                (*   of the constructor.              *)
    | Case of int * term * (string * term) list
                                (* case analysis:              *)      
                                (* - the integer is the tag of *)
                                (*   the corresponding type.   *)
    | Unknown of int            (* meta-variable - used in higher-order  *)
                                (* matching                              *) 


    let rec generate_var_name x env =
      if List.exists (fun (_,s) -> x=s) env then
	generate_var_name (Printf.sprintf "%s'" x) env
      else
	x


     let rec unfold_labs acc level env = function
       | LAbs (x,t) ->
	   let x' = generate_var_name x env in
	     unfold_labs ((level,x')::acc) (level+1) env t
       | t -> acc,level,t

     let rec unfold_abs acc level env = function
       | Abs (x,t) -> 
	   let x' = generate_var_name x env in
	     unfold_abs ((level,x')::acc) (level+1) env t
       | t -> acc,level,t

     let rec unfold_app acc = function
       | App (t1,t2) -> unfold_app (t2::acc) t1
       | t -> acc,t
	
     let is_binder id id_to_sym =
       match id_to_sym id with
	 | Abstract_syntax.Binder,_ -> true
	 | _ -> false

     let is_infix id id_to_sym =
       match id_to_sym id with
	 | Abstract_syntax.Infix,_ -> true
	 | _ -> false

     let is_prefix id id_to_sym =
       match id_to_sym id with
	 | (Abstract_syntax.Prefix|Abstract_syntax.Default),_ -> true
	 | _ -> false


     let rec unfold_binder binder l_level level id_to_sym acc env = function
       | App (Const i,(LAbs(x,u) as t)) when (is_binder i id_to_sym)&&(i=binder) ->
	   let x' = generate_var_name x env in
	     unfold_binder binder (l_level+1) level id_to_sym ((l_level,(x',Abstract_syntax.Linear))::acc) env u
       | App (Const i,(Abs(x,u) as t)) when (is_binder i id_to_sym)&&(i=binder) -> 
	   let x' = generate_var_name x env in
	     unfold_binder binder l_level (level+1) id_to_sym ((level,(x',Abstract_syntax.Non_linear))::acc) env u
       | t -> acc,l_level,level,t
	   
     let parenthesize (s,b) = match b with
       | true -> s
       | false -> Printf.sprintf "(%s)" s



     let type_to_string ty id_to_sym =
       let rec type_to_string_aux ty =
	   match ty with
	     | Atom i -> snd (id_to_sym i),true
	     | DAtom i -> snd (id_to_sym i),true
	     | LFun (ty1,ty2) ->
		 Printf.sprintf "%s -> %s"
		   (parenthesize (type_to_string_aux ty1))
		   (parenthesize (type_to_string_aux ty2)),
		 false
	     | Fun (ty1,ty2) ->
		 Printf.sprintf "%s => %s"
		   (parenthesize (type_to_string_aux ty1))
		   (parenthesize (type_to_string_aux ty2)),
		 false
	     | _ -> failwith "Not yet implemented" in
	 fst (type_to_string_aux ty)

     let kind_to_string k id_to_sym =
       let rec kind_to_string_aux = function
	 | Type -> "type"
	 | Depend (ty,k') -> 
	     let k_str = kind_to_string_aux k' in
	       Printf.sprintf "(%s)%s" (type_to_string ty id_to_sym) k_str in
	 kind_to_string_aux k
		   


     let term_to_string t id_to_sym =
       let rec term_to_string_aux t l_level level (l_env,env) =
	 match t with
	   | Var i -> List.assoc (level - 1  - i) env,true
	   | LVar i -> List.assoc (l_level - 1  - i) l_env,true
	   | Const i -> let _,x = id_to_sym i in x,true
	   | DConst i -> let _,x = id_to_sym i in x,true
	   | Abs (x,t) ->
	       let x' = generate_var_name x env in
	       let vars,l,u=unfold_abs [level,x'] (level+1) env t in
		 Printf.sprintf
		   "Lambda %s. %s"
		   (Utils.string_of_list " " (fun (_,x) -> x) (List.rev vars))
		   (let str,_ = term_to_string_aux u l_level l (l_env,(vars@env)) in str),
	       false
	   | LAbs (x,t) ->
	       let x' = generate_var_name x l_env in
	       let vars,l,u=unfold_labs [l_level,x'] (l_level+1) l_env t in
		 Printf.sprintf
		   "lambda %s. %s"
		   (Utils.string_of_list " " (fun (_,x) -> x) (List.rev vars))
		   (let str,_ = term_to_string_aux u l level ((vars@l_env),env) in str),
	       false
	   | App((Const s|DConst s),Abs(x,u)) when is_binder s id_to_sym ->
	       let x' = generate_var_name x env in
	       let vars,l_l,l,u = unfold_binder s l_level (level+1) id_to_sym [level,(x',Abstract_syntax.Non_linear)] env u in
	       let new_env=
		 List.fold_right
		   (fun  (l,(x,abs)) (l_acc,acc) ->
		      match abs with
			| Abstract_syntax.Non_linear -> l_acc,(l,x)::acc
			| Abstract_syntax.Linear -> (l,x)::l_acc,acc)
		   vars
		   (l_env,env) in
		 Printf.sprintf
		   "%s %s. %s"
		   (let _,const = id_to_sym s in const)
		   (Utils.string_of_list " " (fun (_,(x,_)) -> x) (List.rev vars))
		   (let str,_ = term_to_string_aux u l_l l new_env in str),
	       false
	   | App((Const s|DConst s),LAbs(x,u)) when is_binder s id_to_sym ->
	       let x' = generate_var_name x l_env in
	       let vars,l_l,l,u = unfold_binder s (l_level+1) level id_to_sym [l_level,(x',Abstract_syntax.Linear)] l_env u in
	       let new_env=
		 List.fold_right
		   (fun  (l,(x,abs)) (l_acc,acc) ->
		      match abs with
			| Abstract_syntax.Non_linear -> l_acc,(l,x)::acc
			| Abstract_syntax.Linear -> (l,x)::l_acc,acc)
		   vars
		   (l_env,env) in
		 Printf.sprintf
		   "%s %s. %s"
		   (let _,const = id_to_sym s in const)
		   (Utils.string_of_list " " (fun (_,(x,_)) -> x) (List.rev vars))
		   (let str,_ = term_to_string_aux u l_l l new_env in str),
	       false
	   | App(App((Const s|DConst s),t1),t2) when is_infix s id_to_sym ->
	       Printf.sprintf
		 "%s %s %s" 
		 (parenthesize (term_to_string_aux t1 l_level level (l_env,env)))
		 (let _,const=id_to_sym s in const)
		 (parenthesize (term_to_string_aux t2 l_level level (l_env,env))),
	       false
	   | App(t1,t2) ->
	       let args,t11 = unfold_app [t2] t1 in
	       Printf.sprintf
		 "%s %s"
		 (parenthesize (term_to_string_aux t11 l_level level (l_env,env)))
		 (Utils.string_of_list " " (fun x -> parenthesize (term_to_string_aux x l_level level (l_env,env))) args),false 
	   | _ -> failwith "Not yet implemented" in
	 fst (term_to_string_aux t 0 0 ([],[]))





    (* [is_linear tm] true if the lambda-term [tm] is such *)
    (* that "x" occurs linearly in "lambda x. tm", i.e.,   *)
    (* the linear abstraction [LAbs ("x",tm)] satisfies    *)
    (* the linearity constraint.                           *)  
   
    let is_linear tm =
      let rec lin_occur n tm =
        match tm with
          Var _        -> false
        | LVar m       -> m = n
        | Const _      -> false
        | Abs (_, t)   -> lin_occur n t
        | LAbs (_, t)  -> lin_occur (n+1) t
        | App (t1, t2) -> (lin_occur n t1) <> (lin_occur n t2)
        | _            -> raise Not_yet_implemented
      in
      lin_occur 0 tm

    (* [is_lclosed tm] true if the lambda-term [tm] does not *)
    (* contain any free linear variable.                     *)

    let is_lclosed tm = 
      let rec lclosed n tm =
        match tm with
          Var _        -> true
        | LVar m       -> m < n
        | Const _      -> true
        | Unknown _    -> true
        | Abs (_, t)   -> lclosed n t
        | LAbs (_, t)  -> lclosed (n+1) t
        | App (t1, t2) -> (lclosed n t1) & (lclosed n t2)
        | _            -> raise Not_yet_implemented
      in
      lclosed 0 tm


    (* de Bruijn's indices lifting *)

    let lift l_i nl_i tm =
      let rec lift_aux l_level nl_level tm =
        match tm with
          Var i        -> if i < nl_level
                          then tm
                          else Var (i + nl_i)
        | LVar i       -> if i < l_level
                          then tm
                          else LVar (i + l_i)
        | Const _      -> tm
        | Unknown _    -> tm
        | Abs (x, t)   -> Abs (x, lift_aux l_level (nl_level + 1) t)
        | LAbs (x, t)  -> LAbs (x, lift_aux (l_level + 1) nl_level t)
        | App (t1, t2) -> App (lift_aux l_level nl_level t1, 
                               lift_aux l_level nl_level t2)
        | _            -> raise Not_yet_implemented
      in
      lift_aux 0 0 tm

    (* substitution of a non-linear variable tm1 [x:=tm2] *)

    let var_subst tm1 tm2 =  
      let rec subst l_level nl_level tm =
        match tm with
          Var i        -> if i = nl_level
                          then lift l_level nl_level tm2
                          else if i < nl_level
                               then tm
                               else Var (i-1)
        | LVar _       -> tm
        | Const _      -> tm
        | Unknown _    -> tm
        | Abs (x, t)   -> Abs (x, subst l_level (nl_level + 1) t)
        | LAbs (x, t)  -> LAbs (x, subst (l_level + 1) nl_level t)
        | App (t1, t2) -> App (subst l_level nl_level t1, 
                               subst l_level nl_level t2)
        | _            -> raise Not_yet_implemented
      in 
      subst 0 0 tm1

    (* substitution of a linear variable tm1 [x:=tm2] *)

    let lvar_subst tm1 tm2 =  
      let rec subst l_level nl_level tm =
        match tm with
          Var _        -> tm
        | LVar i       -> if i = l_level
                          then lift l_level nl_level tm2
                          else if i < l_level
                               then tm
                               else LVar (i-1)
        | Const _      -> tm
        | Unknown _    -> tm
        | Abs (x, t)   -> Abs (x, subst l_level (nl_level + 1) t)
        | LAbs (x, t)  -> LAbs (x, subst (l_level + 1) nl_level t)
        | App (t1, t2) -> App (subst l_level nl_level t1, 
                               subst l_level nl_level t2)
        | _            -> raise Not_yet_implemented
      in 
      subst 0 0 tm1

    (* substitution of a term in a type "ty [x:=tm]" *)
    (* tm cannot contain any free linear variable    *)

    let subst_in_type ty tm =
      let rec subst_tm level tm1 =
        match tm1 with
          Var i        -> if i = level
                          then lift 0 level tm
                          else if i < level
                               then tm
                               else Var (i-1)
        | LVar _       -> tm
        | Const _      -> tm
        | Unknown _    -> tm
        | Abs (x, t)   -> Abs (x, subst_tm (level + 1) t)
        | LAbs (x, t)  -> LAbs (x, subst_tm level t)
        | App (t1, t2) -> App (subst_tm level t1, subst_tm level t2)
        | _            -> raise Not_yet_implemented
      in
      let rec subst_ty level ty =
        match ty with
          Atom _              -> ty
        | LFun (ty1, ty2)     -> LFun (subst_ty level ty1, subst_ty level ty2)
        | Fun (ty1, ty2)     -> Fun (subst_ty level ty1, subst_ty level ty2)
        | Dprod (x, ty1, ty2) -> Dprod (x, 
                                        subst_ty level ty1, 
                                        subst_ty (level+1) ty2)
        | TApp (ty1, tm)      -> TApp (subst_ty level ty1, subst_tm level tm)
        | _                   -> raise Not_yet_implemented
      in
      subst_ty 0 ty

     (* [is_vacuous ty] true when "ty" deos not effectively depend on "x" *)
     (* in the dependent type "Dprod (x, t, ty)"                          *)

     let is_vacuous ty =
       let rec vacuous_tm n tm =
         match tm with
           Var i        -> i <> n 
         | LVar _       -> true
         | Const _      -> true
         | Unknown _    -> true
         | Abs (_, t)   -> vacuous_tm (n+1) t
         | LAbs (_, t)  -> vacuous_tm n t
         | App (t1, t2) -> (vacuous_tm n t1) & (vacuous_tm n t2)
         | _            -> raise Not_yet_implemented
       in
       let rec vacuous_ty n ty =
         match ty with
           Atom _              -> true
         | LFun (ty1, ty2)     -> (vacuous_ty n ty1) & (vacuous_ty n ty2)
         | Fun (ty1, ty2)     -> (vacuous_ty n ty1) & (vacuous_ty n ty2)
         | Dprod (_, ty1, ty2) -> (vacuous_ty n ty1) & (vacuous_ty (n+1) ty2)
         | TApp (ty1, tm)      -> (vacuous_ty n ty1) & (vacuous_tm n tm)
         | _                   -> raise Not_yet_implemented
       in
       vacuous_ty 0 ty


     (* beta-normalization *)

     let rec head_normalize ?id_to_term tm =
       match tm with
         Var _        -> tm
       | LVar _       -> tm
       | Const _      -> tm
       | DConst i     -> (match id_to_term with
			    | None -> tm
			    | Some f -> head_normalize ?id_to_term (f i))
       | Unknown _    -> tm
       | Abs (x, t1)  -> Abs (x, head_normalize ?id_to_term t1)
       | LAbs (x, t1) -> LAbs (x, head_normalize ?id_to_term t1)
       | App (t1, t2) -> (match head_normalize ?id_to_term t1 with
                           Abs (_, t)  -> head_normalize ?id_to_term (var_subst t t2)
                         | LAbs (_, t) -> head_normalize ?id_to_term (lvar_subst t t2)
                         | nt1         -> App (nt1, t2))
       | _            -> raise Not_yet_implemented

     let rec normalize ?id_to_term tm =
       match tm with
         Var _        -> tm
       | LVar _       -> tm
       | Const _      -> tm
       | DConst i     -> (match id_to_term with
			    | None -> tm
			    | Some f -> normalize ?id_to_term (f i))
       | Unknown _    -> tm
       | Abs (x, t)   -> Abs (x, normalize ?id_to_term t)
       | LAbs (x, t)  -> LAbs (x, normalize ?id_to_term t)
       | App (t1, t2) -> let nt2 = normalize ?id_to_term t2
                         in
                         (match normalize ?id_to_term t1 with
                            Abs (_, t)  -> normalize ?id_to_term (var_subst t nt2)
                          | LAbs (_, t) -> normalize ?id_to_term (lvar_subst t nt2)
                          | nt1         -> App (nt1, nt2))
       | _            -> raise Not_yet_implemented


     (* beta-equivalence *)

     let beta_convert tm1 tm2 =
       let rec convert tm1 tm2 =
         match (tm1, tm2) with
           (Var i, Var j)                       -> i = j
         | (LVar i, LVar j)                     -> i = j 
         | (Const i, Const j)                   -> i = j
         | (Unknown i, Unknown j)               -> i = j
         | (Abs (_, tm11), Abs (_, tm12))       -> convert tm11 tm12
         | (LAbs (_, tm11), LAbs (_, tm12))    -> convert tm11 tm12
         | (App (tm11, tm12), App (tm21, tm22)) -> (convert tm11 tm21) &
                        (convert (head_normalize tm12) (head_normalize tm22))
         | _                                    -> false
       in
       convert (head_normalize tm1) (head_normalize tm2)

     (* type-normalization *)

     let rec type_normalize ty = 
       match ty with
        Atom _              -> ty
      | LFun (ty1, ty2)     -> LFun (type_normalize ty1, type_normalize ty2) 
      | Fun (ty1, ty2)      -> Fun (type_normalize ty1, type_normalize ty2) 
      | Dprod (x, ty1, ty2) -> Dprod (x, type_normalize ty1,
                                         type_normalize ty2)
      | TAbs (x, ty1)       -> TAbs (x, type_normalize ty1)
      | TApp (ty1, tm)      -> (match type_normalize ty1 with
                                  TAbs (_, nty1) -> subst_in_type nty1 tm
                                | nty1           -> TApp (nty1, tm))
      | _                   -> raise Not_yet_implemented

     (* type beta-equivalence *)

     let type_convert ty1 ty2 =
       let rec convert ty1 ty2 =
         match (ty1, ty2) with
         (Atom i, Atom j) 
           -> i = j
       | (LFun (ty11, ty12), LFun (ty21, ty22)) 
           -> (convert ty11 ty21) & (convert ty12 ty22)
       | (Fun (ty11, ty12), Fun (ty21, ty22)) 
           -> (convert ty11 ty21) & (convert ty12 ty22)
       | (Dprod (_, ty11, ty12), Dprod (_, ty21, ty22)) 
           -> (convert ty11 ty21) & (convert ty12 ty22)
       | (TAbs (_, ty11), TAbs (_, ty21))
           -> convert ty11 ty21
       | (TApp (ty11, tm1), TApp (ty21, tm2))
           -> (convert ty11 ty21) & (beta_convert tm1 tm2)
       | (_, _)
           -> false
       in
       convert (type_normalize ty1) (type_normalize ty2)


  end