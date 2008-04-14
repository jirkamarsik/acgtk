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
         | Dprod (_, ty1, ty2) -> (vacuous_ty n ty1) & (vacuous_ty (n+1) ty2)
         | TApp (ty1, tm)      -> (vacuous_ty n ty1) & (vacuous_tm n tm)
         | _                   -> raise Not_yet_implemented
       in
       vacuous_ty 0 ty


     (* beta-normalization *)

     let rec head_normalize tm =
       match tm with
         Var _        -> tm
       | LVar _       -> tm
       | Const _      -> tm
       | Unknown _    -> tm
       | Abs (x, t1)  -> Abs (x, head_normalize t1)
       | LAbs (x, t1) -> LAbs (x, head_normalize t1)
       | App (t1, t2) -> (match head_normalize t1 with
                           Abs (_, t)  -> head_normalize (var_subst t t2)
                         | LAbs (_, t) -> head_normalize (lvar_subst t t2)
                         | nt1         -> App (nt1, t2))
       | _            -> raise Not_yet_implemented

     let rec normalize tm =
       match tm with
         Var _        -> tm
       | LVar _       -> tm
       | Const _      -> tm
       | Unknown _    -> tm
       | Abs (x, t)   -> Abs (x, normalize t)
       | LAbs (x, t)  -> LAbs (x, normalize t)
       | App (t1, t2) -> let nt2 = normalize t2
                         in
                         (match normalize t1 with
                            Abs (_, t)  -> normalize (var_subst t nt2)
                          | LAbs (_, t) -> normalize (lvar_subst t nt2)
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
