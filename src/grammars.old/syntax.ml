open Lambda
open Signature
open Abstract_syntax
open Typing
open Tries

module Syntax = 

 struct

  exception Typing_error of Abstract_sig.term * Lambda.stype list * int

  exception Kinding_error of Abstract_sig.type_def * Lambda.kind list * int

  exception Binding_error of string * Abstract_sig.location

  exception Conflict of string * Abstract_sig.location

  exception Error

  type linearity = Linear | Non_Linear

  type environment = E of (string * linearity) list

  let push_var x (E l) = (E ((x, Non_Linear)::l))

  let push_lvar x (E l) = (E ((x, Linear)::l))

  exception Not_a_var


  let gen_var t_env (E lst) str =
    let rec gen_aux lst n m = 
      match lst with
        []                   -> raise Not_a_var
      | ((x,Linear)::rm)     -> if x = str
                                then (Lambda.LVar m, 
                                      Typing.type_of_lvar m t_env)
                                else gen_aux rm n (m+1)
      | ((x,Non_Linear)::rm) -> if x = str
                                then (Lambda.Var n,
                                      Typing.type_of_var n t_env)
                                else gen_aux rm (n+1) m
  in
  gen_aux lst 0 0


  let gen_const sg str = 
    let (i, ty) = Signature.get_const sg str
    in
    (Lambda.Const i, ty)


  (********************************************************************)
  (*                                                                  *)
  (* [generate_term sg t_env symtb env tm ty] generates a Lambda.term *)
  (* from an Abstract_sig.term and checks that it is well typed       *)
  (*                                                                  *)
  (* [sg] is the current Signature.t                                  *)
  (* [t_env] is the current typing environment                        *)
  (* [env] is the current environment                                 *)
  (* [tm] is the Abstract_sig.term                                    *)
  (* [ty] is the expected Lambda.stype                                *)
  (*                                                                  *)
  (********************************************************************)


  let rec generate_term sg t_env env tm ty = 
    match tm with
    | Abstract_sig.LAbs (str, tm1,loc) ->
        (match ty with
          Lambda.LFun (ty1, ty2) ->
            let ltm1 = generate_term sg 
                                     (Typing.push_lvar ty1 t_env)
                                     (push_lvar str env)
                                     tm1
                                     ty2
            in
            if Lambda.is_linear ltm1 then Lambda.LAbs (str, ltm1)
                                     else raise (Typing_error (tm, [], 0))
        | _                     -> raise (Typing_error (tm, [ty], 1)))
    | Abstract_sig.Abs (str, tm1,loc)  ->
        (match ty with
          Lambda.Dprod (_, ty1, ty2) ->
            let ltm1 = generate_term sg 
                                     (Typing.push_var ty1 t_env)
                                     (push_var str env)
                                     tm1
                                     ty2
            in
            Lambda.Abs (str, ltm1)
        | _                          -> raise (Typing_error (tm, [ty], 2)))
    | _                            ->
        let (ltm, lty) = generate_e_term sg t_env env tm
        in
        if lty = ty then ltm
                    else raise (Typing_error (tm, [lty; ty], 3))

  and generate_e_term sg t_env env tm =
    match tm with
      Abstract_sig.Id (str, loc)  ->
       (try gen_var t_env env str
        with Not_a_var -> try gen_const sg str
                          with Signature.Not_found -> 
                            raise (Binding_error (str, loc)))
    | Abstract_sig.App (tm1, tm2,loc) ->
        let (ltm1,ty) = generate_e_term sg t_env env tm1
        in
        (match ty with
           Lambda.LFun (ty1, ty2)      ->
             let ltm2 = generate_term sg t_env env tm2 ty1
             in
             (Lambda.App (ltm1, ltm2), ty2)
         | Lambda.Dprod (_, ty1, ty2)  ->
             let ltm2 = generate_term sg t_env env tm2 ty1
             in
             if Lambda.is_lclosed ltm2 
             then (Lambda.App (ltm1, ltm2), Lambda.subst_in_type ty2 ltm2)
             else raise (Typing_error (tm2, [ty], 4))
         | _                           -> 
             raise (Typing_error (tm, [ty], 5)))
    | _                           -> raise (Typing_error (tm, [], 6))



  let rec generate_type_app sg t_env env tls ki ty =
    match tls with
      []     -> (ty, ki)
    | tm::rm -> (match ki with
                   Lambda.Depend (ty1, ki1) ->
                     let ltm = generate_term sg t_env env tm ty1
                     in
                     generate_type_app sg t_env env rm ki1 
                                       (Lambda.TApp (ty, ltm))
                 | _                        ->
                     raise Error)


  let rec generate_type sg t_env env ty ki = 
    match ty with
      | Abstract_sig.Type_atom (str, loc, tls) ->
	  let (i, ki1) = 
	    try
	      Signature.get_atom sg str
            with
	      | Signature.Not_found -> raise (Binding_error (str, loc)) in
          let (lty, ki2) = 
	    try
	      generate_type_app sg t_env env tls ki1  (Lambda.Atom i)
            with
	      | Error -> raise (Kinding_error (ty, [], 1)) in
            if ki2 = ki
	    then lty
            else raise (Kinding_error (ty, [ki; ki2], 2))
      | Abstract_sig.Linear_arrow (ty1, ty2,loc) -> 
	  let lty1 = generate_type sg t_env env ty1 Lambda.Type
          and lty2 = generate_type sg t_env env ty2 Lambda.Type in
	    if ki = Lambda.Type
	    then Lambda.LFun (lty1, lty2)
            else raise (Kinding_error (ty, [ki], 3))
      | Abstract_sig.Arrow (ty1, ty2,loc) ->
	  let lty1 = generate_type sg t_env env ty1 Lambda.Type in
          let lty2 = generate_type sg (Typing.push_var lty1 t_env) (push_var "$" env) ty2 Lambda.Type in
	    if ki = Lambda.Type
	    then Lambda.Dprod ("$", lty1, lty2)
            else raise (Kinding_error (ty, [ki], 4))
      | Abstract_sig.Dep ((str, _, ty1), ty2,loc) ->
	  let lty1 = generate_type sg t_env env ty1 Lambda.Type in
          let lty2 = generate_type sg (Typing.push_var lty1 t_env) (push_var str env) ty2 Lambda.Type in
	    if ki = Lambda.Type
	    then Lambda.Dprod (str, lty1, lty2)
            else raise (Kinding_error (ty, [ki], 5))
      | Abstract_sig.Type_Abs ((str,_,ty1),loc) ->
	  let lty1 = generate_type sg t_env env ty1 Lambda.Type in
	    if ki = Lambda.Type
	    then Lambda.TAbs (str,lty1)
	    else raise (Kinding_error (ty,[ki],6))

  let generate_kind sg (Abstract_sig.K tyls) = 
    let rec generate tyls = 
      match tyls with
        []     -> Lambda.Type
      | ty::rm -> 
          Lambda.Depend 
            (generate_type sg (Typing.E ([], [])) (E []) ty Lambda.Type, 
             generate rm)

    in
    generate tyls

  
  let generate_signature (Abstract_sig.Signature (id, entries)) =
    let rec generate sg entries =
      match entries with
        [] -> sg
      | ((Abstract_sig.Type_decl (str, loc, ki))::rm)
           -> let lki = generate_kind sg ki
              in
              let sg1 = try Signature.insert_type_dcl str lki sg
                        with Tries.Conflict -> raise (Conflict (str, loc))
              in
              generate sg1 rm
      | ((Abstract_sig.Term_decl (str, loc, ty))::rm)
           -> let lty = generate_type sg (Typing.E ([], [])) (E []) ty 
                        Lambda.Type
              in
              let sg1 = try Signature.insert_term_dcl str lty sg
                        with Tries.Conflict -> raise (Conflict (str, loc))
              in
              generate sg1 rm
    in
    generate (Signature.create()) entries
  


 end
