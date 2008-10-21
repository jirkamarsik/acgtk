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

(* !!!! some renaming is needed *)

open Lambda
open Signature

module Print =

  struct

    type environment = E of string list * string list
    
    let push_var x (E (l1, l2)) = (E (x::l1, l2))

    let get_var i (E (l, _)) = List.nth l i

    let push_lvar x (E (l1, l2)) = (E (l1, x::l2))

    let get_lvar i (E (_, l)) = List.nth l i  

    let rec kind_to_string sg env ki =
      match ki with
        Lambda.Type -> "type"
      | _           -> "("^(dkind_to_string sg env ki)

    and dkind_to_string sg env ki = 
      match ki with
        Lambda.Depend (ty, ki1)
          -> (type_to_string sg env ty)^
             (match ki1 with
                Lambda.Depend _ 
                   -> ", "^(dkind_to_string sg env ki1)
              | _  -> ") "^(kind_to_string sg env ki1))
      | _ -> raise (Failure "Print.dkind_to_string")

    and type_to_string sg env ty = 
      match ty with
        Lambda.LFun  (ty1, ty2)    
          -> (atomic_type_to_string sg env ty1)^
                                      " -> "^(type_to_string sg env ty2)
      | Lambda.Dprod (_, ty1, ty2) 
          -> if Lambda.is_vacuous ty2
             then (atomic_type_to_string sg env ty1)^" => "^
                  (type_to_string sg (push_var "$" env) ty2)
             else "("^dep_type_to_string sg env ty
      | _ -> atomic_type_to_string sg env ty

    and atomic_type_to_string sg env ty =
      match ty with
        Lambda.Atom i  -> Signature.string_of_atom i sg
      | Lambda.TApp  _ -> type_application_to_string sg env ty
      | _              -> "("^(type_to_string sg env ty)^")"

    and dep_type_to_string sg env ty = 
      match ty with
        Lambda.Dprod (x, ty1, ty2) 
          -> x^" : "^(type_to_string sg env ty1)^
             (match ty2 with
                Lambda.Dprod (_, _, ty3) 
                  -> if Lambda.is_vacuous ty3
                     then ") "^(type_to_string sg (push_var x env) ty2)
                     else ", "^(dep_type_to_string sg (push_var x env) ty2)
              | _ -> ") "^(type_to_string sg (push_var x env) ty2))
      | _ -> raise (Failure "Print.dep_type_to_string")

    and type_application_to_string sg env ty =
      match ty with
        Lambda.TApp (ty1, tm) -> (type_application_to_string sg env ty1)^" "^
                                 (atomic_term_to_string sg env tm)
      | _                     -> atomic_type_to_string sg env ty

    and term_to_string sg env tm =
      match tm with
        Lambda.Abs _  -> "Lambda"^(abs_term_to_string sg env tm)
      | Lambda.LAbs _ -> "lambda"^(labs_term_to_string sg env tm)
      | Lambda.App _  -> app_term_to_string sg env tm
      | _             -> atomic_term_to_string sg env tm

    and atomic_term_to_string sg env tm =
      match tm with
        Lambda.Var i   -> get_var i env
      | Lambda.LVar i  -> get_lvar i env
      | Lambda.Const i -> Signature.string_of_const i sg
      | _       -> "("^(term_to_string sg env tm)^")"

    and abs_term_to_string sg env tm =
      match tm with
        Lambda.Abs (x, tm1)
          -> " "^x^(match tm1 with
                      Lambda.Abs _ 
                        -> abs_term_to_string sg (push_var x env) tm1
                    | _ -> ". "^(term_to_string sg (push_var x env) tm1))
      | _ -> raise (Failure "Print.abs_term_to_string") 

    and labs_term_to_string sg env tm =
      match tm with
        Lambda.LAbs (x, tm1)
          -> " "^x^(match tm1 with
                      Lambda.LAbs _ 
                        -> labs_term_to_string sg (push_lvar x env) tm1
                    | _ -> ". "^(term_to_string sg (push_lvar x env) tm1))
      | _ -> raise (Failure "Print.labs_term_to_string")

    and app_term_to_string sg env tm =
      match tm with
        Lambda.App (tm1, tm2) 
          -> (match tm1 with
                Lambda.App _
                  -> app_term_to_string sg env tm1
              | _ -> atomic_term_to_string sg env tm1)^" "^
             (atomic_term_to_string sg env tm2)
      | _ -> raise (Failure "Print.app_term_to_string")

    let sig_entry_to_string sg e =
      match e with
        Signature.Type_declaration (x, _, ki)
          -> x^" : "^(kind_to_string sg (E ([], [])) ki)
      | Signature.Term_declaration (x, _, ty)
          -> x^" : "^(type_to_string sg (E ([], [])) ty)

    let print_signature sg =
      let n = Signature.size sg
      in
      let rec print_sig i =
        if i < n
        then (print_string (sig_entry_to_string sg (Signature.lookup i sg));
              print_string ";\n";
              print_sig (i+1))
        else ()
      in
      print_sig 0

  end
