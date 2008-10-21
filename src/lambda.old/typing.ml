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

open Lambda
open Signature

module Typing =

  struct

    exception Error of int

    exception Non_normal_form

    type environment = E of Lambda.stype list * Lambda.stype list

    let empty = E ([], [])

    let push_var ty (E (l1, l2)) = E (ty::l1, l2)

    let push_lvar ty (E (l1, l2)) = E (l1, ty::l2)

    let type_of_var i (E (l, _)) = List.nth l i

    let type_of_lvar i (E (_, l)) = List.nth l i



(*

    let kind_of_atom i = Signature.kind_of_atom i Sg.s

    let rec split_type_app ty =
      match ty with
        Lambda.TApp (ty1, tm) -> let (hd, args) = split_type_app ty1
                                 in
                                 (hd, tm::args)
      | _                     -> (ty, [])

    let rec kind_check env ty ki =
      match ty with
        Lambda.Atom i
      | Lambda.DAtom i             -> let ki1 = kind_of_atom i
                                      in
                                      if ki1 = ki 
                                      then ()
                                      else raise (Error 0)
      | Lambda.Lfun (ty1, ty2)     -> if ki = Lambda.Type
                                      then (kind_check env ty1 Lambda.Type;
                                            kind_check env ty2 Lambda.Type)
                                      else raise (Error 1)
      | Lambda.Dprod (_, ty1, ty2) -> if ki = Lambda.Type
                                      then (kind_check env ty1 Lambda.Type;
                                            kind_check (push_var ty1 env) 
                                                       ty2 Lambda.Type)
                                      else raise (Error 2)
      | Lambda.Record (_, tyls)    -> if ki = Lambda.Type
                                      then (List.map
                                             (fun t ->
                                                kind_check env t Lambda.Type)
                                             tyls;
                                            ())
                                      else raise (Error 3)
      | Lambda.Variant (_, tyls)   -> if ki = Lambda.Type
                                      then (List.map
                                             (fun t -> 
                                               kind_check env t Lambda.Type)
                                             tyls;
                                            ())
                                      else raise (Error 4)
      | Lambda.TAbs (_, ty1)       -> (match ki with
                                         Lambda.Depend (ty2, ki1)
                                           -> kind_check (push_var ty2 env)
                                                         ty1 ki1
                                       | _ -> raise (Error 5))
      | Lambda.TApp _              -> let (hd, args) =  split_type_app ty
                                      in
                                      (match hd with
                                         Lambda.Atom i
                                       | Lambda.DAtom i 
                                           -> kind_check_app 
                                                env (kind_of_atom i) args ki
                                       | Lambda.TAbs _  
                                           -> raise Non_normal_form
                                       | _ -> raise (Error 6))

    and     kind_check_app env ki1 args ki =
              match args with 
                []     -> if ki1 = ki 
                          then ()
                          else raise (Error 7)
              | tm::rm -> (match ki1 with
                             Lambda.Depend (ty, ki2)
                               -> (type_check env tm ty;
                                   kind_check_app env ki2 rm ki)
                           | _ -> raise (Error 8))

    and     type_check env tm ty = ()

*)

  end
