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
open Table
open Signature

module Lexicon =
  
  struct

    exception Not_yet_implemented

    type entry =
      Type_realisation of Lambda.stype
    | Term_realisation of Lambda.term

    module Table = Make_table (struct let b = 10 end)

    type t = Lx of Signature.t * Signature.t * entry Table.t
    
    let create sg1 sg2 = Lx (sg1, sg2, Table.create())

    let insert_type_r i ty (Lx (sg1, sg2, tb)) = 
      Lx (sg1, sg2, Table.insert i (Type_realisation ty) tb)

    let insert_term_r i tm (Lx (sg1, sg2, tb)) = 
      Lx (sg1, sg2, Table.insert i (Term_realisation tm) tb)

    let get_type_r i (Lx (_, _, tb)) = 
      match Table.lookup i tb with
        Type_realisation ty -> ty
      | Term_realisation _  -> raise (Failure "Lexicon:get_type_r")

    let get_term_r i (Lx (_, _, tb)) = 
      match Table.lookup i tb with
        Type_realisation _  -> raise (Failure "Lexicon:get_term_r")
      | Term_realisation tm -> tm

    let rec realize_term lx tm =
      match tm with
        Lambda.Var _          -> tm
      | Lambda.LVar _         -> tm
      | Lambda.Const i        -> get_term_r i lx
      | Lambda.Abs  (x, tm1)  -> Lambda.Abs  (x, realize_term lx tm1)
      | Lambda.LAbs (x, tm1)  -> Lambda.LAbs (x, realize_term lx tm1)
      | Lambda.App (tm1, tm2) -> Lambda.App (realize_term lx tm1,
                                             realize_term lx tm2)
      | _                     -> raise Not_yet_implemented

    and realize_type lx ty =
      match ty with
        Lambda.Atom i              -> get_type_r i lx
      | Lambda.LFun (ty1, ty2)     -> Lambda.LFun (realize_type lx ty1,
                                                   realize_type lx ty2)
      | Lambda.Dprod (x, ty1, ty2) -> Lambda.Dprod (x, realize_type lx ty1, 
                                                       realize_type lx ty2)
      | Lambda.TApp (ty1, tm)      -> Lambda.TApp (realize_type lx ty1,
                                                   realize_term lx tm)
      | _                          -> raise Not_yet_implemented

    and realize_kind lx ki =
      match ki with
        Lambda.Type             -> Lambda.Type
      | Lambda.Depend (ty, ki1) -> Lambda.Depend (realize_type lx ty,
                                                  realize_kind lx ki1)


  end
