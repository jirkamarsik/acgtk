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

open Table
open Tries
open Syntactic_data_structures
open Interface
open Abstract_syntax
open Error

module Lambda (*: Signature_sig*) =
  struct
    module Table : TABLE = Make_table (struct let b = 10 end)

    type term =
      | Var of int
	    (** If the term is variable (bound by a binder)*)
      | LVar of int
	    (** If the term is variable (bound by a binder)*)
      | Const of int
	    (** If the term is a constant (not bound by a binder) *)
      | DConst of int
	    (** If the term is a constant (not bound by a binder) *)
      | LAbs of string * term
	    (** If the term is a linear abstraction *)
      | App of term * term
	    (** If the term is an application *)	
	    
    type type_def = 
      | Type_atom of int * term list
	    (** If the type is atomic. The third parameter is the terms to
		which the type is applied in case of a dependent type. The
		list is empty if the type does not depend on any type *)
      | Linear_arrow of type_def * type_def
	    (** If the type is described with a linear abstraction *)

	    
    type kind = K of type_def list

    let rec find level = function
	[] -> raise (Failure "Sign.find : Not_found")
      | v::l -> 
	  if level = 1
	  then v
	  else find (level - 1) l

    let rec inc_index = function
	[] -> []
      | (s,i)::ls -> (s,i+1)::(inc_index ls)
				
    let add_assoc ind_assoc s =
      (s,1)::(inc_index ind_assoc)


    let to_string t f sg =
      let rec rec_to_string t lin_ind_list ind_list =
	match t with
	| Var i -> let (s,_) = find i ind_list in s
	| LVar i -> let (s,_) = find i lin_ind_list in s
	| Const i -> f i sg
	| DConst i -> f i sg
	| LAbs (s,t) -> 
	    let t' = rec_to_string t (add_assoc lin_ind_list s) ind_list in
	    let vars,new_ind_assoc,u =
	      unfold_labs [s] (add_assoc lin_ind_list s) t in
	    Printf.sprintf
	      "(lambda %s. %s)"
	      (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	      (rec_to_string u lin_ind_list ind_list)
	| App (t1,t2) ->
	    Printf.sprintf
	      "(%s %s)"
	      (rec_to_string t1 lin_ind_list ind_list)
	      (rec_to_string t2 lin_ind_list ind_list)
      and unfold_labs acc ind_assoc = function
	| LAbs (s,t) -> 
	    unfold_labs (s::acc) (add_assoc ind_assoc s) t
	| t -> acc,ind_assoc,t
      in
      rec_to_string t [] []
	    
  end
