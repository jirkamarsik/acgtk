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

open Printf
open Tries
	
module Abstract_sig =
struct

  type location = Token.flocation


  type symbol =
    | Prefix of string
    | Infix of string
    | Outfix of string*string
    | Binder of string

  type term =
    | Id of string * location
	(** I f the term is either a constant or a variable (not to
	    determine it during parsing) *)
    | Symbol of symbol * location
    | Abs of string * term * location
	(** If the term is a intuitionistic abstraction *)
    | LAbs of string * term * location
	(** If the term is a linear abstraction *)
    | App of term * term * location
      (** If the term is an application *)
      
      
  type type_def =
    | Type_atom of string * location * term list
	(** If the type is atomic *)
    | Linear_arrow of type_def * type_def * location
	(** If the type is described with a linear abstraction *)
    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def) * location
	(** If the type is a dependent type build with an abstraction *)
    
    
  let is_atomic_term = function
    | Id _ -> true
    | _ -> false
      

  let extract_term_location = function
    | Id (_,l) -> l
    | Symbol (_,l) -> l
    | Abs (_,_,l) -> l
    | LAbs (_,_,l) -> l
    | App (_,_,l) -> l

  let extract_type_location = function
    | Type_atom (_,l,_) -> l
    | Linear_arrow (_,_,l) -> l
    | Arrow (_,_,l) -> l
    | Dep (_,_,l) -> l
    | Type_Abs (_,l) -> l

  let rec term_to_string = function
    | Id (s,_) -> s
    | Symbol (Prefix s,_) -> s
    | Symbol (Infix s,_) -> s
    | Symbol (Outfix(s1,s2),_) -> s1^s2
    | Symbol (Binder s,_) -> s
    | Abs (s,t,_) -> 
	let vars,u=unfold_abs [s] t in
	  sprintf
	    "Lambda %s. %s"
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u)
    | LAbs (s,t,_) -> 
	let vars,u=unfold_labs [s] t in
	  sprintf
	    "lambda %s. %s"
	    (Utils.string_of_list " " (fun x -> x) (List.rev vars))
	    (term_to_string u)
    | App (t1,t2,_) ->
	sprintf
	  "(%s %s)"
	  (term_to_string t1)
	  (term_to_string t2)
  and unfold_abs acc = function
    | Abs (s,t,_) -> unfold_abs (s::acc) t
    | t -> acc,t
  and unfold_labs acc = function
    | LAbs (s,t,_) -> unfold_labs (s::acc) t
    | t -> acc,t
  and unfold_app acc = function
    | App (t1,t2,_) -> unfold_app (t2::acc) t1
    | t -> acc,t
      
  let rec is_atomic_type = function
    | Type_atom _ -> true
    | Dep (_,t,_) -> is_atomic_type t 
    | _ -> false
      
  let is_arrow = function
    | Arrow _ -> true
    | Linear_arrow _ -> true
    | _ -> false
      
  let rec type_def_to_string = function
    | Type_atom (s,_,terms) ->
       (match terms with
          [] -> s
        | _  -> sprintf "%s %s" s (Utils.string_of_list " " (fun x -> sprintf "(%s)" (term_to_string x)) terms))
    | Linear_arrow (t1,t2,_) ->
	let arrows,u = unfold_linear_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u else sprintf "(%s)" (type_def_to_string u) in
	  sprintf
	    "%s -> %s"
	    (Utils.string_of_list
	       " -> "
	       (fun x -> if is_atomic_type x then type_def_to_string x else sprintf "(%s)" (type_def_to_string x))
	       (List.rev arrows))
	    u_string
    | Arrow (t1,t2,_) -> 
	let arrows,u = unfold_arrow [t1] t2 in
	let u_string = if (is_atomic_type u)||(is_arrow u) then type_def_to_string u else sprintf "(%s)" (type_def_to_string u) in
	  sprintf
	    "%s => %s"
	    (Utils.string_of_list
	       " => "
	       (fun x -> if (is_atomic_type x) then type_def_to_string x else sprintf "(%s)" (type_def_to_string x))
	       (List.rev arrows))
	    u_string
    | Dep ((s,_,ty1),ty2,_) ->
	let deps,u = unfold_dep [(s,ty1)] ty2 in
	sprintf 
	  "(%s) %s"
	  (Utils.string_of_list ", " (fun (id,t) -> sprintf "%s:%s" id (type_def_to_string t)) (List.rev deps))
          (if is_atomic_type u
	   then
	     type_def_to_string u
           else
	     sprintf "(%s)" (type_def_to_string u))
    | Type_Abs ((s,_,ty),_) -> 
	let abs,u = unfold_type_abs [s] ty in
	  sprintf
	    "lambda %s.(%s)"
	    (Utils.string_of_list " " (fun x -> x) (List.rev abs))
	    (type_def_to_string ty)
  and unfold_linear_arrow acc = function
    | Linear_arrow (t1,t2,_) -> unfold_linear_arrow (t1::acc) t2
    | t -> acc,t
  and unfold_arrow acc = function
    | Arrow (t1,t2,_) -> unfold_arrow (t1::acc) t2
    | t -> acc,t
  and unfold_dep acc = function
    | Dep ((s,_,t),t2,_) -> unfold_dep ((s,t)::acc) t2
    | t -> acc,t
  and unfold_type_abs acc = function
    | Type_Abs ((s,_,t),_) -> unfold_type_abs (s::acc) t
    | t -> acc,t



  type kind = K of type_def list
  type t = Signature of string * sig_content
  and sig_content = sig_entry list
  and sig_entry = 
    | Type_decl of (string * location * kind)
    | Term_decl of (string * location * type_def)
	
  let empty s = Signature (s,[])

  let insert_type_decl id types loc (Signature (name,content)) =
    Signature (name,(Type_decl (id,loc,K types))::content)
      
  let insert_term_decl id ty loc  (Signature (name,content)) =
    Signature (name,(Term_decl (id,loc,ty))::content)
      
  let to_string (Signature (name,dec)) =
    sprintf
      "signature %s = \n%s\nend"
      name
      (Utils.string_of_list
	 "\n"
	 (function
	    | Type_decl (id,_,K types) -> 
                (match types 
                 with [] -> sprintf "\t%s: type;" id
                   | _  -> sprintf "\t%s: (%s)type;" id (Utils.string_of_list "," type_def_to_string types))
	    | Term_decl (id,_,t) -> sprintf "\t%s: %s;" id (type_def_to_string t))
	 dec)
      
end

module Abstract_lexicon =
struct

  type term = Abstract_sig.term
  type type_def = Abstract_sig.type_def
  type location = Abstract_sig.location

(*  
  type t = Lexicon of string * string * string * lex_content
  and lex_content = lex_assignment list
  and lex_assignment = 
    | Type_assgt of string * location * type_def
    | Const_assgt of string * location * term
	
  let empty name abs_sg obg_sg = Lexicon (name,abs_sg,obg_sg,[])

  let insert_type_assgt id ty loc (Lexicon(name,abs_sg,obg_sg,lst)) =
      Lexicon (name,abs_sg,obg_sg,(Type_assgt (id,loc,ty))::lst)

  let insert_const_assgt id cst loc (Lexicon(name,abs_sg,obg_sg,lst)) =
      Lexicon (name,abs_sg,obg_sg,(Const_assgt (id,loc,cst))::lst)
*)

  type t = Lexicon of string * string * string * lex_content
  and lex_content = lex_assignment Tries.t
  and lex_assignment = 
    | Type_assgt of string * location * type_def
    | Const_assgt of string * location * term
	
  let empty name abs_sg obg_sg = Lexicon (name,abs_sg,obg_sg,Tries.empty)

  let insert_type_assgt id ty loc (Lexicon(name,abs_sg,obg_sg,tr)) =
      Lexicon (name,abs_sg,obg_sg,Tries.insert id (Type_assgt (id,loc,ty)) tr)

  let insert_const_assgt id cst loc (Lexicon(name,abs_sg,obg_sg,tr)) =
      Lexicon (name,abs_sg,obg_sg,Tries.insert id (Const_assgt (id,loc,cst)) tr)

  let to_string (Lexicon (name,abs_sg,obg_sg,tr)) = 
    sprintf
      "lexicon %s(%s): %s = \n%s\nend"
      name
      abs_sg
      obg_sg
      (Utils.string_of_list
	 "\n"
	 (function
	    | Type_assgt (id,_,ty) -> sprintf "\t%s:= %s;" id (Abstract_sig.type_def_to_string ty)
	    | Const_assgt (id,_,t) -> sprintf "\t%s:= %s;" id (Abstract_sig.term_to_string t))
	 (Tries.content tr))

end
