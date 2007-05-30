open Lambda
open Table
open Tries

module Signature = 

  struct

    exception Not_found

    type 'a option = None | Some of 'a

    type entry =
      Type_declaration of string * int * Lambda.kind
    | Term_declaration of string * int * Lambda.stype

    module Table = Make_table (struct let b = 10 end)

    type t = Sg of int * entry Table.t * entry Tries.t

    let create() = Sg (0, Table.create(), Tries.empty)

    let size (Sg (n, _, _)) = n

    let insert_type_dcl id ki (Sg (size, tb, tr)) =
      let e = Type_declaration (id, size , ki)
      in
      Sg (size+1, Table.insert size e tb, Tries.insert id e tr)

    let insert_term_dcl id ty (Sg (size, tb, tr)) =
      let e = Term_declaration (id, size, ty)
      in 
      Sg (size+1, Table.insert size e tb, Tries.insert id e tr)

    let lookup i (Sg (_, tb, _)) = Table.lookup i tb

    let get_const (Sg (_, _, tr)) id =
      try
      (match (Tries.lookup id tr) with
        Term_declaration (_, i, ty) -> (i, ty)
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let get_atom (Sg (_, _, tr)) id =
      try
      (match (Tries.lookup id tr) with
        Type_declaration (_, i, ki) -> (i, ki)
       | _                          -> raise Not_found)
      with Tries.Not_found -> raise Not_found

    let string_of_const i (Sg (_, tb, _)) =
      match Table.lookup i tb with
        Type_declaration _         -> raise (Failure 
                                             "Signature.string_of_const")
      | Term_declaration (x, _, _) -> x 

    let string_of_atom i (Sg (_, tb, _)) =
      match Table.lookup i tb with
        Type_declaration (x, _, _) -> x 
      | Term_declaration _         -> raise (Failure 
                                             "Signature.string_of_atom")

(*
    let kind_of_atom i (Sg (_, tb)) =
      match Table.lookup i tb with
        Type_declaration (_, ki) -> ki
      | Term_declaration _       -> raise (Failure "Signature.kind_of_atom")
  
*)  

    let is_a_cst id (Sg (_, _, tr)) =
      try
	match (Tries.lookup id tr) with
          | Term_declaration _ -> true
	  | _ -> false
      with
	| Tries.Not_found -> false


    let is_a_type id (Sg (_, _, tr)) =
      try
	match (Tries.lookup id tr) with
          | Type_declaration _ -> true
	  | _ -> false
      with
	| Tries.Not_found -> false

  end
