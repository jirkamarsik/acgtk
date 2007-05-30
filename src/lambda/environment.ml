open Tries
open Signature
open Lexicon

module Environment =

  struct

    type error =
      | Not_found of string
      | Conflict of string

    let error_to_string = function
      | Not_found s -> Printf.sprintf "%s not found" s
      | Conflict s -> s

    exception Error of string

    let raise_error e = raise (Error (error_to_string e))

    type t = Env of (Signature.t  Tries.t* Lexicon.t Tries.t)

    let empty = Env (Tries.empty , Tries.empty)

    let insert_signature name sg (Env (tr_sg,tr_lx)) =
      try
	Env (Tries.insert name  sg tr_sg,tr_lx)
      with
	| Tries.Conflict -> raise_error (Conflict (Printf.sprintf "Signature \"%s\" already defined\n" name))


    let insert_lexicon name lx (Env (tr_sg,tr_lx)) =
      try
	Env (tr_sg,Tries.insert name lx tr_lx)
      with
	| Tries.Conflict -> raise_error (Conflict (Printf.sprintf "Lexicon \"%s\" already defined\n" name))

    let get_signature name (Env (s, _)) = 
      try
	Tries.lookup name s
      with
	| Tries.Not_found -> raise_error (Not_found (Printf.sprintf "Signature \"%s\"" name))

    let get_lexicon name (Env (_, l)) = 
      try
	Tries.lookup name l 
      with
	| Tries.Not_found -> raise_error (Not_found (Printf.sprintf "Lexicon \"%s\"" name))

  end
