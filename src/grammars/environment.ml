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

open Interface
open Abstract_syntax



module type Environment_sig =
sig
  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string


(*  module Signature1:Signature_sig*)

  module Signature1:Signature_sig with type term=Lambda.Lambda.term
  module Lexicon:Interface.Lexicon_sig with type Signature.t=Signature1.t and type Signature.term=Signature1.term and type Signature.stype=Signature1.stype


  type t
  type entry = 
    | Signature of Signature1.t
    | Lexicon of Lexicon.t
  val empty : t
  val insert : ?override:bool -> entry -> t -> t
  val get_signature : string -> t -> Signature1.t
  val get_lexicon : string -> t -> Lexicon.t
  val get : string -> t -> entry 
  val append : ?override:bool -> t -> t -> t
  val iter : (entry -> unit) -> t -> unit
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a
  val sig_number : t -> int
  val lex_number : t -> int
  val choose_signature : t -> Signature1.t option 

  val select : string -> t -> t

  val unselect : t -> t

  val focus : t -> entry option
end

  
module Make (Lex:Interface.Lexicon_sig) =
struct
(*  module Signature=Sg*)
  module Lexicon=Lex
  module Sg=Lex.Signature
  module Signature1=Sg

  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string
  module Env = Utils.StringMap

  type entry = 
    | Signature of Sg.t
    | Lexicon of Lex.t

  type t = {map:entry Env.t;sig_number:int;lex_number:int;focus:entry option}

  let empty = {map=Env.empty;sig_number=0;lex_number=0;focus=None}

  let append ?(override=false) e1 e2 =
    let erased_sig = ref 0 in
    let erased_lex = ref 0 in
    let new_map =
      Env.merge
	(fun k v1 v2 ->
	  match v1,v2,override with
	  | None,None,_ -> None
	  | None,Some v,_ -> Some v
	  | Some v,None,_ -> Some v
	  | Some (Lexicon _),Some v2,true -> 
	    let () = erased_lex:=!erased_lex+1 in
	    Some v2
	  | Some (Signature _),Some v2,true -> 
	    let () = erased_sig:=!erased_sig+1 in
	    Some v2
	  | Some v1,Some v2,false ->
	    match v2 with
	    | Signature sg ->
	      let _,pos=Sg.name sg in
	      raise (Error.Error (Error.Env_error (Error.Duplicated_entry k,pos)))
	    | Lexicon lex ->
	      let _,pos=Lex.name lex in
	      raise (Error.Error (Error.Env_error (Error.Duplicated_entry k,pos))))
	e1.map
	e2.map in
    {map=new_map;
     sig_number=e1.sig_number + e2.sig_number - !erased_sig;
     lex_number=e1.lex_number + e2.lex_number - !erased_lex;
     focus = match e2.focus with
     | Some e -> Some e
     | None -> e1.focus}
    

  let insert ?(override=false) d e = match d with
    | Signature s -> let name,(p1,p2) = Sg.name s in
	if (not (Env.mem name e.map))||override
	then
	  {e with map=Env.add name d e.map ;sig_number=e.sig_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_signature name,(p1,p2))))
    | Lexicon l -> let name,(p1,p2) = Lex.name l in
	if not (Env.mem name e.map)||override
	then
	  {e with map=Env.add name d e.map ;lex_number=e.lex_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_lexicon name,(p1,p2))))

  let iter f {map=e} =  Env.iter (fun _ d -> f d) e

  let fold f a {map=e} = Env.fold (fun _ d acc -> f d acc) e a

  let sig_number {sig_number=n} = n
  let lex_number {lex_number=n} = n

  let get_signature s {map=e} =
    try
      match Env.find s e with
	| Signature sg -> sg
	| Lexicon _ -> raise (Signature_not_found s)
    with
      | Not_found -> raise (Signature_not_found s)

  let get_lexicon s {map=e} =
    try
      match Env.find s e with
	| Signature _ -> raise (Lexicon_not_found s)
	| Lexicon lex -> lex
    with
      | Not_found -> raise (Lexicon_not_found s)


  let get s {map=e} =
    try
      Env.find s e 
    with
      | Not_found -> raise (Entry_not_found s)

  let select name e =
    {e with focus=Some (get name e)}

  let unselect e = {e with focus=None}

  let focus {focus=f} = f



  exception Sig of Sg.t    

  let choose_signature {map=e} =
    try
      let () = Env.fold
	(fun _ c a -> 
	   match c with
	     | Signature s -> raise (Sig s)
	     | Lexicon _ -> a )
	e
	() in
	None
    with
      | Sig s -> Some s
    


end
