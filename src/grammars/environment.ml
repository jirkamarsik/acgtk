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

(*module type Signature_sig =
sig
  (** Exceptions raised when definitions of types or constants are
      duplicated *)
  
  exception Duplicate_type_definition
  exception Duplicate_term_definition

  (** The type of the signature as abstract object *)
  type t

  (** The (ocaml) type for the terms of the signature *)
  type term

  (** The (ocaml) type for the types of the signature *)
  type stype

    
  (** [empty name] returns the empty signature of name [name] *)
  val empty : (string*Abstract_syntax.location) -> t
    
  (** [name s] returns the name of the signature [s] and the location of its definition *)
  val name : t -> (string*Abstract_syntax.location)
    
  (** [add_entry e s] returns a signature where the entry [e] has been
      added *)
  val add_entry : Abstract_syntax.sig_entry -> t -> t
    
  (** [is_atomic_ype id s ] returns [true] if [id] is the name of an
      atomic type in [s] and [false] oterwise *)
  val is_type : string -> t -> bool
    
  (** [is_constant id s ] returns [(true,Some b)] together with its
      syntactic behaviour [b] if [id] is the name of a constant in [s]
      and [false,None] oterwise *)
  val is_constant : string -> t -> bool * Abstract_syntax.syntactic_behavior option

  (** [add_warnings w s ] resturns a signature where the warning [w] have been added *)
  val add_warnings : Error.warning list -> t -> t

  (** [get_warnings sg] returns the warnigs emitted while parsing [sg]. *)
  val get_warnings : t -> Error.warning list

  (** [to_string sg] returns a string describing the signature
      [sg]. Should be parsable *)
  val to_string : t -> string

  (** [term_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val term_to_string : term -> t -> string

  (** [type_to_string t sg] returns a string describing the term [t]
      wrt the signature [sg]. *)
  val type_to_string : stype -> t -> string
    
  (** [convert_term t ty sg] returns a the term corresponding to the
      parsed term [t] with parsed type [ty] wrt to the signature [sg]
  *)
  val convert_term : Abstract_syntax.term -> Abstract_syntax.type_def -> t -> term * stype

  (** [convert_type ty sg] returns a type to the parsed type [ty] wrt
      to the signature [sg] *)
  val convert_type : Abstract_syntax.type_def -> t -> stype
  val get_binder_argument_functional_type : string -> t -> Abstract_syntax.abstraction option


end
  *)


module type Environment_sig =
sig
  exception Signature_not_found of string
  exception Lexicon_not_found of string
  exception Entry_not_found of string


  module Signature1:Signature_sig
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
