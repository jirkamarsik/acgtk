open Interface


module type Environment_sig =
sig
  exception Signature_not_found of string


  module Signature:Signature_sig
  module Lexicon:Lexicon_sig with type signature=Signature.t

  type t
  type entry = 
    | Signature of Signature.t
    | Lexicon of Lexicon.t
  val empty : t
  val insert : entry -> t -> t
  val get_signature : string -> t -> Signature.t
  val iter : (entry -> unit) -> t -> unit
  val fold : (entry -> 'a -> 'a) -> 'a -> t -> 'a
  val sig_number : t -> int
  val choose_signature : t -> Signature.t option 
end

  
module Make (Sg:Signature_sig)(Lex:Lexicon_sig with type signature = Sg.t) =
struct
  module Signature=Sg
  module Lexicon=Lex

  exception Signature_not_found of string
    
  module Env = Utils.StringMap

  type entry = 
    | Signature of Sg.t
    | Lexicon of Lex.t

  type t = {map:entry Env.t;sig_number:int;lex_number:int}

  let empty = {map=Env.empty;sig_number=0;lex_number=0}

  let insert d e = match d with
    | Signature s -> let name,(p1,p2) = Sg.name s in
	if not (Env.mem name e.map)
	then
	  {e with map=Env.add name d e.map ;sig_number=e.sig_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_signature name,(p1,p2))))
    | Lexicon l -> let name,(p1,p2) = Lex.name l in
	if not (Env.mem name e.map)
	then
	  {e with map=Env.add name d e.map ;lex_number=e.lex_number+1}
	else
	  raise (Error.Error (Error.Env_error (Error.Duplicated_lexicon name,(p1,p2))))

  let iter f {map=e} =  Env.iter (fun _ d -> f d) e

  let fold f a {map=e} = Env.fold (fun _ d acc -> f d acc) e a

  let sig_number {sig_number=n} = n

  let get_signature s {map=e} =
    try
      match Env.find s e with
	| Signature sg -> sg
	| Lexicon _ -> raise (Signature_not_found s)
    with
      | Not_found -> raise (Signature_not_found s)


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
