module Entry :
sig
  type t =
    | Signatures
    | Signature
    | Sig_entries
    | Sig_entry
    | Type_decl
    | Type_def
    | Term_decl
    | Term_def
    | Comma_ids
    | Type_exp
    | Atomic_type
    | Term_dec_start
    | Term_def_start
    | Term
    | Binder
    | Idents
    | Atomic_term
    | Application
	
  val set : t -> string -> unit
      
end
