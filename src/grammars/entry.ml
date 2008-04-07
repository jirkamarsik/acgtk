module Entry =
struct
  let log_on_stderr = false

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

  let to_string = function
    | Signatures -> "Signatures"
    | Signature -> "Signature"
    | Sig_entries -> "Sig_entries"
    | Sig_entry -> "Sig_entry"
    | Type_decl -> "Type_decl"
    | Type_def -> "Type_def"
    | Term_decl -> "Term_decl"
    | Term_def -> "Term_def"
    | Comma_ids -> "Comma_ids"
    | Type_exp -> "Type_exp"
    | Atomic_type -> "Atomic_type"
    | Term_dec_start -> "Term_dec_start"
    | Term_def_start -> "Term_def_start"
    | Term -> "Term"
    | Binder -> "Binder"
    | Idents -> "Idents"
    | Atomic_term -> "Atomic_term"
    | Application -> "Application"
	
  let current_entry = ref None
    
  let local_set x s =
    let () =
      if log_on_stderr then
	Printf.fprintf stderr "%s \"%s\" \n%!" (to_string x) s 
      else
	() in
      current_entry := Some x
	
  let set e s = match e with
    | Signatures -> local_set Signatures s
    | Signature -> local_set Signature s
    | Sig_entries -> local_set Sig_entries s
    | Sig_entry -> local_set Sig_entry s
    | Type_decl -> local_set Type_decl s
    | Type_def -> local_set Type_def s
    | Term_decl -> local_set Term_decl s
    | Term_def -> local_set Term_def s
    | Comma_ids -> local_set Comma_ids s
    | Type_exp -> local_set Type_exp s
    | Atomic_type -> local_set Atomic_type s
    | Term_dec_start -> local_set Term_dec_start s
    | Term_def_start -> local_set Term_def_start s
    | Term -> local_set Term s
    | Binder -> local_set Binder s
    | Idents -> local_set Idents s
    | Atomic_term -> local_set Atomic_term s
    | Application -> local_set Application s
	
	
end
