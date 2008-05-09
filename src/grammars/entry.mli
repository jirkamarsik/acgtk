module Entry :
sig
  type data

  type valuation =
    | EOI
    | Sig_kwd
    | Id
    | Equal
    | Comma
    | Colon
    | Type_kwd
    | End_kwd
    | Semi_colon
    | Type_or_term
    | Prefix_kwd
    | Infix_kwd
    | Binder_kwd
    | Sym

 
  exception Expect of valuation list

  val start_data : unit -> data

  val transition : data -> valuation -> data

  val valuation_to_string : valuation -> string

      
end
