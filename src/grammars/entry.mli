module Entry :
sig
  type data

  type term

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
    | Type_or_term of type_or_term_tokens
    | Prefix_kwd
    | Infix_kwd
    | Binder_kwd
    | Sym
  and type_or_term_tokens =
    | LPAR
    | RPAR
    | DOT
    | LAMBDA
    | ARROW


 
  exception Expect of valuation list

(*  exception Unexpected_token of type_or_term_kind *)

  val start_data : unit -> data

  val start_term : unit -> term

  val data_transition : data -> valuation -> data

  val term_transition : term -> valuation -> term

  val valuation_to_string : valuation -> string

      
end
