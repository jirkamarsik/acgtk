(** This module provides a type for the current parsed structure. It
aims at giving useful indication to parse errors *)

(** Entry is the actual module *)
module Entry :
sig

  (** The type for the automaton corresponding to the general data
      (signature or lexicon) being parsed so far *)
  type data
    
  (** The type for the automaton corresponding to the term being parsed
      so far (it is intented to be used in some interaction process)
  *)
  type term

  (** The type of the tokens or valuation found on the edges of the
      automaton *)
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


  (** This exception specifies the expected tokens at some point *)
  exception Expect of valuation list

  (** [start_data ()] returns an empty data corresponding to the
  initial state of the automaton *)
  val start_data : unit -> data

  (** [start_term ()] returns an empty data corresponding to the
  initial state of the automaton *)
  val start_term : unit -> term

  (** [data_transition d v] returns the state of the automaton reached
      from [d] with valuation [v] *)
  val data_transition : data -> valuation -> data

  (** [term_transition d v] returns the state of the automaton reached
      from [d] with valuation [v] *)
  val term_transition : term -> valuation -> term

  (** [valuation_to_string v] returns a tring describing it *)
  val valuation_to_string : valuation -> string

      
end
