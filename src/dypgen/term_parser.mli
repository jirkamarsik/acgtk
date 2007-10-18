
type token =
  | SYMBOL of ((string*Abstract_syntax.Abstract_sig.location))
  | IDENT of ((string*Abstract_syntax.Abstract_sig.location))
  | LIN_ARROW of (Abstract_syntax.Abstract_sig.location)
  | ARROW of (Abstract_syntax.Abstract_sig.location)
  | LAMBDA0 of (Abstract_syntax.Abstract_sig.location)
  | LAMBDA of (Abstract_syntax.Abstract_sig.location)
  | BINDER of (Abstract_syntax.Abstract_sig.location)
  | INFIX of (Abstract_syntax.Abstract_sig.location)
  | PREFIX of (Abstract_syntax.Abstract_sig.location)
  | TYPE of (Abstract_syntax.Abstract_sig.location)
  | END_OF_DEC of (Abstract_syntax.Abstract_sig.location)
  | SIG_OPEN of (Abstract_syntax.Abstract_sig.location)
  | DOT of (Abstract_syntax.Abstract_sig.location)
  | RPAREN of (Abstract_syntax.Abstract_sig.location)
  | LPAREN of (Abstract_syntax.Abstract_sig.location)
  | COMMA of (Abstract_syntax.Abstract_sig.location)
  | COLON of (Abstract_syntax.Abstract_sig.location)
  | SEMICOLON of (Abstract_syntax.Abstract_sig.location)
  | EQUAL of (Abstract_syntax.Abstract_sig.location)
  | EOI

module Dyp_priority_data :
sig
  val priority_data : Dyp.priority_data
  val default_priority : Dyp.priority
  val app : Dyp.priority
  val arrow_type : Dyp.priority
  val atom : Dyp.priority
  val atom_type : Dyp.priority
  val binder : Dyp.priority
end

val signature : ?global_data:unit -> ?local_data:unit -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Abstract_syntax.Abstract_sig.t) * Dyp.priority) list

