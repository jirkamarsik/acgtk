type token =
  | SYMBOL of (string*(Abs.location))
  | IDENT of (string*(Abs.location))
  | LAMBDA0 of (Abs.location)
  | LAMBDA of (Abs.location)
  | BINDER of (Abs.location)
  | INFIX of (Abs.location)
  | PREFIX of (Abs.location)
  | TYPE of (Abs.location)
  | END_OF_DEC of (Abs.location)
  | SIG_OPEN of (Abs.location)
  | DOT of (Abs.location)
  | RPAREN of (Abs.location)
  | LPAREN of (Abs.location)
  | COMMA of (Abs.location)
  | COLON of (Abs.location)
  | SEMICOLON of (Abs.location)
  | EQUAL of (Abs.location)
  | EOI

module Dyp_priority_data :
sig
  val priority_data : Dyp.priority_data
  val default_priority : Dyp.priority
  val app : Dyp.priority
  val atom : Dyp.priority
  val binder : Dyp.priority
end

val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Abs.term list) * Dyp.priority) list

