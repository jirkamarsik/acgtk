type token =
  | DOT
  | LAMBDA0 of (string)
  | LAMBDA of (string)
  | IDENT of (string)
  | RPAREN
  | LPAREN
  | EOI

module Dyp_priority_data :
sig
  val priority_data : Dyp.priority_data
  val default_priority : Dyp.priority
  val app : Dyp.priority
  val at : Dyp.priority
  val bind : Dyp.priority
end

val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((term) * Dyp.priority) list

