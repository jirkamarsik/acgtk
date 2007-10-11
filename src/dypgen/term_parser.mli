type token =
  | SYMBOL of (string)
  | IDENT of (string)
  | LAMBDA0
  | LAMBDA
  | BINDER
  | INFIX
  | PREFIX
  | TYPE
  | END_OF_DEC
  | SIG_OPEN
  | DOT
  | RPAREN of (Lexing.position)
  | LPAREN of (Lexing.position)
  | COMMA
  | COLON
  | SEMICOLON
  | EQUAL
  | EOI

module Dyp_priority_data :
sig
  val priority_data : Dyp.priority_data
  val default_priority : Dyp.priority
  val app : Dyp.priority
  val at : Dyp.priority
  val bind : Dyp.priority
end

val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Abs.term list) * Dyp.priority) list

