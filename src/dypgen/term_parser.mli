type token =
  | DOT
  | LAMBDA0
  | LAMBDA
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

val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Abs.term option) * Dyp.priority) list

# 50 "term_parser.dyp"
     
  val error : unit -> string
# 250                "term_parser.ml"
