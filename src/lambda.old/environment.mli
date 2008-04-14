open Signature
open Lexicon

module Environment :
sig
  type t
  exception Error of string
  val empty : t
  val insert_signature : string -> Signature.t -> t -> t
  val insert_lexicon : string -> Lexicon.t -> t -> t
  val get_signature : string -> t -> Signature.t
  val get_lexicon : string -> t -> Lexicon.t
end
