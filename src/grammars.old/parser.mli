open Abstract_syntax

(** This modules implements the parser for both signature and lexicon files *)
module Parser :
sig
  
  (** This exception is raised whenever a parse error is
      encountered. The string parameter is the related error message *)
  exception Parse_Error of string

  (** The associated type *)
  type t =
    | Signature of Abstract_sig.t
    | Lexicon of Abstract_lexicon.t
	
  (** [parse file] parses [file] and returns a list of the signatures
      and lexicons that [file] defined *)
  val parse : string -> Environment.Environment.t -> (Environment.Environment.t * t list)

  (** [to_string p] outputs a string describing either a signature or a lexicon *)
  val to_string : t -> string
end
