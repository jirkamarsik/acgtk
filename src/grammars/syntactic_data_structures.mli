open Abstract_syntax

(** This modules implements the abstract syntax and the build function for the signatures *)

module Abstract_sig : Interface.Signature_sig with type entry = Abstract_syntax.sig_entry

module Abstract_lex : Interface.Lexicon_sig with type Signature.t = Abstract_sig.t
