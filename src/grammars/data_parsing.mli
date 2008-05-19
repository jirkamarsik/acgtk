(** This module provides an interface to parsing data files
    (i.e. containing signatures and lexicons) to put them in an
    {!Abstract_sig.Environment}, or to parse strings as terms *)

open Abstract_syntax


(** Data_parsing is the actual module *)
module Data_parsing :
sig 
  (** [data filename e] adds the data (signatures or lexicons) to [e]
  and returns the resulting environment *)
  val data : string -> Environment.t  -> Environment.t

  (** [term s sg] returns [Some t] with [t] being an
  {!Abstract_sig.Abstract_sig.term} if [s] is parsable, and [None]
  otherwise *)
  val term : string -> Abstract_sig.t -> Abstract_sig.term option
end
