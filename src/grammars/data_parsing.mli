open Abstract_syntax

module Data_parsing :
  sig 
    val signature : string -> Environment.t
    val term : string -> Abstract_sig.t -> Abstract_sig.term option
  end
