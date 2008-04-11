module Data_parsing :
  sig 
    val signature : string -> Parser.content
    val term : string -> Abstract_syntax.Abstract_sig.t -> Abstract_syntax.Abstract_sig.term
  end
