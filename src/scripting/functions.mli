open Environment

module Make(E:Environment_sig) :
sig


  type file_type = | Data | Script of (string -> E.t -> E.t)

  val load : file_type -> string -> E.t -> E.t

  val list : E.t -> unit

  val select : string -> E.t -> E.t

  val unselect : E.t -> E.t

  val trace : unit -> unit

  val print : ?name:string -> E.t -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> E.t -> ?offset:string -> string -> unit

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> E.t -> E.t

  val wait : unit -> unit

  val dont_wait : unit -> unit

  val should_wait : unit -> bool
end
