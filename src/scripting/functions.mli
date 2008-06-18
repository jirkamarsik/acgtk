open Environment

module Make(E:Environment_sig) :
sig


  type file_type = | Data | Script

  val load : file_type -> string -> E.t -> E.t

  val list : E.t -> unit

  val select : string -> E.t -> E.t

  val unselect : E.t -> E.t

  val trace : unit -> unit

  val print : ?name:string -> E.t -> unit

  val analyse : ?name:string -> E.t -> string -> unit

  val compose : string -> string -> string -> E.t -> E.t
end
