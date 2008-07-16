open Environment

module Make(E:Environment_sig) :
sig

  exception Not_yet_implemented of string

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Compose
    | Dont_wait
    | Wait
    | Help of action option




  type file_type = | Data | Script of (string -> E.t -> E.t)

  val load : file_type -> string -> E.t -> E.t

  val list : E.t -> unit

  val select : string -> (Lexing.position * Lexing.position) -> E.t -> E.t

  val unselect : E.t -> E.t

  val trace : unit -> unit
  val dont_trace : unit -> unit

  val print : ?name:string -> E.t -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> E.t -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> unit

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> E.t -> E.t

  val wait : unit -> unit

  val dont_wait : unit -> unit

  val should_wait : unit -> bool

  val help : action -> unit

  val exit : unit -> unit
end
