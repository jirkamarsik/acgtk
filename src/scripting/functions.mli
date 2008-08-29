open Environment

module type Action_sig = 
sig

  type env

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




  type file_type = | Data | Script of (string -> env -> env)

  val load : file_type -> string -> env -> env

  val list : env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : unit -> unit
  val dont_trace : unit -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> unit

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> env -> env

  val wait : unit -> unit

  val dont_wait : unit -> unit

  val should_wait : unit -> bool

  val help : action -> unit

  val exit : unit -> unit
end


module Make(E:Environment_sig) : Action_sig with type env=E.t
