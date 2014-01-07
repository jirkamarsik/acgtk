(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open Environment

module type Action_sig = 
sig

  type env

  exception Not_yet_implemented of string
  exception Stop

  type action =
    | Load
    | List
    | Select
    | Unselect
    | Trace
    | Dont_trace
    | Print
    | Analyse
    | Add
    | Compose
    | Dont_wait
    | Wait
    | Help of action option
    | Create
    | Save
    | Parse




  type file_type = | Data | Script of (string -> string list -> env -> env)

  val load : file_type -> string -> string list -> env -> env

  val list : env -> unit

  val select : string -> (Lexing.position * Lexing.position) -> env -> env

  val unselect : env -> env

  val trace : unit -> unit
  val dont_trace : unit -> unit

  val print : ?name:string -> env -> (Lexing.position * Lexing.position) -> unit

  val analyse : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> unit

  val parse : ?name:(string * (Lexing.position * Lexing.position)) -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> unit

  val add : ?names:(string * (Lexing.position * Lexing.position)) list -> env -> ?offset:string -> string -> (Lexing.position * Lexing.position) -> env

  val compose : 
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) ->
    string * (Lexing.position * Lexing.position) -> env -> env

  val wait : unit -> unit

  val dont_wait : unit -> unit

  val should_wait : unit -> bool

  val help : action -> unit

  val exit : unit -> unit


  val create_sig :  (string * (Lexing.position * Lexing.position)) -> env -> env


  val create_lex :  abs:(string * (Lexing.position * Lexing.position)) -> obj:(string * (Lexing.position * Lexing.position)) -> (string * (Lexing.position * Lexing.position)) -> env -> env

  val save : ?names:(string * (Lexing.position * Lexing.position)) list -> string -> env -> (Lexing.position * Lexing.position) -> unit


end


module Make(E:Environment_sig) : Action_sig with type env=E.t
