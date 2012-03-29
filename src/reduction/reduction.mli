open Lambda
open Reduction_functor
open Datalog_solver
open Program
open Acg_lexicon

(** This is the functor that provides the actual reduction *)
module Actual_reduction :
sig

  module Lexicon : Interface.Lexicon_sig with type t = Sylvain_lexicon.t 
  module Program1 : Program_sig with type program = Program.program
  module Solver : Datalog_solver_sig with type item = Datalog_solver.item

  (*val break_aux : int -> (int * int) list -> Lambda.stype -> int list * (int * int) list * int
  val break1 : Lambda.stype -> int -> int list list
  val break2 : Lambda.stype * Lambda.stype list -> int -> int list list * int list list
  val break3 : Lambda.stype * Lambda.stype list -> int list * int list list*)
  val database_query : Lexicon.Signature.t -> Program1.Signature1.signature -> Lambda.term -> Lambda.stype -> Solver.item list * Solver.item
  val program : Lexicon.t -> Program1.program
  val reduction : Lexicon.t -> Lambda.term -> Lambda.stype -> Program1.program * Solver.item list * Solver.item

end
