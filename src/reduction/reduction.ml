open Lambda
open Acg_lexicon
open Datalog_solver

module Actual_reduction = Reduction_functor.Make (Sylvain_lexicon) (Datalog_solver)
