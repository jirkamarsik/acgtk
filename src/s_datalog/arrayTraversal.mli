(** This module implements a depth-first array traversal. It is
    depth-first in order to fit with backtracking when cells contain
    persitent array. *)


module type Evaluator_TYPE =
  sig
    type state
    type cell
    val init : state
    val update: state -> cell -> state option
  end

module Make (E:Evaluator_TYPE) :
sig
  (** The type of a row *)
  type row = E.cell list

  (** The type of the array *)
  type array = row list

  (** [fold_on_results f acc a test] returns [f (... (f (f (f acc r1)
      r2) r3)... ) rN] where the [r1 ... rN] are paths from top to
      bottom of [a] such that for all [r=[a1 ; ... ; aK]] (all paths
      have this shape) that for all J [test aJ] holds.  *)
  val collect_results : ('a -> E.state -> 'a) -> 'a -> array -> 'a
end
