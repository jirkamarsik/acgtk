(** This module implements a depth-first array traversal. It is
    depth-first in order to fit with backtracking when cells contain
    persitent array. *)


module type Evaluator_TYPE =
  sig
    type state
    type cell
    val update: state -> cell -> cell option
    val collector: 'a -> state -> 'a
  end

module ArrayTraversal :
sig
  (** The type of a row *)
  type 'a row = 'a list

  (** The type of the array *)
  type 'a array = 'a row list

  (** [fold_on_results f acc a test] returns [f (... (f (f (f acc r1)
      r2) r3)... ) rN] where the [r1 ... rN] are paths from top to
      bottom of [a] such that for all [r=[a1 ; ... ; aK]] (all paths
      have this shape) that for all J [test aJ] holds.  *)
  val all_results : ('b -> 'a list -> 'b) -> 'b -> 'a array -> ('a -> bool) -> 'b
end
