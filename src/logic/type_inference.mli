(** This module is provides a type inference algorithm for linear lambda terms *)

open Lambda

(** The module that provides the type inference algorithm *)
module TypeInference :
sig

  (** the list of exceptions used in the module *)
  exception NotImplemented
  exception NotUnifiable
  exception NonLinear

  (** the type of substitutions on term types used in the unify algorithm *)
  type subst

  (** [substitute i s] returns the type associated to atom [i] in substitution [s] *)
  val substitute : int -> subst -> Lambda.stype
  (** [lift_subst s ty] returns the type [ty] in which substitution [s] has been applied*)
  val lift_subst : subst -> Lambda.stype -> Lambda.stype
  (** [occurs i ty] returns true if [i] is an atomic type that appears in [ty] *)
  val occurs : int -> Lambda.stype -> bool
  (** [unify (l0,r0)...(ln,rn)] returns the most general unifier for the unification problem:
l0=r0,...,ln=rn if it exists,raises NotUnifiable otherwise *)
  val unify : (Lambda.stype * Lambda.stype) list -> subst
  (** [rename_vars t] returns a lambda term in which de Bruijn's indices have been replaced
by static indices and the constants made distinct,the mapping of each lambda to its new
variable and the number of constants in [t] *)
  val rename_vars : Lambda.term -> Lambda.term * (int * int) list * int
  (** [type_inference_aux m] returns the most general type of m in which constants have been
replaced by variables and the type of these variables*)
  val type_inference_aux : Lambda.term -> Lambda.stype * Lambda.stype list

end
