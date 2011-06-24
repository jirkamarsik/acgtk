open Lambda
open Type_inference

(** This module type describes the interface with ACG signatures *)
module type Signature_sig =
sig

  type t
  (** the abstract type of signatures *)

  val interpret_constant : t -> int -> Lambda.stype
  (** [intrpret_constant s id] returns the type tau(c) where c is the constant with id [id], if it exists *)
  val get_atomic_types : t -> int list
  (** [get_atomic_types s] returns the list of identifiers of declared atomic types in [s]*)
  val get_constants : t -> (string * Lambda.stype) list
  (** [get_constants s] returns the list of the names of the constants in [s], with the associated type *)
  (**val get_tau : t -> int -> Lambda.term*)
  (** [get_tau s id] returns the type corresponding to the constant [id] if it exists *)
  val unfold_type_definition : int -> t -> Lambda.stype
  (** [unfold_type_definition id s] returns the actual type corresponding to [Lambda.DAtom id] if it exists *)
  val unfold_term_definition : int -> t -> Lambda.term
  (** [unfold_type_definition id s] returns the actual term corresponding to [Lambda.DConst id] if it exists *)
  val get_constant_of_name : t -> string -> int
  (** [get_constant_of_name s name] returns the id of the constant with name [name] in the signature [s]
if it exists *)

end

(** This module type describes the interface with ACGs *)
module type Acg_sig =
sig

  type t
  (** the abstract type of ACGs *)

  module Signature : Signature_sig

  val empty : t
  val interpret_type : t -> Lambda.stype -> Lambda.stype
  (** [interpret_type t g] returns the type L([t]) as defined in the ACG [g] *)
  val get_abstract : t -> Signature.t
  (** [get_abstract g] returns the abstract signature of [g] *)
  val get_object : t -> Signature.t
  (** [get_object g] returns the object signature of [g] *)
  val interpret_constant : t -> int -> Lambda.term
  (** [interpret_constant g id] returns the lambda-term L([c]) *)

end

(** This module type describes the interface with Datalog signatures *)
module type Datalog_signature_sig =
sig

  type predicate
  (** the abstract type of predicates in datalog signatures *)
  type signature
  (** the abstract type of datalog signatures *)

  val empty : signature
  (** [empty] is an empty signature *)
  val add_pred : int -> string -> signature -> signature
  (** [add_pred n name s] adds the predicate with name [name] and arity [n] to the signature [s] *)
  val get_preds : signature -> predicate list
  (** [get_preds s] returns the list of predicates in signature [s] *)
  val make_pred : int -> predicate
  (** [make_pred n] builds a predicate with identifier [n] *)

end

(** This module type describes the interface with Datalog programs *)
module type Program_sig =
sig

  type predicate
  (** The abstract type of predicates in Datalog programs *)
  type clause
  (** The abstract type of clauses in Datalog programs *)
  type program
  (** The abstract type of Datalog programs *)

  module Signature : Datalog_signature_sig

  val make_pred : Signature.predicate -> int list -> predicate
  (** [make_pred p l] builds a Datalog predicate based on the Datalog signature predicate [p] (its
identifier in the signature) and the list of its variables [l] *)
  val make_clause : predicate -> predicate list -> clause
  (** [make_clause p l] builds a Datalog clause based on the Datalog predicate [p] (its lhs) and
the list of Datalog predicates [l] (its rhs) *)
  val make_program : Signature.signature -> clause list -> program
  (** [make_program s l] builds a Datalog program based on the Datalog signature [s] and
the list of its clauses [l] *)

end

(** This module type describes the interface with the Datalog solver *)
module type Datalog_solver_sig =
sig

  type item
  (** The abstract type of items *)
  type memory
  (** The abstract type of memory *)

  module Program : Program_sig

  val make_item : int -> int list -> item
  (** [make_item n l] builds an item *)
  val solve : Program.program -> item list -> memory
  (** [solve p l] returns a memory that contains all the clauses that can be infered
from the program [p] and the list of items [l] (?) *)

end

(** This is the functor that provides the actual reduction *)
module Make (Acg1 : Acg_sig) (Program1 : Program_sig) (Solver1 : Datalog_solver_sig) :
sig

  module Acg : Acg_sig
  module Program : Program_sig
  module Solver : Datalog_solver_sig

  val database_query : Lambda.term -> int -> Solver.item list * Solver.item
  val program : Acg.t -> Program.program
  val reduction : Acg.t -> Lambda.term -> Lambda.stype -> Program.program * Solver.item list * Solver.item

end
