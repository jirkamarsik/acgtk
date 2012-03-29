open Lambda
open Type_inference

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
  (** [add_pred n name s] adds the predicate with name [name] and
      arity [n] to the signature [s] *)
  val make_pred : int -> predicate
  (** [make_pred n] builds a predicate with identifier [n] *)
  val find_pred_of_name : string -> signature -> int*int

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

  module Signature1 : Datalog_signature_sig

  val make_pred : Signature1.predicate -> int list -> predicate
  (** [make_pred p l] builds a Datalog predicate based on the Datalog
      signature predicate [p] (its identifier in the signature) and the list
      of its variables [l] *)
  val make_clause : predicate -> predicate list -> clause
  (** [make_clause p l] builds a Datalog clause based on the Datalog
      predicate [p] (its lhs) and the list of Datalog predicates [l]
      (its rhs) *)
  val make_program : Signature1.signature -> clause list -> program
  (** [make_program s l] builds a Datalog program based on the Datalog
      signature [s] and the list of its clauses [l] *)
  val get_signature : program -> Signature1.signature

end

(** This module type describes the interface with the Datalog
    solver *)
module type Datalog_solver_sig =
sig

  type item
  (** The abstract type of items *)
  type memory
  (** The abstract type of memory *)

  module Program1 : Program_sig

  val make_item : int -> int list -> item
(** [make_item n l] builds an item *)
(*val solve : Program1.program -> item list -> memory (** [solve p l]
  returns a memory that contains all the clauses that can be infered
  from the program [p] and the list of items [l] (?) *)*)

end

(** This is the functor that provides the actual reduction *)
module Make (Lexicon1 : Interface.Lexicon_sig  with type Signature.stype=Lambda.stype and type Signature.term=Lambda.term) (Solver1 : Datalog_solver_sig) :
sig

  module Lexicon : Interface.Lexicon_sig with type t = Lexicon1.t
  module Program1 : Program_sig with type program = Solver1.Program1.program
  module Solver : Datalog_solver_sig with type item = Solver1.item

  (*val break_aux : int -> (int * int) list -> Lambda.stype -> int
    list * (int * int) list * int val break1 : Lambda.stype -> int ->
    int list list val break2 : Lambda.stype * Lambda.stype list -> int
    -> int list list * int list list val break3 : Lambda.stype *
    Lambda.stype list -> int list * int list list*)
  val database_query : Lexicon.Signature.t -> Program1.Signature1.signature -> Lambda.term -> Lambda.stype -> Solver.item list * Solver.item
  val program : Lexicon.t -> Program1.program
  val reduction : Lexicon.t -> Lambda.term -> Lambda.stype -> Program1.program * Solver.item list * Solver.item

end
