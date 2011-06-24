(** We assume that for every c, L(c) is eta-long wrt L(tau(c)) *)

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

module Make (Acg1:Acg_sig) (Program1:Program_sig) (Solver1:Datalog_solver_sig) =
struct

  open Lambda
  open TypeInference

  module Acg = Acg1
  module Program = Program1
  module Solver = Solver1
  module Signature = Acg.Signature
  module Datalog_signature = Program.Signature

  exception NonLinear
  exception NotImplemented

  let acg = Acg.empty
  let abs = Acg.get_abstract Acg.empty
  let obj = Acg.get_object Acg.empty

  let rec unfold_term s = function
    | LVar x -> LVar x
    | Const x -> Const x
    | App(m,n) -> App(unfold_term s m,unfold_term s n)
    | LAbs(x,m) -> LAbs(x,unfold_term s m)
    | DConst x -> Signature.unfold_term_definition x s
    | (Var _ | Abs(_,_)) -> raise NonLinear
    | _ -> raise NotImplemented

  let rec unfold_type s = function
    | Atom x -> Atom x
    | LFun(t1,t2) -> LFun(unfold_type s t1,unfold_type s t2)
    | DAtom x -> Signature.unfold_type_definition x s
    | Fun(_,_) -> raise NonLinear
    | _ -> raise NotImplemented

  let rec get_constants = function
    | LVar _ -> []
    | Const x -> [x]
    | LAbs(_,m) -> get_constants m
    | App(m,n) -> (get_constants m)@(get_constants n)
    | (Var _ | Abs(_,_)) -> raise NonLinear
    | _ -> raise NotImplemented

  let break_aux i l x =
    let rec aux i l = function
      | Atom x ->
	(try
	   [List.assoc x l],l,i
	 with
	     Not_found ->
	       [i],(x,i)::l,(i+1))
      | LFun(t1,t2) -> let (r,l',i') = aux i l t1 in
		       let (r',l'',i'') = aux i' l' t2 in
		       r@r',l'',i''
      | Fun(_,_) -> raise NonLinear
      | DAtom _ -> failwith "Type not unfolded"
      | _ -> raise NotImplemented in
    aux i l x

  let break i l x =
    let rec aux i l = function
      | Atom x -> let (r,l',i') = break_aux i l (Atom x) in
		  [r],l',i'
      | LFun(t1,t2) -> let (r,l',i') = break_aux i l t1 in
		       let (r',l'',i'') = aux i' l' t2 in
		       r::r',l'',i''
      | Fun(_,_) -> raise NonLinear
      | DAtom _ -> failwith "Type not unfolded"
      | _ -> raise NotImplemented in
    aux i l x

  let break1 x =
    let (r,l,i) = break 0 [] x in
    r

  let break2 x =
    let rec aux alpha = function
      | [] -> let (r,l,i) = break 0 [] alpha in
              (r,[]),l,i
      | t::q -> let ((ra,rq),l,i) = aux alpha q in
                let (r,l',i') = break i l t in
                (ra,r::rq),l',i' in
    match x with
      | (a,b) -> let (r,_,_) = aux a b in
		 r

  let rec reorder = function
    | [] -> failwith "Liste vide"
    | [a] -> (a,[])
    | t::q -> let (a,l) = reorder q in
              (a,t::l)

  let make_signature g =
    let rec add_pred_list s = function
      | [] -> s
      | (n,p)::q -> Datalog_signature.add_pred n p (add_pred_list s q) in
    let signature = Datalog_signature.empty in
    let abs = Acg.get_abstract g and obj = Acg.get_object g in
    let rec arity = function
      | Atom _ -> 1
      | LFun(t1,t2) -> (arity t1) + (arity t2)
      | Fun(_,_) -> raise NonLinear
      | _ -> raise NotImplemented in
    let f1 t =
      arity (Acg.interpret_type g (Atom t)),
      Printf.sprintf "t.%s" (type_to_string (Atom t) (function x -> Abstract_syntax.Abstract_syntax.Default,Printf.sprintf "Const %d" x)) in
    let f2 n c = arity (Signature.interpret_constant obj c),Printf.sprintf "c.%d" (n+c+1) in
    let constants = List.map (Signature.get_constant_of_name obj) (List.map fst (Signature.get_constants obj))
    and types = Signature.get_atomic_types abs in
    let n = (List.fold_left max min_int constants) - (List.fold_left min max_int types) in
    let signature' = add_pred_list signature (List.map f1 types) in
    add_pred_list signature' (List.map (f2 n) constants),n

  let make_clauses n g =
    let mapcombine f a b = List.map f (List.combine a b) in
    let f1 = function
      | (a,p) -> Program.make_pred (Datalog_signature.make_pred p) a in
    let f2 = function
      | (b,d) -> Program.make_pred (Datalog_signature.make_pred (d+n)) b in
    let abs = Acg.get_abstract g in
    let rec aux = function
      | [] -> []
      | (c,t)::q -> let id = Signature.get_constant_of_name abs c in
                    let l_c = Acg.interpret_constant g id in
                    let m = eta_long_form (normalize (unfold_term (Acg.get_object g) l_c))
                      (Acg.interpret_type g t) (Signature.interpret_constant (Acg.get_object g)) in
                    let (a,b) = break2 (type_inference m) in
                    let (a0,a') = reorder a and (p0,p') = reorder (break1 t) in
                    (Program.make_clause (Program.make_pred (Datalog_signature.make_pred (List.hd p0)) a0) 
                       ((mapcombine f1 a' (List.hd p'))@(mapcombine f2 (List.hd b) (get_constants l_c)))
		    )::(aux q) in
  aux (Signature.get_constants abs)

  let program g =
    let (signature,n) = make_signature g in
    Program.make_program (signature) (make_clauses n g)

  let database_query m t =
    let make_item = function
      | (a,b) -> Solver.make_item a b in
    let (alpha,beta) = break2 (type_inference m) and c = get_constants m in
    List.map make_item (List.combine c (List.hd beta)),Solver.make_item t (List.hd alpha)

  let reduction g m = function
    | Atom x -> let n = eta_long_form (normalize (unfold_term obj m)) (unfold_type abs (Atom x))  in
                let p = program g and (d,q) = database_query m x in
		(p,d,q)
    | _ -> raise NotImplemented

end
