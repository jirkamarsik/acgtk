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
  (** [add_pred n name s] adds the predicate with name [name] and arity [n] to the signature [s] *)
  val make_pred : int -> predicate
  (** [make_pred n] builds a predicate with identifier [n] *)
  val find_pred_of_name : string -> signature -> int*int
  (** [find_pred_of_name name s] renvoie [(ar,id)] où [ar] est l'aritéet [id] l'identifiant du
prédicat [name] dans la signature [s] *)

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
  (** [make_pred p l] builds a Datalog predicate based on the Datalog signature predicate [p] (its
identifier in the signature) and the list of its variables [l] *)
  val make_clause : predicate -> predicate list -> clause
  (** [make_clause p l] builds a Datalog clause based on the Datalog predicate [p] (its lhs) and
the list of Datalog predicates [l] (its rhs) *)
  val make_program : Signature1.signature -> clause list -> program
  (** [make_program s l] builds a Datalog program based on the Datalog signature [s] and
the list of its clauses [l] *)
  val get_signature : program -> Signature1.signature
  (** [get_signature p] renvoie la signature sur laquelle le programme [p] est défini *)

end

(** This module type describes the interface with the Datalog solver *)
module type Datalog_solver_sig =
sig

  type item
  (** The abstract type of items *)
  type memory
  (** The abstract type of memory *)

  module Program1 : Program_sig

  val make_item : int -> int list -> item
  (** [make_item n l] builds an item *)
  (*val solve : Program1.program -> item list -> memory
  (** [solve p l] returns a memory that contains all the clauses that can be infered
from the program [p] and the list of items [l] (?) *)*)

end

module Make (Lexicon1:Interface.Lexicon_sig with type Signature.stype=Lambda.stype  and type Signature.term=Lambda.term) (Solver1:Datalog_solver_sig) =
struct

  open Lambda
  open TypeInference

  module Lexicon = Lexicon1
  module Solver = Solver1
  module Signature = Lexicon1.Signature
  module Program1 = Solver.Program1
  module Datalog_signature = Program1.Signature1

  exception NonLinear
  exception NotImplemented
  exception NotSecondOrder

  (* renvoie la liste des constantes d'un lambda-terme lu de gauche à droite *)
  let rec lambda_get_constants = function
    | LVar _ -> []
    | Const x -> [x]
    | LAbs(_,m) -> lambda_get_constants m
    | App(m,n) -> (lambda_get_constants m)@(lambda_get_constants n)
    | (Var _ | Abs(_,_)) -> raise NonLinear
    | _ -> raise NotImplemented

  (* renvoie la liste des types atomiques (et d'un identifiant) présent dans une signature *)
  let sg_get_atomic_types s =
    Signature.fold
      (fun e l -> match Signature.is_declared e s with
	| None -> l
	| Some name -> if Signature.is_type name s
	  then (match l with
	    | [] -> [(name,0)]
	    | (t,n)::q -> (name,n+1)::(t,n)::q)
	  else l)
      []
      s

  (* renvoie la liste des constantes (et d'un dientifiant) présentes dans une signature *)
  let sg_get_constants s =
    Signature.fold
      (fun e l -> match Signature.is_declared e s with
	| None -> l
	| Some name -> if fst (Signature.is_constant name s)
	  then (match l with
	    | [] -> [(name,0)]
	    | (t,n)::q -> (name,n+1)::(t,n)::q)
	  else l)
      []
      s

  (* renvoie la liste des constantes présentes dans [s] et leur type *)
  let sg_get_constants_and_constant_types s =
    Signature.fold
      (fun e l -> match Signature.is_declared e s with
	| None -> l
	| Some name -> if fst (Signature.is_constant name s)
	  then (Signature.find_term name s)::l else l)
      []
      s

  (* renvoie le terme où les définitions des constantes définies ont été dépliées suivant
     leur définition dans la signature *)
  let rec sg_unfold_term s = function
    | LVar x -> LVar x
    | Const x -> Const x
    | App(m,n) -> App(sg_unfold_term s m,sg_unfold_term s n)
    | LAbs(x,m) -> LAbs(x,sg_unfold_term s m)
    | DConst x -> Signature.unfold_term_definition x s
    | (Var _ | Abs(_,_)) -> raise NonLinear
    | _ -> raise NotImplemented

  (* "casse" un type en la liste de ses types atomiques :
     entrée :
     i : identifiant 'fresh' pour représenter un type atomique
     l : liste associative (type atomique, identifiant)
     x : type
     sortie :
     r : liste des identifiants des types atomiques de [x] lu de gauche à droite
     l : liste associative (type atomique, identifiant)
     i : identifiant 'fresh' pour représenter un type atomique
     exemple :
     break 0 [] ((a -> b) -> ((a -> b) -> c) -> c -> c)
     ---> [0;1;0;1;2;2;2],[(c,2);(b,1);(a,0)],3 *)
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
      | DAtom _ -> failwith "Error in : Reduction_functor.Make.break_aux(type not unfolded)"
      | _ -> raise NotImplemented in
    aux i l x 

  (* "casse" un type en la liste de ses [n] sous-types 'grossiers', eux-mêmes "cassés" avec
     break_aux :
     entrée :
     i : identifiant 'fresh' pour représenter un type atomique
     l : liste associative (type atomique, identifiant)
     n : le nombre de sous-types en lesquels [x] doit être "cassé"
     x : type
     sortie :
     r : liste des
     l : liste associative (type atomique, identifiant)
     i : identifiant 'fresh' pour représenter un type atomique
     exemple :
     break 0 [] 3 ((a -> b) -> ((a -> b) -> c) -> c -> c)
     ---> [[0;1];[0;1;2];[2;2]],[(c,2);(b,1);(a,0)],3 *)
  let break i l n x =
    let rec aux i l n = function
      | _ as t when n = 1 -> let (r,l',i') = break_aux i l t in
			     [r],l',i'
      | Atom _ -> failwith (Printf.sprintf "Error in : Reduction_functor.Make.break\n\texpected 1, got %d" n)
      | LFun(t1,t2) when n > 2 -> let (r,l',i') = break_aux i l t1 in
                                  let (r',l'',i'') = aux i' l' (n-1) t2 in
				  r::r',l'',i''
      | LFun(t1,t2) when n = 2 -> let (r,l',i') = break_aux i l t1 in
				  let (r',l'',i'') = break_aux i' l' t2 in
				  r::[r'],l'',i''
      | LFun(_,_) -> failwith (Printf.sprintf "Error in : Reduction_functor.Make.break\n\texpected >1, got %d" n)
      | Fun(_,_) -> raise NonLinear
      | DAtom _ -> failwith "Error in : Reduction_functor.Make.break (type not unfolded)"
      | _ -> raise NotImplemented in
    aux i l n x

  (* "casse" un type en [n] sous-types 'grossiers' *)
  let break1 x n =
    let (r,l,i) = break 0 [] n x in
    r

  (* "casse" un type en [n] sous-types 'grossiers' et "casse" une liste de types
     en leurs types atomiques :
     exemple :
     break2 ((a -> b) -> a -> b, [(a -> b) -> c; c]) 2
     ---> ([[0;1];[0;1]], [[0;1;2];[2]]) *)
  let break2 x n =
    let rec aux alpha = function
      | [] -> let (r,l,i) = break 0 [] n alpha in
              (r,[]),l,i
      | t::q -> let ((ra,rq),l,i) = aux alpha q in
		let (r,l',i') = break_aux i l t in
		(ra,r::rq),l',i' in
    match x with
      | (a,b) -> let (r,_,_) = aux a b in
		 r

  (* "casse" un type et une liste de types en leurs types atomiques :
     exemple :
     break3 ((a -> b) -> a -> b, [(a -> b) -> c; c])
     ---> ([0;1;0;1], [[0;1;2];[2]]) *)
  let break3 x =
    let rec aux alpha = function
      | [] -> let (r,l,i) = break_aux 0 [] alpha in
	      (r,[]),l,i
      | t::q -> let ((ra,r),l,i) = aux alpha q in
		let (r',l',i') = break_aux i l t in
		(ra,r'::r),l',i' in
    match x with
      | (a,b) -> let (r,_,_) = aux a b in
		 r

  (* singularise le dernier élément d'une liste *)
  let rec reorder = function
    | [] -> failwith "Error in : Reduction_functor.Make.reorder"
    | [a] -> (a,[])
    | t::q -> let (a,l) = reorder q in
              (a,t::l)

  (* singularise le dernier type atomique d'un type lu de gauche à droite *)
  let rec reorder2 = function
    | Atom x -> x,[]
    | LFun(Atom x,t) -> let (r,l) = reorder2 t in
			r,x::l
    | LFun(_,_) -> raise NotSecondOrder
    | Fun(_,_) -> raise NonLinear
    | DAtom _ -> failwith "Error in : Reduction_functor.Make.reorder2\n\ttype not unfolded"
    | _ -> raise NotImplemented

  (* retourne le nombre de type atomiques dans un type en déroulant ses définitions *)
  let rec arity s = function
    | Atom _ -> 1
    | DAtom x -> arity s (Signature.unfold_type_definition x s)
    | LFun(t1,t2) -> (arity s t1) + (arity s t2)
    | Fun(_,_) -> raise NonLinear
    | _ -> raise NotImplemented

  (* retourne la signature sur laquelle est construit le programme Datalog qui correspond
     à la réduction d'une ACG du second ordre, elle est constituée :
     - des prédicats "t.[t]" où [t] est un type atomique abstrait, leur arité est le nombre de
     types atomiques dans l'interprétation de [t] dans le lexique
     - des prédicats "c.[c]" où [c] est unue constante objet, leur arité est le nombre de types
     atomiques dans l'interprétation de [c] dans la signature objet *)
  let make_signature g =
    let rec add_pred_list s = function
      | [] -> s
      | (n,p)::q -> Datalog_signature.add_pred n p (add_pred_list s q) in
    let signature = Datalog_signature.empty in
    let (abs,obj) = Lexicon.get_sig g in
    let f1 (n,t) =
      arity obj (Lexicon.interpret_type (Atom t) g),
      Printf.sprintf "t.%s" n in
    let f2 c =
      arity obj ((fun s c -> Signature.type_of_constant c s) obj c),
      Printf.sprintf "c.%s" c in
    let constants = fst (List.split (sg_get_constants obj)) in
    let types = sg_get_atomic_types abs in
    (*let _ = List.iter (function (x,n) -> Printf.printf "(%s,%d)\n%!" x n) types in
      let _ = List.iter (function x -> Printf.printf "%s\n%!" x) constants in*)
    let n = List.length types in
    let signature' = add_pred_list signature (List.map f1 types) in
    let signature'' = add_pred_list signature' (List.map f2 constants) in
    signature'',n

  (* retourne la liste des clauses qui constituent le programme Datalog
     correspondant à la réduction d'une ACG du second ordre *)
  let make_clauses signature g =
    let mapcombine f a b = List.map f (List.combine a b) in
    let f1 = function
      | (a,p) -> Program1.make_pred (Datalog_signature.make_pred p) a in
    let f2 = function
      | (b,d) -> Program1.make_pred (Datalog_signature.make_pred d) b in
    let (abs,obj) = Lexicon.get_sig g in
    let rec aux = function
      | [] -> []
      | (c,t)::q -> let l_c = Lexicon.interpret_term c g in
		    let const = List.map (function x -> snd (Datalog_signature.find_pred_of_name ("c."^(Signature.term_to_string (Const x) obj)) signature)) (lambda_get_constants l_c) in
		    let m = Signature.eta_long_form l_c (Lexicon.interpret_type t g) obj in
                    let (a,b) = break2 (type_inference m) (arity abs t) in
                    let (a0,a') = reorder a and (p0,p') = reorder2 t in
		    (Program1.make_clause (Program1.make_pred (Datalog_signature.make_pred p0) a0) 
                       ((mapcombine f1 a' p')@(mapcombine f2 b const))
                    )::(aux q) in
    let l = sg_get_constants_and_constant_types abs in
    aux l

  (* retourne le programme correspondant à la réduction d'un ACG du second ordre *)
  let program g =
    let (signature,n) = make_signature g in
    Program1.make_program (signature) (make_clauses signature g)

  (* retourne la base de données et la requète correspondant à un terme objet et
     un type atomique abstrait (le terme est supposé beta-normal, eta-long par rapport
     à l'interprétation du type) *)
  let database_query obj signature m = function
    | Lambda.Atom t ->
      let make_item = function
	| (a,b) -> Solver.make_item a b in
      let (alpha,beta) = break3 (type_inference m) in
      let c = List.map (function x -> snd (Datalog_signature.find_pred_of_name ("c."^(Signature.term_to_string (Const x) obj)) signature)) (lambda_get_constants m) in
      List.map make_item (List.combine c beta),
      Solver.make_item t alpha
    | _ -> raise NotImplemented

  (* retourne le programme, la base de données et la requète correspondant à une
     ACG, un terme objet et un type atomique abstrait *)
  let reduction g m t =
    let (_,obj) = Lexicon.get_sig g in
    let n = Signature.eta_long_form (sg_unfold_term obj m) (Lexicon.interpret_type t g) obj in
    let p = program g in
    let (d,q) = database_query obj (Program1.get_signature p) n t in
    (p,d,q)
      
end
