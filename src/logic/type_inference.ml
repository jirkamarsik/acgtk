open Lambda
open Abstract_syntax

module type Signature_sig =
sig

  type t

  (** [unfold_term_definition id s] returns the term corresponding to the defined constant with id [id] if it exists *)
  val unfold_term_definition : int -> t -> Lambda.term

end

module TypeInference =
struct

  open Lambda

  exception NotImplemented
  exception NotUnifiable
  exception NonLinear
  exception NotWellTyped

  type subst = S of (int * stype) list

  let rec substitute x = function
    | S [] -> Atom x
    | S ((v,t)::q) -> if v = x then t else substitute x (S q)

  let rec lift_subst sigma = function
    | Atom v -> substitute v sigma
    | LFun(t1,t2) -> LFun(lift_subst sigma t1,lift_subst sigma t2)
    | _ -> raise NotImplemented

  let rec occurs x = function
    | Atom v -> v = x
    | LFun(t1,t2) -> (occurs x t1) || (occurs x t2)
    | _ -> raise NotImplemented

  let unify c =
    let rec aux sigma = function
      | (s,t)::q when s=t -> aux sigma q
      | (Atom s,t)::q when not (occurs s t) -> let (q1,q2) = List.split q and replace_s_by_t = List.map (lift_subst (S [(s,t)])) in
                                               aux ((s,t)::sigma) (List.combine (replace_s_by_t q1) (replace_s_by_t q2))
      | (s,Atom t)::q -> aux sigma ((Atom t,s)::q)
      | (LFun(t1,t2),LFun(s1,s2))::q -> aux sigma ((t1,s1)::(t2,s2)::q)
      | [] -> sigma
      (*| ((DAtom _ | Fun(_,_) | Dprod(_,_,_) | Record(_,_) | Variant(_,_) | TAbs(_,_) | TApp(_,_)),_)::q -> raise NotImplemented*)
      | _ -> raise NotUnifiable in
    S(aux [] c)

  let rename_vars m =
    let rec aux i j c l = function
      | LVar x -> let k = List.nth l x in
                  LVar (j+1),i,j+1,c,[(k,j+1)]
      | App(n,p) -> let (n',i',j',c',r) = aux i j c l n in
                    let (p',i'',j'',c'',r') = aux i' j' c' l p in
                    App(n',p'),i'',j'',c'',r@r'
      | LAbs(x,n) -> let (n',i',j',c',r) = aux (i+1) j c (i::l) n in
                     LAbs(x,n'),i',j',c',r
      | Const x -> Const (c+1),i,j,c+1,[]
      | (Abs(_,_) | Var _) -> raise NonLinear
      | _ -> raise NotImplemented in
    let (n,_,_,c,r) = aux 0 0 0 [] m in
    (n,r,c)

  let rec nb_vars = function
    | LVar _ -> 1
    | App(t1,t2) -> (nb_vars t1) + (nb_vars t2)
    | LAbs(_,t) -> nb_vars t
    | Const _ -> 0
    | DConst _ -> failwith "Error in : Type_inference.Type_inference.nb_vars (type not unfolded)"
    | (Var _ | Abs(_,_)) -> raise NonLinear
    | _ -> raise NotImplemented

  let fixed_point f d =
    let rec aux a b =
      if a=b then a else aux b (f b) in
    aux d (f d)

  let type_inference m =
    let (n,r,c) = rename_vars m in
    let v = (nb_vars n) + 1 in
    let fresh_type = (+) v in
    let rec aux e i j = function
      | [] -> e
      | (LVar x, tau)::q -> aux ((Atom x,tau)::e) i j q
      | (App(m,p), tau)::q -> let t = fresh_type i in
                              aux e (i+1) j ((m,LFun(Atom t,tau))::(p,Atom t)::q)
      | (LAbs(x,m), tau)::q -> (try
				  let t = fresh_type i in
				  aux ((tau,LFun(Atom (List.assoc j r),Atom t))::e) (i+1) (j+1) ((m,Atom t)::q)
	with
	    Not_found ->
	      let t = fresh_type i in
	      aux((tau,LFun(Atom (t+1),Atom t))::e) (i+2) (j+1) ((m,Atom t)::q))
      | (Const x, tau)::q -> aux ((Atom (-x),tau)::e) i j q
      | (DConst _,_)::_ -> failwith "Error in : Type_inference.Type_inference.type_inference (type not unfolded)"
      | (Var _,_)::_ -> raise NonLinear
      | (Abs(_,_),_)::_ -> raise NonLinear
      | _ -> raise NotImplemented in
    try
      let f = fixed_point (lift_subst (unify (aux [] 1 0 [(n, Atom 0)]))) in
      let rec const = function
        | 0 -> []
        | i -> (Atom (-i))::(const (i-1)) in
      f (Atom 0),List.rev_map f (const c)
    with
        NotUnifiable ->
          raise NotWellTyped

  let rec print_subst = function
    | S [] -> ()
    | S((i,ty)::q) -> (Printf.printf "%d->%s\n" i (type_to_string ty (function x -> Abstract_syntax.Default,Printf.sprintf "%d" x));
                       print_subst (S q))

end
