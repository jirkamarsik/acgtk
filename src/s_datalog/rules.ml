module type CorrespondanceTableTYPE=
sig
  type identifier
  type table
  exception Not_found
  val empty:table
  val find_id_of_sym : string -> table -> identifier
  val find_sym_from_id : identifier -> table -> string
  val add_sym : string -> table -> identifier*table
end

module type IdGen_TYPE =
sig
  type id
  type t
  val init : unit -> t
  val get_fresh_id : t -> (id*t)
  val eq : id -> id -> bool
  val compare : id -> id -> int
    
  module IdMap : Map.S with type key=id

  module Corr : CorrespondanceTableTYPE with type identifier=id

end

module type IdType=
sig
  type t
  val compare : t -> t -> int
  val succ: t -> t
  val start: t
end



module IdGen(ID:IdType) =
struct
  type id=ID.t
  type t=Generator of id
  let init () = Generator ID.start
  let get_fresh_id (Generator n) = n, Generator (ID.succ n)
  let eq i j = ID.compare i j=0
  let compare = ID.compare
  module IdMap=Map.Make(ID)
  module Corr=
    struct
      type identifier=id
      type table= {symbols:id Tries.Tries.t;
	       ids:string IdMap.t;
	       gen: t}
      exception Not_found

      let empty = {symbols=Tries.Tries.empty;
		   ids=IdMap.empty;
		   gen=init ()}
	
      let find_id_of_sym symbol {symbols=table} = 
	try
	  Tries.Tries.find symbol table
	with
	| Tries.Tries.Not_found -> raise Not_found
	  
      let find_sym_from_id id {ids=table} = 
	try
	  IdMap.find id table
	with
	| Not_found -> raise Not_found
	  
      let add_sym sym ({symbols=syms;ids=ids;gen=vargen} as table) = 
	try
	  Tries.Tries.find sym syms,table
	with
	| Tries.Tries.Not_found -> 
	  let new_var,new_vargen=get_fresh_id vargen in
	  new_var,{symbols=Tries.Tries.add sym new_var syms;
		   ids=IdMap.add new_var sym ids;
		   gen=new_vargen}
	    
    end
end

module Var=
struct
  type t=Var of int
  let compare (Var i) (Var j)=i-j
  let succ (Var i)=Var (i+1)
  let start=Var 0
end
  
module VarGen = IdGen(Var)
  
module Const=
struct
  type t=Const of int
  let compare (Const i) (Const j)=i-j
  let start=Const 0
  let succ (Const i)=Const (i+1)
end
module ConstGen=IdGen(Const)


module type RulesAbstractSyntac_TYPE =
sig
  type _ content = 
  | Var : VarGen.id -> VarGen.id content
  | Const : ConstGen.id -> ConstGen.id content

  type predicate_id_type =string
  type 'a predicate={p_id:predicate_id_type;
		     arity:int;
		     components:'a content list 
		    (** It is assumed that the size of the list is the
			arity *)
		    }      
  type 'a rule={id:int;
		lhs:'a predicate;
		e_rhs:'a predicate list;
		(** represents the extensionnal predicates of the rule *)
		i_rhs:'a predicate list; 
	       (** represents the intensionnal predicates of the rule *)
	       }
  type  fact = ConstGen.id predicate
  type  ground_clause = ConstGen.id rule
    

  val fact_compare : fact -> fact -> int
  val compare : predicate_id_type -> predicate_id_type -> int


end

(** These modules are the abstract syntactic representations of
    predicates and rules *)
module Pred_and_Rules =
struct
  type _ content = 
  | Var : VarGen.id -> VarGen.id content
  | Const : ConstGen.id -> ConstGen.id content
  type predicate_id_type=string
  type 'a predicate={p_id:predicate_id_type;
		     arity:int;
		     components:'a content list
		    (** It is assumed that the size of the list is the
			arity *)
		    }      
  type 'a rule={id:int;
		lhs:'a predicate;
		e_rhs:'a predicate list;
		i_rhs:'a predicate list;
	       }

  type  fact = ConstGen.id predicate
  type  ground_clause = ConstGen.id rule
  let compare id1 id2 = String.compare id1 id2

  let rec content_compare l1 l2 =
    match l1,l2 with
    | [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (Const a1)::tl1,(Const a2)::tl2 ->
      let res = ConstGen.compare a1 a2 in
      if ConstGen.compare a1 a2 <> 0 then
	res
      else
	content_compare tl1 tl2

  let fact_compare ({p_id=id1;arity=a1;components=l1}:fact) ({p_id=id2;arity=a2;components=l2}:fact) =
    let res = compare id1 id2 in
    if res<>0 then
      res
    else
      let res = a1-a2 in
      if res<>0 then
	res
      else
	content_compare l1 l2

end

