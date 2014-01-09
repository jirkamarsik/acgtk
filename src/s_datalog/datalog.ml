open PersistentArray
open Focused_list
open Datalog_AbstractSyntax


module ASPred=AbstractSyntax.Predicate 
module ASRule=AbstractSyntax.Rule
module ASProg=AbstractSyntax.Program

module type Datalog_Sig=
sig
  exception Fails
  module UF:UnionFind.S
    
  module Predicate :
  sig
    type predicate = { p_id : ASPred.pred_id; arity : int; }
    val make_predicate : Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate -> predicate
    module PredMap : Map.S with type key = ASPred.pred_id
    module FactSet :Set.S with type elt = ASPred.predicate
    val conditionnal_add :
      FactSet.elt -> FactSet.t -> FactSet.t -> FactSet.t -> FactSet.t
    val facts_to_string : FactSet.t PredMap.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
    module PredicateMap : Map.S with type key = ASPred.predicate
    module Premise :
    sig
      type t = ASPred.predicate list
      val to_string : t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> string
    end
    module PremiseSet : Set.S with type elt = Premise.t
    val add_map_to_premises_to_buffer : Buffer.t -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit
    val format_derivations2 : ?query:Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> PremiseSet.t PredicateMap.t -> unit
      

    val add_pred_arguments_to_content :
      ASPred.term list ->
      Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
        int Datalog_AbstractSyntax.VarGen.IdMap.t ->
      Datalog_AbstractSyntax.ConstGen.id UF.content list * int *
        int Datalog_AbstractSyntax.VarGen.IdMap.t
	
  end
    
  module Rule :
  sig
    type rule = {
      id : int;
      lhs : Predicate.predicate;
      e_rhs : (Predicate.predicate*int) list;
      i_rhs : (Predicate.predicate*int) list;
      content : Datalog_AbstractSyntax.ConstGen.id UF.t;
    }
    val make_rule : ASRule.rule -> rule
    val cyclic_unify : int -> int -> 'a UF.t -> 'a UF.t
    val extract_consequence :
      rule -> Datalog_AbstractSyntax.ConstGen.id UF.t -> ASPred.predicate
    module FactArray :
    sig
      type row = Predicate.FactSet.t
      type array = row list
      val collect_results :
        ('a -> (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> 'a) ->
        'a ->
        (int * Datalog_AbstractSyntax.ConstGen.id UF.t) * Predicate.FactSet.elt list -> array -> 'a
    end
    val immediate_consequence_of_rule :
      rule -> FactArray.row Predicate.PredMap.t -> ASPred.predicate list
      
    module Rules:Set.S with type elt=rule
  end
    
  module Program :
  sig
    type program = {
      rules : Rule.rule list Predicate.PredMap.t;
      edb : ASPred.pred_id list;
      edb_facts:Predicate.FactSet.t Predicate.PredMap.t;
      idb : ASPred.pred_id list;
      pred_table: ASPred.PredIdTable.table;
      const_table: Datalog_AbstractSyntax.ConstGen.Table.table;
      rule_id_gen:IdGenerator.IntIdGen.t;
    }
    val empty : program
    val make_program : ASProg.program -> program
    val temp_facts :
      Rule.rule ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      (ASPred.predicate * Predicate.FactSet.elt list -> 'a -> 'a) -> 'a -> ASPred.PredIdTable.table -> Datalog_AbstractSyntax.ConstGen.Table.table -> 'a
    val p_semantics_for_predicate :
      Predicate.PredMap.key ->
      program ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t ->
      Rule.FactArray.row Predicate.PredMap.t -> Predicate.PremiseSet.t Predicate.PredicateMap.t -> Predicate.FactSet.t * Predicate.PremiseSet.t Predicate.PredicateMap.t 
    val seminaive : program -> Rule.FactArray.row Predicate.PredMap.t * Predicate.PremiseSet.t Predicate.PredicateMap.t
    val to_abstract : program -> ASProg.program
      
    val extend : program -> ASProg.modifier -> program
      
    val add_e_facts : program -> (ASRule.rule list*Datalog_AbstractSyntax.ConstGen.Table.table*IdGenerator.IntIdGen.t) -> program
      
    (** [add_rule r p] adds a [ASRule.rule] to a [Datalog.Program]
	with the assumption that it will not change the {em nature} of
	a predicate (that is making it change from extensional to
	intensional). *)
      
    val add_rule : ASRule.rule -> program -> program
      
      
    val get_fresh_rule_id : program -> (int * program)
    val get_fresh_cst_id : string -> program -> (Datalog_AbstractSyntax.ConstGen.id * program)
    val add_pred_sym : string -> program -> (ASPred.pred_id*program)
  end
end



  
module Make (S:UnionFind.Store) =
struct
  exception Fails
  module UF= UnionFind.Make(S)

  module Predicate =
  struct
    
    (** For the type of the predicates, we use the same identifiers as
	for the predicates of the datalog abstract syntax {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate} *)
    type predicate={p_id:ASPred.pred_id;
		    arity:int;
		   }
      
    (** [make_predicate p] returns an actual predicate from some
	abstract syntax representation {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate} *)
    let make_predicate p = {p_id=p.ASPred.p_id;arity=p.ASPred.arity}
      
    (** [to_abstract p (s,content) (vars,vargen)] returns a triple
	[(abs_p,vars',vargen')] where [abs_p] is the [p] predicate
	translated into an equivalent predicate from the datalog
	abstract syntax. In order to be able to perform this
	translation, we need [s] and index and [content] a indexed
	storage data structure which is meant to contain the arguments
	of [p] starting at index [s]. Then, in case some variable are
	still present, to be able to translate them according to the
	other variables that could be in the content [content], we
	need to check in [vars] if it's index already was associated
	to some [VarGen.id] generated by [vargen]. In this case
	[vars'=vars] and [vargen'=vargen], otherwise [vars'] is [var]
	with a new variable generated by [vargen] associated to the
	variable index, and [vargen'] is the result of generating this
	new variable from [vargen].*)
    let to_abstract {p_id=id;arity=arity} (start,content) (vars,vargen) pred_table =
      LOG "Starting the extraction of predicate %s/%d" (ASPred.to_string {ASPred.p_id=id;ASPred.arity=arity;ASPred.arguments=[]} pred_table ConstGen.Table.empty) arity LEVEL TRACE;
      let get_var i (vars,vargen) = 
	try
	  Utils.IntMap.find i vars,(vars,vargen)
	with
	| Not_found -> 
	  let new_var,new_vargen = VarGen.get_fresh_id vargen in
	  new_var,(Utils.IntMap.add i new_var vars,new_vargen) in
      let new_vars,new_vargen,rev_arguments=
	List.fold_left
	  (fun (vars,vargen,acc) -> function
	  | UF.Value v -> vars,vargen,((ASPred.Const v)::acc)
	  | UF.Link_to i -> let var,(new_vars,new_vargen)=get_var i (vars,vargen) in
			    new_vars,new_vargen,(ASPred.Var var)::acc)
	  (vars,vargen,[])
	  (UF.extract ~start:start arity content) in      
      {ASPred.p_id=id;
       ASPred.arity=arity;
       arguments=List.rev rev_arguments},
      new_vars,
      new_vargen
	
    (** [lst_to_abstract lst (start,content) (vars,vargen)] returns a
	4-uple [(abs_p_lst,start',vars',vargen')] where all the
	predicates of [lst] have been translated and put into
	[abs_p_lst]. The predicates in [lst] are supposed to be
	represented in [content] starting at index [start] in an
	adjacent way. [start'] indexes the component of the next
	predicate in [content], and [vars'] and [vargen'] keep track
	of the variable that can have been generated. *)
    let lst_to_abstract lst (start,content) (vars,vargen) pred_table =
      let next_idx,vars',vargen',abs_preds =
	List.fold_left
	  (fun (s,l_vars,l_vargen,acc) (p,pos) ->
	    let abs_p,new_vars,new_vargen = to_abstract p (s,content) (l_vars,l_vargen) pred_table in
	    s+p.arity,new_vars,new_vargen,(abs_p,pos)::acc)
	  (start,vars,vargen,[])
	  lst in
      (List.rev abs_preds),next_idx,vars',vargen'
	
	
    (** [instantiate_with p (i,c)] instantiates the content [c] with the
	fact [p] starting at [i]. It returns a pair [(i',c')] when [i]
	is the index of the first component of the [p] predicate in the
	content [c] {e THIS IS NOT CHECKED HERE}. [i'=i+a] where [a] is
	the arity of [p] (it means [i'] should index the first component
	of the next predicate in the content of the rule) and [c'] is a
	new content where all the components between [i] and [i'-1] have
	been instantiated with the components of [p]. When such an
	instantiation fails, it raises {! UF.Union_Failure} *)
    let instantiate_with
	{ASPred.p_id=_;ASPred.arity=_;ASPred.arguments=args}
	(idx,content) =
      let last_i,(new_c,_) =
	List.fold_left
	  (fun  (i,(cont,vars)) value ->
	    (i+1,
	     match value with
	     | ASPred.Const v -> UF.instantiate i v cont,vars
	     | ASPred.Var var -> 
	       try
		 UF.union i (VarGen.IdMap.find var vars) cont,vars
	       with
	       | Not_found -> cont,VarGen.IdMap.add var i vars))
	  (idx,(content,VarGen.IdMap.empty))
	  args in
      last_i,new_c
	

    type unifiable_predicate={u_p_id:ASPred.pred_id;
			      u_arity:int;
			      content:ConstGen.id UF.t;
			     }

  (** [add_pred_arguments_to_content arguments (content,idx,mapped_vars)]
      returns a triple (content',idx',mapped_vars') where [content']
      is the list [content] to which has been added {e *in the reverse
      order*} the information from [arguments]. The update is such
      that if the argument of [arguments] is a [Var i] then it is
      replaced by a [Link_to j] such that [j] is the index at which
      the variable [Var i] was met for the first time (it is stored in
      [mapped_vars]. If the argument is a [Const c], then a [Value c]
      is added at the current position. [idx'] is the value of the
      next position if another list of arguments has to be added. And
      [mapped_vars'] is a map from variables [Var i] to positions
      (i.e. [int]) in which these variables first occur in [content']
      - [arguments] is the list of the arguments of some predicate
      - [content] is a list meant to become the content of a rule,
      i.e. an indexed storage data structure that is meant to be
      extended with [arguments]. *BE CAREFUL: IT COMES IN INVERSE
      ORDER*
      - [idx] is the index to be given for the next element of
      [content]
      - [mapped_vars] is a mapping from [P.Var i] variables to the
      index at which they've been stored in [content]. When such a
      variable is met for the first time, as expected in the
      UnionFind data structure, the content at [idx] is [Link_to]'ed
      itself. *)
    let add_pred_arguments_to_content arguments (content,idx,mapped_vars) =
      List.fold_left
	(fun (cont,i,vars) (arg : ASPred.term) -> 
	  match arg with
	  | ASPred.Var v -> 
	    begin
	      try 
		let var_index = VarGen.IdMap.find v vars in
		((UF.Link_to var_index) :: cont,i+1,vars) with
	      | Not_found -> 
		LOG "I met a new variable and put it at index %d" i LEVEL TRACE;
		((UF.Link_to i) :: cont,i+1,VarGen.IdMap.add v i vars)
	    end
	  | ASPred.Const c -> ((UF.Value c) :: cont,i+1,vars))
	(content,idx,mapped_vars)
	arguments

    let make_unifiable_predicate {ASPred.p_id;ASPred.arity;ASPred.arguments} =
      let content_as_lst,_,_ =
	add_pred_arguments_to_content arguments ([],1,VarGen.IdMap.empty) in
      {u_p_id=p_id;u_arity=arity;content=UF.create (List.rev content_as_lst)}

    let unifiable p u_p =
      try
	if p.ASPred.p_id=u_p.u_p_id then
	  let _ = instantiate_with p (1,u_p.content) in
	  true
	else
	  false
      with
      | UF.Union_Failure -> false

	
    (** A map whose key is of type of the predicates identifers *)
    module PredMap=ASPred.PredIdMap

    (** A map whose key is of type [predicate] *)
    (* TODO: Could it be replaced by predicate id only? *)
    module FactSet=Set.Make
      (struct
	type t=ASPred.predicate
	let compare = ASPred.compare 
       end)
      
      
    let add_facts_to_buffer b pred_table cst_table f =
      FactSet.iter (fun elt -> Buffer.add_string b (Printf.sprintf "%s.\n" (ASPred.to_string elt pred_table cst_table))) f 

    let add_map_to_facts_to_buffer b pred_table cst_table map =
      PredMap.iter
	(fun _ v -> add_facts_to_buffer b pred_table cst_table v)
	map


    let facts_to_string facts pred_table cst_table =
      let buff=Buffer.create 100 in
      let () = add_map_to_facts_to_buffer buff pred_table cst_table facts in 
      Buffer.contents buff
      
    (** [conditionnal_add e s1 s2 s3] adds [e] to the set [s1] only if
	[e] doesn't belong to [s2] nor to [s3]*)
    let conditionnal_add e s1 s2 s3=
      if FactSet.mem e s2 then
	s1
      else
	if FactSet.mem e s3 then
	  s1
	else
	  FactSet.add e s1
	    
    (** A map indexed by integers to store facts at step (or time) [i]
	in the seminaive algorithm. These facts are also indexed by
	[predicate_id_type]. *)
    (*    module Indexed_Facts=Utils.IntMap  *)


    module Premise =
    struct
      type t = ASPred.predicate list
      let rec compare pred_lst_1 pred_lst_2 =
	match pred_lst_1,pred_lst_2 with
	| [],[] -> 0
	| _,[] -> 1
	| [],_ -> -1
	| p1::tl1,p2::tl2 ->
	  let diff =ASPred.compare p1 p2 in
	  if diff <> 0 then
	    diff
	  else
	    compare tl1 tl2

      let to_string premises pred_table const_table =
	Utils.string_of_list "," (fun p -> ASPred.to_string p pred_table const_table) premises


    end

    module PremiseSet=Set.Make(Premise)

    module PredicateMap=Map.Make(
      struct 
	type t = ASPred.predicate
	let compare = ASPred.compare
      end)




    let rec format_derivations2 ?query pred_table cst_table map =
      let u_query = 
	match query with
	| Some q -> Some (make_unifiable_predicate  q)
	| None -> None in      
      PredicateMap.iter
	(fun k v -> 
	  match u_query with
	  | Some q when not (unifiable k q) -> ()
	  | _ ->
	    let () = format_derivation "" k v pred_table cst_table map FactSet.empty in 
	    Printf.fprintf stdout "\n")
	map
    and format_derivation prefix k v pred_table cst_table map set=
      if FactSet.mem k set then
	Printf.printf "... (infinite loop on %s)" (ASPred.to_string k pred_table cst_table) 
      else
	let new_set=FactSet.add k set in
	let _ = 
	  PremiseSet.fold
	    (fun premises (first,length)  -> 
	      let new_length,new_prefix= 
		match first with
		| true ->
		  let s=ASPred.to_string k pred_table cst_table in
		  let () = Printf.fprintf stdout "%s" s in
		  let n_l=String.length s in
		  n_l,Printf.sprintf "%s%s" prefix (String.make n_l ' ')
		| false ->  
		  let () = Printf.fprintf stdout "\n%s  %s" prefix (String.make (length -2) '>') in
		  length,Printf.sprintf "%s  %s" prefix (String.make (length-2) ' ') in
	      let () = format_premises2 new_prefix (List.rev premises) true pred_table cst_table map new_set in
	      (*	  let () = Printf.fprintf stdout "\n" in*)
	      false,new_length)
	    v
	    (true,0) in
	()
    and format_premises2 prefix premises first pred_table cst_table map set =
      let () = match first with
	| true -> Printf.fprintf stdout ":--" 
	| false -> Printf.fprintf stdout "\n%s|--" prefix in
      match premises with
      | [] -> ()
      | [p] -> 
	let () = 
	  try
	    format_derivation (Printf.sprintf "%s   " prefix) p (PredicateMap.find p map) pred_table cst_table map set
	  with
	  | Not_found -> Printf.fprintf stdout "%s" (ASPred.to_string p pred_table cst_table)   in
	Printf.fprintf stdout ""
      | p::tl ->
	let () = 
	  try
	    format_derivation (Printf.sprintf "%s   " prefix) p (PredicateMap.find p map) pred_table cst_table map set
	  with
	  | Not_found -> Printf.fprintf stdout "%s" (ASPred.to_string p pred_table cst_table)   in
	let () = format_premises2 prefix tl false  pred_table cst_table map set in
	Printf.fprintf stdout ""
	  

    let add_map_to_premises_to_buffer b pred_table cst_table map =
      PredicateMap.iter
	(fun k v ->  
	  PremiseSet.iter
	    (fun premise ->
	      Buffer.add_string
		b
		(Printf.sprintf
		   "%s <- %s\n"
		   (ASPred.to_string k pred_table cst_table)
		   (Premise.to_string premise pred_table cst_table)))
	    v)
	map




    let facts_to_string facts pred_table cst_table =
      let buff=Buffer.create 100 in
      let () = add_map_to_facts_to_buffer buff pred_table cst_table facts in 
      Buffer.contents buff


    let add_to_map_to_set k v m =
      let current_set =
	try
	  PredicateMap.find k m
	with
	| Not_found -> PremiseSet.empty in 
      PredicateMap.add k (PremiseSet.add v current_set) m

  end
    
  module Derivation =
  struct

  end


  module Rule =
  struct
    
    (** In a [rule], all the compoments of all the predicates
	are stored in a {! UnionFind} indexed data structure. We assume
	here that from [1] to [lhs.arity] the components of the left
	hand side predicate are stored, then from [lhs.arity+1] to
	[lhs.arity+(hd rhs).arity] the components of the first predicate
	on the right hand side are stored, etc. It is assumed that this
	structure is correct (no cycle, links within the range, etc.) *)
    type rule={id:int;
	       lhs:Predicate.predicate;
	       e_rhs:(Predicate.predicate*int) list;
	       i_rhs:(Predicate.predicate*int) list;
	       content:ConstGen.id UF.t;
	      (* TODO: Maybe put the label of the predicate in the
		 content in order to enforce checking of the current
		 instantiation *)
	      (*	       abs_rule:ASRule.rule;*)
	      }
      
    module Rules=Set.Make(struct
      type t=rule
      let compare {id=i} {id=j} = i-j
    end)
      
	
  (** [make_rule r] returns an internal rule, that is one whose
      content is now a {! UnionFind.UnionFind} indexed data
      structure *)
	
    let make_rule {ASRule.id=id;ASRule.lhs=lhs;ASRule.e_rhs=e_rhs;ASRule.i_rhs=i_rhs} =
    (* Be careful, the list of the rhs is reversed *)
      LOG "Preparing the lhs content..." LEVEL TRACE;
      let lhs_content=
	Predicate.add_pred_arguments_to_content lhs.ASPred.arguments ([],1,VarGen.IdMap.empty) in
      LOG "Done." LEVEL TRACE;
      LOG "Preparing the e_rhs..." LEVEL TRACE;
      let e_rhs,e_rhs_content =
	List.fold_left
	  (fun (rhs,content) ({ASPred.p_id=n;ASPred.arity=k;ASPred.arguments=pred_args},pos) ->
	    (({Predicate.p_id=n;Predicate.arity=k},pos) :: rhs,
	     Predicate.add_pred_arguments_to_content pred_args content))
	  ([],lhs_content)
	  e_rhs in
      LOG "Done." LEVEL TRACE;
      LOG "Preparing the i_rhs..." LEVEL TRACE;
      let i_rhs,(content,_,_) =
	List.fold_left
	  (fun (rhs,content) ({ASPred.p_id=n;ASPred.arity=k;ASPred.arguments=pred_args},pos) ->
	    (({Predicate.p_id=n;Predicate.arity=k},pos) :: rhs,
	     Predicate.add_pred_arguments_to_content pred_args content))
	  ([],e_rhs_content)
	  i_rhs in
      LOG "Done. Content is of size %d" (List.length content) LEVEL TRACE;
      let internal_content = UF.create (List.rev content) in
      LOG "It is represented by:" LEVEL TRACE;
      let () =
	List.iter (fun c -> LOG c LEVEL TRACE) (Bolt.Utils.split "\n" (UF.to_string internal_content)) in
      {id=id;
       lhs=Predicate.make_predicate lhs;
       e_rhs=List.rev e_rhs;
       i_rhs=List.rev i_rhs;
       content=internal_content
      }
	
  (* the [dag] parameter [h] is meant to be the components of some
     predicate or rule *)
    let cyclic_unify i j h =
      match UF.cyclic i h with
      | true,_ -> raise Fails
      | _, h' -> 
	(try UF.union i j h with
	| UF.Union_Failure -> raise Fails)

	  
    (** [extract_consequence r content] returns a fact from
	content. The arguments are of the form [Const c] or [Var v]
	(that is something of type {!
	Datalog_AbstractSyntax.AbstractSyntax.Predicate.term}). When
	it is a [Var v], it means that when this variable range over
	the constants of the program, it still are facts (=
	provable). *)
    let extract_consequence r content =
      let args,_,_= 
	List.fold_left 
	  (fun (args,varmap,vargen) elt ->
	    match elt with 
	    | UF.Value v -> (ASPred.Const v )::args,varmap,vargen
	    | UF.Link_to i -> 
	      let new_var,new_varmap,new_vargen = 
		try
		  Utils.IntMap.find i varmap,varmap,vargen
		with
		| Not_found -> let n_v,n_vg=VarGen.get_fresh_id vargen in
			       n_v,Utils.IntMap.add i n_v varmap,n_vg in
	      (ASPred.Var new_var)::args,new_varmap,new_vargen )
	  ([],Utils.IntMap.empty,VarGen.init ())
	  (UF.extract (r.lhs.Predicate.arity) content) in
      {ASPred.p_id=r.lhs.Predicate.p_id;
       ASPred.arity=r.lhs.Predicate.arity;
       ASPred.arguments=List.rev args }
    (* TODO: Directly extract from content, then the list would be
       crossed only once *)
	    
    (** [to_abstract r content] returns a datalog abstract syntax rule
	where the arguments of all (datalog abstract syntax)
	predicates have been computed using [content]. *)
    let to_abstract {id=id;lhs=lhs;e_rhs=e_rhs;i_rhs=i_rhs} content pred_table =
      LOG "Going to work with the following content:" LEVEL TRACE;
      let () =
	List.iter
	  (fun s -> LOG s LEVEL TRACE)
	  (Bolt.Utils.split "\n" (UF.to_string content)) in
      let abs_lhs,vars,vargen=Predicate.to_abstract lhs (1,content) (Utils.IntMap.empty,VarGen.init ()) pred_table in
      let abs_e_rhs,start',vars',vargen'=Predicate.lst_to_abstract e_rhs (1+lhs.Predicate.arity,content) (vars,vargen) pred_table in
      let abs_i_rhs,_,_,_ = Predicate.lst_to_abstract i_rhs (start',content) (vars',vargen') pred_table in
      {ASRule.id=id;
       ASRule.lhs=abs_lhs;
       ASRule.e_rhs=abs_e_rhs;
       ASRule.i_rhs=abs_i_rhs
      }


    (** [FactArray] is a module implementing a traversal of facts using
	the {! ArrayTraversal.Make} functor. The [update] function is
	such that we don't consider cells (i.e. facts) that don't unify
	with the rule (i.e. a {! UF.Union_Failure} exception was
	raised).*)
    module FactArray=ArrayTraversal.Make2(
      struct
	type cell = Predicate.FactSet.elt (*P.fact *)
  	type state = (int*(ConstGen.id UF.t))*(cell list)
	(* The state [(i,c),lst] stores the next index [i] of the
	   content [c] where the update should start start, and [lst]
	   keep track of the facts against which the content has been
	   unified. {e Be careful:} it stores them in the reverse
	   order.*)

	module CellSet=Predicate.FactSet
	let update (s,cells) c =
	  try 
	    Some (Predicate.instantiate_with c s,c::cells)
	  with
	  | UF.Union_Failure -> None
      end
    )
      
    (** [immediate_consequence_of_rule r db] returns a list of facts
	generated by the rule [r] using the facts stored in [db]. {e
	*these facts are not added to [db] when collecting the new
	facts*}.

	Note that it is important that resulting states need to be
	processed otherwise they will be lost in backtracking when using
	{! PersistentArray}.*)
    let rec immediate_consequence_of_rule r db =
    (* We collect all the contents compatible with the facts of the
       database corresponding to intensional predicates *)
      let make_search_array_i_pred =
	List.map (fun (pred,_) -> Predicate.PredMap.find pred.Predicate.p_id db) r.i_rhs in
    (* We define the function to be run on each reached end state of
       the instantiation with the extensional predicates *)
      let resume_on_i_pred acc state =
	FactArray.collect_results
	  (fun l_acc ((_,content),_) -> (extract_consequence r content)::l_acc)
	  acc
	state
	make_search_array_i_pred in
    (* We now collect all the contents compatible with the facts of
       the extensional database (facts of the database corresponding
       to extensional predicates). *)
    let make_search_array_e_pred =
      List.map (fun (pred,_) -> Predicate.PredMap.find pred.Predicate.p_id db) r.e_rhs in
    FactArray.collect_results
      (fun acc s -> resume_on_i_pred acc s)
      []
      ((r.lhs.Predicate.arity+1,r.content),[])
      make_search_array_e_pred
      
  end

  module Program =
  struct
    type program = {rules:Rule.rule list Predicate.PredMap.t;
		    (* the list of the rules of the program indexed by
		       the id of the lhs predicate *)
		    edb:ASPred.pred_id list;
		    (* the list of the ids of the extensional
		       predicates *)
		    edb_facts:Predicate.FactSet.t Predicate.PredMap.t;
		    (* a map from predicate ids to facts for this
		       predicate*)
		    idb:ASPred.pred_id list;
		    (* the list of the ids of the intensional
		       predicates *)
		    pred_table: ASPred.PredIdTable.table;
		    (* the table to record the translation from ids to
		       sym of the predicate *)
		    const_table: ConstGen.Table.table;
		    (* the table to record the translation from ids to
		       sym of the constants *)
		    rule_id_gen:IdGenerator.IntIdGen.t;
		    (* the id generator for the rules in case rules
		       are to be added after the first built of the
		       program*)
(*		    e_pred_to_rules: Rule.Rules.t AbstractSyntax.Predicate.PredIdMap.t; *)
		   (* a map keeping track of the rules where
		      extensional predicates occur so that when a rule
		      is dynamically added, if it turns an extensional
		      predicate into an intensional one, we can modify
		      the rules accordingly *)
		   (* This feature is an overkill for the kind of
		      extensions we're interested in for ACG parsing,
		      where only facts with edb predicates are added
		      when extending the program. To it is suppressed
		      for the moment *)
		   }


    let empty = {
      rules=Predicate.PredMap.empty;
      edb=[];
      idb=[];
      edb_facts=Predicate.PredMap.empty;
      pred_table=ASPred.PredIdTable.empty;
      const_table=ConstGen.Table.empty;
      rule_id_gen=IdGenerator.IntIdGen.init ()}

    let extend_map_to_list k v map_list =
      try
	let lst=Predicate.PredMap.find k map_list in
	Predicate.PredMap.add k (v::lst) map_list
      with
      | Not_found -> Predicate.PredMap.add k [v] map_list

    let extend_map_to_set k v map_to_set =
      let current_set = 
	try
	  Predicate.PredMap.find k map_to_set
	with
	| Not_found -> Predicate.FactSet.empty in
      Predicate.PredMap.add k (Predicate.FactSet.add v current_set) map_to_set


	
    let make_program {ASProg.rules=r;ASProg.pred_table=pred_table;ASProg.const_table=cst_table;ASProg.i_preds=i_preds;ASProg.rule_id_gen;ASProg.e_pred_to_rules} =
      let rules,e_facts,rule_to_rule_map = 
	ASRule.Rules.fold
	  (fun ({ASRule.lhs=lhs} as r) (acc,e_facts,r_to_r) ->
	    LOG "Dealing with rule:\t%s" (ASRule.to_string r pred_table cst_table) LEVEL TRACE;
	    let new_rule = Rule.make_rule r in
	    let updated_e_facts = 
	      if not (ASPred.PredIds.mem lhs.ASPred.p_id i_preds) then
		extend_map_to_set lhs.ASPred.p_id lhs e_facts 
	      else
		e_facts in
	    extend_map_to_list lhs.ASPred.p_id new_rule acc,updated_e_facts,ASRule.RuleMap.add r new_rule r_to_r)
	  r
	  (Predicate.PredMap.empty,Predicate.PredMap.empty,ASRule.RuleMap.empty) in
      LOG "All rules done." LEVEL TRACE;
      LOG "Now separate the e and i predicates." LEVEL TRACE;
      let edb,idb=
	ASPred.PredIdTable.fold
	  (fun k _ (e,i) ->
	    if ASPred.PredIds.mem k i_preds then
	      (e,k::i)
	    else
	      (k::e,i))
	  pred_table
	  ([],[]) in
      LOG "Done." LEVEL TRACE;
      {rules=rules;
       edb=edb;
       edb_facts=e_facts;
       idb=idb;
       pred_table=pred_table;
       const_table=cst_table;
       rule_id_gen;
       (*e_pred_to_rules=
	  AbstractSyntax.Predicate.PredIdMap.map
	    (fun rules -> 
	      AbstractSyntax.Rule.Rules.fold
		(fun r acc -> Rule.Rules.add (ASRule.RuleMap.find r rule_to_rule_map) acc)
		rules
		Rule.Rules.empty)
	    e_pred_to_rules*)
      }
	


	
    let to_abstract {rules=r;idb=idb;pred_table=pred_table;const_table=cst_table;rule_id_gen;edb_facts(*e_pred_to_rules*)} =
      LOG "Transforming internal rules into abastract ones..." LEVEL TRACE;
      let rules = 
	Predicate.PredMap.fold
	  (fun _ rules acc -> 
	    List.fold_left
	      (fun acc' rule -> 
		ASRule.Rules.add (Rule.to_abstract rule rule.Rule.content pred_table) acc')
	      acc
	      rules)
	  r
	  ASRule.Rules.empty in
      LOG "Done." LEVEL TRACE;
      LOG "Transforming facts into rules" LEVEL TRACE;
      let rules,rule_id_gen = 
	Predicate.PredMap.fold
	  (fun pred fact_set (acc,gen) -> 
	    Predicate.FactSet.fold 
	      (fun fact (l_acc,id_rule_gen) -> 
		let id_rule,id_rule_gen=IdGenerator.IntIdGen.get_fresh_id id_rule_gen in
		ASRule.(Rules.add {id=id_rule;lhs=fact;e_rhs=[];i_rhs=[]} l_acc),id_rule_gen)
	      fact_set
	      (acc,gen))
	  edb_facts
	  (rules,rule_id_gen) in
      LOG "Done." LEVEL TRACE;
      let i_preds=
	List.fold_left
	  (fun acc id -> ASPred.PredIds.add id acc)
	  ASPred.PredIds.empty
	  idb in
      ASProg.({rules=rules;
	       pred_table=pred_table;
	       const_table=cst_table;
	       i_preds=i_preds;
	       rule_id_gen;
	       e_pred_to_rules=AbstractSyntax.Predicate.PredIdMap.empty
(*	  AbstractSyntax.Predicate.PredIdMap.map
	    (fun rules -> 
	      Rule.Rules.fold
		(fun r acc -> 
		  AbstractSyntax.Rule.Rules.add
		    (Rule.to_abstract r r.Rule.content pred_table) 
		    acc)
		rules
		AbstractSyntax.Rule.Rules.empty)
	    e_pred_to_rules*)
	      })
	
	
	
  (** [temp_facts r e_facts previous_step_facts facts delta_facts
      agg_f start] returns the result of applying [agg_f] to [start]
      and to all the facts that are deduced from [temp]{^ [time+1]}{_
      [S]} where [S] is the head predicate of the rule [r] and [temp]
      is the set of temporary rules associated with [r] as in the
      algorithm described in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu} (p.315).
      
      [previous_step_facts] and [facts] denote the intentional facts
      at the two required successive steps and [delta_facts] denote
      the new facts that are computed during this step. *)
	
  (* TODO: if a set of facts for a predicate of the rhs is empty, we
     can stop the computation *)
    let temp_facts r e_facts previous_step_facts facts delta_facts agg_function start pred_table cst_table =
      LOG "Scanning the rule: %s" (ASRule.to_string (Rule.to_abstract r r.Rule.content pred_table) pred_table cst_table) LEVEL TRACE;
      (* We first collect all the contents compatible with the facts of
	 the intensional database. They depend on the intensional
	 predicate [delta_position] and the ones that are before it
	 ([rev_pred_lst]) and the ones that are after it
	 ([pred_lst]). This triple correspond to a {!Focused_list.t}
	 type. *)
      let make_search_array_i_pred (rev_pred_lst,delta_position,pred_lst) =
	let facts_at_delta_position=
	  try
	    Predicate.PredMap.find delta_position.Predicate.p_id delta_facts
	  with
	  | Not_found -> Predicate.FactSet.empty in
	let end_pred_facts =
	  List.map
	    (fun pred -> 
	      try
		Predicate.PredMap.find pred.Predicate.p_id previous_step_facts
	      with
	      | Not_found -> Predicate.FactSet.empty)
	    pred_lst in
	List.fold_left
	  (fun acc pred ->
	    try
	      (Predicate.PredMap.find pred.Predicate.p_id facts)::acc
	    with
	    | Not_found -> acc)
	  (facts_at_delta_position::end_pred_facts)
	  rev_pred_lst in
      (* We define the function to be run on each reached end state of
	 the instantiation with the extensional predicates. This
	 function will run a result collection (with
	 [FactArray.collect_results]) for each of the possible
	 [delta_facts], that is for each of the possible [Focused_list]
	 that can be reach from [zip] (including [zip] itself). *)
      let resume_on_i_pred acc (((i,content),premises) as state) =
	match r.Rule.i_rhs with
	| [] -> agg_function ((Rule.extract_consequence r content),premises) acc
	| _ -> 
	(* We now init the focused list corresponding to the intensional
	   predicates of the rule [r] *)
	  let zip=Focused_list.init (fst (List.split r.Rule.i_rhs)) in
	  Focused_list.fold
	    (fun l_acc focus ->
	    (* For a given focus in the intensional list of predicates
	       of [r], we extract all the possible facts from the rule
	       [r] *)
	      Rule.FactArray.collect_results
		(fun ll_acc ((_,content),premises) -> agg_function ((Rule.extract_consequence r content),premises) ll_acc)
		l_acc
		state
		(make_search_array_i_pred focus))
	    acc
	    zip in
      (* We now collect all the contents compatible with the
	 facts of the extensional database *)
      let make_search_array_e_pred =
	List.map
	  (fun (pred,_) -> 
	    try
	      Predicate.PredMap.find pred.Predicate.p_id e_facts
	    with
	    | Not_found -> Predicate.FactSet.empty) 
	  r.Rule.e_rhs in
      Rule.FactArray.collect_results
	(fun acc s -> 
	  (* For each partial completion of the rule on the extensional
	     database, we need to take into account the remaining
	     intensional predicates. *)
	  resume_on_i_pred acc s)
	start
	((r.Rule.lhs.Predicate.arity+1,r.Rule.content),[])
	make_search_array_e_pred
	
    let custom_find k map =
      try
	Predicate.PredMap.find k map
      with
      | Not_found -> Predicate.FactSet.empty


  (** [p_semantics_for_predicate s prog e_facts previous_step_facts
      facts delta_facts] returns a set of all the facts that can
      deduced by all the rules in [prog] at a given step and whose lhs
      predicate is [s] when the edb is [e_facts], the step has
      produced [facts] and the previous step has produced
      [previous_step_facts] and the variation of facts at this step
      are [delta_facts].
      
      It corresponds to [P]{^ [time]}{_ [S]} [(edb,T]{^ [time -1]}{_
      [1]}[,...,T]{^ [time-1]}{_ [l]}[,T]{^ [time]}{_ [1]}[,...,T]{^
      [time]}{_ [l]}[, Delta]{^ [time]}{_ [T]{_ [1]}},...,[Delta]{^
      [time]}{_ [T]{_ [l]}}) in {{:
      http://webdam.inria.fr/Alice/pdfs/Chapter-13.pdf} Chap. 13 of
      "Foundations of Databases", Abiteboul, Hull, and Vianu} *)
    let p_semantics_for_predicate s_id prog e_facts previous_step_facts facts delta_facts derivations =
      List.fold_left
	(fun acc r ->
	  temp_facts
	    r
	    e_facts
	    previous_step_facts
	    facts
	    delta_facts
	    (fun (new_fact,from_premises) (new_fact_set,new_fact_derivations) -> 
	      (Predicate.conditionnal_add
		new_fact
		new_fact_set
		(custom_find r.Rule.lhs.Predicate.p_id previous_step_facts)
		(custom_find r.Rule.lhs.Predicate.p_id delta_facts),
	       Predicate.add_to_map_to_set new_fact from_premises new_fact_derivations))
	    acc
	    prog.pred_table
	    prog.const_table)
	(Predicate.FactSet.empty,derivations)
	(Predicate.PredMap.find s_id prog.rules)
	
    let seminaive prog =
      (** [seminaive_aux facts delta_facts] returns [(S]{^
	  [i]}[,][Delta]{^ [i+1]}{_ [S]}[)] for all [S] when [facts]
	  corresponds to [S]{^ [i-1]} for all [S] and [delta_facts] to
	  [Delta]{^ [i]}{_ [S]} for all [S] *)
      let rec seminaive_aux facts delta_facts derivations =
	(* TODO: Check that PredMap has all intensional predicates of
	   prog *)
	let new_facts = 
	  Predicate.PredMap.merge
	    (fun pred_id v1 v2 ->
	      match v1,v2 with
	      | Some l1,Some l2 -> Some (Predicate.FactSet.union l1 l2)
	      | Some _ as v,None -> v
	      | None, (Some _ as v) -> v
	      | None,None -> None)
	    facts
	    delta_facts in
	let new_delta_facts,new_derivations_for_all_i_pred = 
	  List.fold_left
	    (fun (acc,derivations) pred ->
	      LOG "Trying to derive facts for: %s" (ASPred.to_string {ASPred.p_id=pred;ASPred.arity=0;ASPred.arguments=[]} prog.pred_table prog.const_table) LEVEL DEBUG;
	      let new_facts_for_pred,new_derivations=
		p_semantics_for_predicate
		  pred
		  prog
		  prog.edb_facts
		  facts
		  new_facts
		  delta_facts
		  derivations in
	      if Predicate.FactSet.is_empty new_facts_for_pred then
		acc,new_derivations
	      else
		Predicate.PredMap.add
		  pred
		  new_facts_for_pred
		  acc,new_derivations) 
	    (Predicate.PredMap.empty,derivations)
	    prog.idb  in 
	LOG "% d new facts:" (Predicate.PredMap.fold (fun _ v acc -> acc+(Predicate.FactSet.cardinal v)) new_delta_facts 0) LEVEL DEBUG;
	let () = 
	  List.iter
	    (fun s -> LOG s LEVEL DEBUG)
	    (Bolt.Utils.split "\n" (Predicate.facts_to_string new_delta_facts prog.pred_table prog.const_table)) in
	(new_facts,new_delta_facts,new_derivations_for_all_i_pred) in
      (** [seminaive_rec (facts,delta_facts)] returns the result when
	  the fixpoint is reached, ie when [seminaive_aux facts
	  delta_facts] does not produce any new fact. This is the
	  iteration at step 5 in the seminaive algo. *)
      let rec seminaive_rec (facts,delta_facts,derivations)= 
	if Predicate.PredMap.is_empty delta_facts then
	  facts,derivations
	else
	  seminaive_rec (seminaive_aux facts delta_facts derivations) in
      let first_step_results = seminaive_aux prog.edb_facts Predicate.PredMap.empty Predicate.PredicateMap.empty in
      seminaive_rec first_step_results

    let extend prog {ASProg.modified_rules;ASProg.new_pred_table;ASProg.new_const_table;ASProg.new_i_preds;ASProg.new_e_preds;ASProg.new_rule_id_gen;}=
      let i_preds = 
	ASPred.PredIds.fold
	  (fun e acc ->
	    if List.mem e prog.idb then
	      acc
	    else
	      e::acc)
	  new_i_preds
	  prog.idb in
      let internal_modified_rules,updated_e_facts = 
	ASRule.Rules.fold 
	  (fun r (acc,e_facts) -> 
	    let new_rule = Rule.make_rule r in
	    let updated_e_facts =
	      if 
		not (ASPred.PredIds.mem r.ASRule.lhs.ASPred.p_id new_i_preds)
		&& not (List.mem r.ASRule.lhs.ASPred.p_id prog.idb)
	      then
		extend_map_to_set r.ASRule.lhs.ASPred.p_id r.ASRule.lhs e_facts 
	      else
		e_facts in
	    Rule.Rules.add new_rule acc,updated_e_facts)
	  modified_rules
	  (Rule.Rules.empty,prog.edb_facts)
      in
      let updated_rules =
	Rule.Rules.fold
	  (fun ({Rule.lhs=lhs} as rule) acc -> 
	    try
	      Predicate.PredMap.add
		lhs.Predicate.p_id
		(rule::(List.filter Rule.(fun r -> r.id=rule.id) (Predicate.PredMap.find lhs.Predicate.p_id acc)))
		acc
	    with
	    | Not_found -> Predicate.PredMap.add lhs.Predicate.p_id [rule] acc)
	  internal_modified_rules
	  prog.rules in
      {rules=updated_rules;
       edb=
	  ASPred.PredIds.fold
	    (fun e acc -> 
	      if List.mem e prog.idb then
		acc
	      else
		e::acc)
	    new_e_preds
	    prog.edb;
       edb_facts=updated_e_facts;
       idb=i_preds;
       pred_table=new_pred_table;
       const_table=new_const_table;
       rule_id_gen=new_rule_id_gen;
      }

    let add_e_fact prog (r,const_table,rule_id_gen) =
      if List.mem r.ASRule.lhs.ASPred.p_id prog.idb then
	failwith (Printf.sprintf "BUG: You're not supposed to extend a program with an intensional predicate \"%s\"" (ASPred.to_string {ASPred.p_id=r.ASRule.lhs.ASPred.p_id;ASPred.arity=r.ASRule.lhs.ASPred.arity;ASPred.arguments=[]} prog.pred_table  ConstGen.Table.empty))
      else
	{prog with 
	  edb_facts=
	    extend_map_to_set r.ASRule.lhs.ASPred.p_id r.ASRule.lhs prog.edb_facts;
	  const_table;
	  rule_id_gen}

    let add_e_facts prog (r_lst,const_table,rule_id_gen) =
      {prog with 
	edb_facts=
	  List.fold_left
	    (fun acc r ->
	      if List.mem r.ASRule.lhs.ASPred.p_id prog.idb then
		failwith (Printf.sprintf "BUG: You're not supposed to extend a program with an intensional predicate \"%s\"" (ASPred.to_string {ASPred.p_id=r.ASRule.lhs.ASPred.p_id;ASPred.arity=r.ASRule.lhs.ASPred.arity;ASPred.arguments=[]} prog.pred_table  ConstGen.Table.empty))
	      else
		extend_map_to_set r.ASRule.lhs.ASPred.p_id r.ASRule.lhs acc)
	    prog.edb_facts
	    r_lst;
	const_table;
	rule_id_gen}
	

    (** TODO: only useful until we change the type of idb and idb
	to sets *)

    let rec list_extension_aux a lst scanned_lst =
      match lst with
      | [] -> List.rev (a::scanned_lst)
      | b::tl when a=b -> List.rev_append scanned_lst lst
      | b::tl -> list_extension_aux a tl (b::scanned_lst)

    let list_extension a lst = list_extension_aux a lst []

    (** [add_rule r p] adds a [ASRule.rule] to a [Datalog.Program]
	with the assumption that it will not change the {em
	nature} of a predicate (that is making it change from
	extensional to intensional). *)
      
    let add_rule r prog =
      let new_rule = Rule.make_rule r in
      let lhs_pred=r.ASRule.lhs.ASPred.p_id in
      let new_e_facts,new_edb,new_idb =
	match r.ASRule.e_rhs,r.ASRule.i_rhs with
	| [],[] -> 
	  extend_map_to_set lhs_pred r.ASRule.lhs prog.edb_facts,
	  list_extension lhs_pred prog.edb,
	  prog.idb
	| _,_ -> prog.edb_facts,prog.edb,list_extension lhs_pred prog.idb in
      {prog with
	rules=extend_map_to_list lhs_pred new_rule prog.rules;
	edb_facts=new_e_facts;
	edb=new_edb;
	idb=new_idb}
	 
	

    let get_fresh_rule_id ({rule_id_gen} as prog) =
      let new_id,rule_id_gen=IdGenerator.IntIdGen.get_fresh_id rule_id_gen in
      new_id,{prog with rule_id_gen}

    let get_fresh_cst_id name ({const_table} as prog) =
      let id,const_table=ConstGen.Table.add_sym name const_table in
      id,{prog with const_table}

    let add_pred_sym name ({pred_table} as prog) =
      let p_id,pred_table=ASPred.PredIdTable.add_sym name pred_table in
      p_id,{prog with pred_table}
    
	
  end
    
    
end
  
  
module Datalog=Make(UnionFind.StoreAsMap)
