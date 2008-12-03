module Datalog_solver =
struct

  open Program

  type item = It of (int*int list)

  type eval_rule = ER of (predicate* predicate * predicate list * int Int_map.t ) (* the first predicate is the conclusion, the second is the next that need to be identified, the list contains the predicates that need to be proved and the int_map is the substitution that has been computed so far.*)

  type result = RER of eval_rule | RIt of item

  exception Not_unifiable

  let get_eval_rule (Cl(lhs,rhs)) = 
    (match rhs with
        [] -> failwith "get_eval_rule: rule with an empty right hand side?"
      | pred::rhs_tl ->     ER (lhs, pred, rhs_tl, Int_map.empty)
    )
       

  let instanciate (Pred(k,args)) subst =
    It(k,
      List.fold_right
        (function arg ->
          function l ->
            (Int_map.find arg subst)::l
        )
        args
        []
    )

  let rec eliminate_eq_neq predicates eq neq subst =
    (match predicates with
        Pred(k,[arg1;arg2])::predicates_tl ->
          if k=eq
          then
            (try
                let k1 = Int_map.find arg1 subst in
                  (try 
                      let k2 = Int_map.find arg2 subst in
                        if k1 = k2 
                        then eliminate_eq_neq predicates_tl eq neq subst
                        else raise Not_unifiable
                    with 
                        Not_found ->
                          eliminate_eq_neq predicates_tl eq neq
                            (Int_map.add arg2 k1 subst)
                  )
              with Not_found ->
                (try 
                    let k2 = Int_map.find arg2 subst in
                       eliminate_eq_neq predicates_tl eq neq
                            (Int_map.add arg1 k2 subst)
                    with 
                        Not_found ->
                          raise Not_unifiable
                  )
            )
          else
            (if k=neq
            then 
              (try
                  let k1 = Int_map.find arg1 subst in
                    (try 
                        let k2 = Int_map.find arg2 subst in
                          if k1 != k2
                          then eliminate_eq_neq predicates_tl eq neq subst
                          else raise Not_unifiable
                      with
                          Not_found ->
                            raise Not_unifiable
                    )
                with 
                    Not_found ->
                      raise Not_unifiable
              )
              else
                (subst,predicates)
            )
              
      |_ -> (subst,predicates)
          
    )   

  
        
    
 
  module Ordered_items =
    struct 
      type t = item
      let compare k1 k2 = Pervasives.compare k1 k2
    end

  module Ordered_eval_rules = 
    struct
      type t = eval_rule
      let compare k1 k2 = Pervasives.compare k1 k2
    end

  module Item_set = Set.Make(Ordered_items)
  module Item_map = Map.Make(Ordered_items)
  module ER_set = Set.Make(Ordered_eval_rules)

  type chart =(int Item_map.t) array (*array whose ith address contains the function f whose domain contains the lists args such that the ith predicate holds for the list args and that this fact has been added at the kth step of the algo wher k = f(args)*)

  type eval_rules = (ER_set.t) array (*array whose ith address contains the eval_rules that can be completed by an instance of the ith predicate *)

  type agenda = Item_set.t (*set containing pairs (predicate identifier, arg instanciation)*)

  type memory = M of (Signature.signature*chart*eval_rules*agenda*(item list)*int*int*int)

  exception No_more_step

  let get_predicates_in_chart k (M(_,chart,_,_,_,_,_,_)) = (Array.get chart k)
    

  let add_mem (It(k,_) as item) (M(sign,chart,ev_rules,agenda,facts,step,eq,neq) as mem) =
    if Item_map.mem item (get_predicates_in_chart k mem)
    then mem 
    else 
      let agenda = Item_set.add item agenda in
        M(sign,chart,ev_rules,agenda,facts,step,eq,neq)

  let ev_rule_known (ER(_,Pred(k,_),_,_)as ev_rule) (M(_,_,ev_rules,_,_,_,_,_)) = 
    ER_set.mem ev_rule (Array.get ev_rules k)

  let add_ev_rule (ER(_,Pred(k,_),_,_)as ev_rule) (M(sign,chart,ev_rules,agenda,facts,step,eq,neq)) = 
    Array.set ev_rules k (ER_set.add ev_rule (Array.get ev_rules k));
    M(sign,chart,ev_rules,agenda,facts,step,eq,neq)
          

  let rec extend_eval_rule
      (ER(res,Pred(k,args_pred),predicates,subst))
      (It(l,args_item))
      (M(_,_,_,_,_,_,eq,neq) as mem)
      = 
    (if k=l
    then
      (try 
          let subst = 
            List.fold_left2
              (function subst ->
                function arg_pred->
                  function arg_item->
                    (try
                        let k = Int_map.find arg_pred subst in
                          if k = arg_item
                          then subst
                          else raise Not_unifiable
                      with 
                          Not_found ->
                            (Int_map.add arg_pred arg_item subst)
                    )
              )
              subst
              args_pred
              args_item
          in
            (match eliminate_eq_neq predicates eq neq subst with
                (subst,[]) -> 
                  (try
                    add_mem (instanciate res subst) mem
                  with Not_found -> 
                    (match res with Pred(k,_) -> failwith ("PB: pred "^(string_of_int k))))
              | (subst,(Pred(k,_) as pred)::predicates) -> 
                  let ev_rule = (ER(res,pred,predicates,subst)) in
                  if ev_rule_known ev_rule mem
                  then mem
                  else 
                    let mem = add_ev_rule ev_rule mem in
                      Item_map.fold
                        (function item ->
                          function _ ->
                            function mem ->
                              extend_eval_rule 
                                ev_rule
                                item 
                                mem
                        )
                        (get_predicates_in_chart k mem)
                        mem
            )
              
        with
            Not_unifiable -> mem
      ) 
      else failwith "extend_eval_rule: trying to extend a rule with a rong item.")

  let rec process_item (It(pred,_) as item) (M(_,_,ev_rules,_,_,_,_,_) as mem) = 
    let pred_ev_rules = Array.get ev_rules pred in
    let (M(sign,chart,ev_rules,agenda,facts,step,eq,neq)) = 
      ER_set.fold
        (function ev_rule ->
          function mem ->
            extend_eval_rule ev_rule item  mem
        )
        pred_ev_rules
        mem
    in
    let agenda = Item_set.remove item agenda in
    let _ = Array.set 
      chart 
      pred  
      (Item_map.add item step (Array.get chart pred)) 
    in
      M(sign,chart,ev_rules,agenda,facts,step,eq,neq)
    


  let next_step (M(sign,chart,ev_rules,agenda,facts,step,eq,neq)) = 
    (match facts with
        [] -> raise No_more_step
      | fact::remaining_facts ->
          let _ = print_string ("Make step: "^(string_of_int (step+1))^".\n") in
          M(sign,chart,ev_rules,Item_set.add fact agenda,remaining_facts,step+1,eq,neq)
    )

  let rec naive_solve (M(sign,chart,ev_rules,agenda,facts,step,eq,neq) as mem)=
   if(Item_set.is_empty agenda)
   then 
     (if facts=[]
     then mem
       else naive_solve(next_step mem))
   else 
     naive_solve(process_item (Item_set.choose agenda) mem)

  let init_solver program facts =
    match program with
        Prog(sign,clauses) ->
          let n = Signature.fresh sign in
          let chart = Array.make n (Item_map.empty) in
          let ev_rules = Array.make n (ER_set.empty) in
          let agenda = Item_set.empty in
          let step = -1 in
          let (eq,sign) = Signature.add_eq_get_id sign in
          let (neq,sign) = Signature.add_neq_get_id sign in
          let _ =  
            List.fold_left
              (function () ->
                function Cl(pred,(Pred(k,args) as fst)::rem) ->
                  let ev_rule = ER(pred,fst,rem,Int_map.empty) in
                    Array.set ev_rules k (ER_set.add ev_rule (Array.get ev_rules k))
                  |_ -> failwith "init_solver: the program contains a clause with no rhs."
              )
              ()
              clauses
          in
            M(sign,chart,ev_rules,agenda,facts,step,eq,neq)

  let init_solver2 program list magic =
    let Prog(sign,_) = program in
    let (_,facts) = 
      List.fold_left
        (function (n,l) ->
          function string ->
            try
              let (_,pred) = Signature.find_pred_of_name string sign in
                (n+1,It(pred,[n;n+1])::l)
            with
                Not_found ->
                  failwith ("init_solver2: predicate '"^string^"' is not defined.")
        )
        (0,[])
        list
    in
      init_solver 
        program 
        (It(magic,[0])::(List.rev facts))

  let solve program facts = naive_solve(init_solver program facts)
            

  let solve2 program list magic = naive_solve(init_solver2 program list magic)

  let print_chart (M(sign,chart,_,_,_,step,_,_)) = 
    let res = Array.make (step+1) [] in
    let _ = 
      Array.fold_right
        (function set ->
          function () ->
            Item_map.fold
              (function It(k,args)->
                function step0 ->
                  function () ->
                    Array.set 
                      res 
                      step0 
                      ((Signature.get_name k sign,args)::(Array.get res step0))
              )
              set
              ()
        )
        chart
        ()
    in
      res
end
