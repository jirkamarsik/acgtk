module Entry =
struct
  type data = sig_dec
  and sig_dec = 
    | No_sig_dec
    | Sig of sig_dec_id
  and sig_dec_id =
    | No_sig_dec_id
    | Sig_dec_id of sig_dec_equal
  and sig_dec_equal =
    | No_sig_dec_equal
    | Sig_dec_equal of entry
  and entry =
    | No_entry
    | Entry_id of entry_id_list
    | Prefix_infix of symbol
    | Binder of binder_id
  and entry_id_list =
    | No_entry_id_list
    | Comma_in_id_list
    | Colon_assignment of type_kwd_or_type_or_term
    | Equal_def of type_or_term_in_def
  and symbol =
    | No_symbol
    | Symbol 
  and binder_id =
    | No_binder_id
    | Binder_id
  and term_dec_or_def =
    | No_term_dec_or_def
    | Colon_term_dec of type_kwd_or_type_or_term
    | Equal_term_def of type_or_term_in_def
  and type_kwd_or_type_or_term =
    | No_type_kwd_or_type_or_term
    | Type_kwd_or_type_or_term of (type_or_term_kind*valuation)
  and type_or_term_in_def =
    | No_type_or_term_in_def
    | Type_or_term_in_def of (type_or_term_kind*valuation)
  and type_or_term_kind =
    | Nothing
    | Unset
    | Type
    | Term
  and valuation =
    | EOI
    | Sig_kwd
    | Id
    | Equal
    | Comma
    | Colon
    | Type_kwd
    | End_kwd
    | Semi_colon
    | Type_or_term of type_or_term_tokens
    | Prefix_kwd
    | Infix_kwd
    | Binder_kwd
    | Sym
  and type_or_term_tokens =
    | LPAR
    | RPAR
    | DOT
    | LAMBDA
    | ARROW

  type term = type_or_term_in_def


  let valuation_to_string = function
    | EOI -> "End of input"
    | Sig_kwd -> "\"signature\""
    | Id -> "Identifier"
    | Equal -> "\"=\""
    | Comma -> "\",\""
    | Colon -> "\":\""
    | Type_kwd -> "\"type\""
    | End_kwd -> "\"end\""
    | Semi_colon -> "\";\""
    | Type_or_term (LPAR|RPAR) -> "type or term"
    | Type_or_term (DOT|LAMBDA) -> "term"
    | Type_or_term ARROW -> "type"
    | Prefix_kwd -> "\"prefix\""
    | Infix_kwd -> "\"infix\""
    | Binder_kwd -> "\"binder\""
    | Sym -> "symbol"

  exception Expect of valuation list

  let start_data () = No_sig_dec

  let start_term () = No_type_or_term_in_def


  let term_expectation = function
    | No_type_or_term_in_def -> let l = [Id;Type_or_term LPAR;Sym] in
	l,(function
	     | (Id|Type_or_term (LPAR|RPAR)) as a -> Type_or_term_in_def (Unset,a)
	     | Type_or_term (DOT|LAMBDA) as a -> Type_or_term_in_def (Term,a)
	     | Type_or_term ARROW as a -> Type_or_term_in_def (Type,a)
	     | Sym as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Unset,(Id|Type_or_term(LPAR|RPAR))) -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Id as a -> Type_or_term_in_def (Unset,a)
	     | Type_or_term (LPAR|RPAR) as a  -> Type_or_term_in_def (Unset,a)
	     | Type_or_term DOT as a  -> Type_or_term_in_def (Term,a)
	     | Type_or_term LAMBDA as a  -> Type_or_term_in_def (Term,a)
	     | Type_or_term ARROW as a  -> Type_or_term_in_def (Type,a)
	     | Sym as a  -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Unset,_) -> failwith "Bug: should not occur"
    | Type_or_term_in_def (Type,Id) -> let l = [Type_or_term ARROW;EOI] in
	l,(function
	     | Id -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term (LPAR|RPAR) -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term DOT -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term LAMBDA -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term ARROW -> Type_or_term_in_def (Type,Type_or_term ARROW)
	     | Sym -> raise (Expect [Type_or_term DOT])
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Type,Type_or_term LPAR) -> let l = [Id;Type_or_term LPAR;EOI] in
	l,(function
	     | Id as a -> Type_or_term_in_def (Type,a)
	     | Type_or_term LPAR as a -> Type_or_term_in_def (Type,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Type,Type_or_term RPAR) -> let l = [Type_or_term ARROW;EOI] in
	l,(function
	     | Type_or_term RPAR as a -> Type_or_term_in_def (Type,a)
	     | Type_or_term ARROW as a -> Type_or_term_in_def (Type,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Type,Type_or_term ARROW) -> let l = [Id;Type_or_term LPAR] in
	l,(function
	     | Id as a -> Type_or_term_in_def (Type,a)
	     | Type_or_term LPAR as a -> Type_or_term_in_def (Type,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Type,_) -> failwith "Bug: should not occur"
    | Type_or_term_in_def (Term,Id) -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Type_or_term ARROW -> raise (Expect [Type_or_term DOT])
	     | (Id|Type_or_term _| Sym) as a -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Sym) -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Type_or_term RPAR -> raise (Expect l)
	     | Type_or_term ARROW -> raise (Expect [Type_or_term DOT])
	     | (Id|Type_or_term _|Sym) as a -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term DOT) -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Type_or_term ARROW -> raise (Expect [Type_or_term DOT])
	     | Type_or_term (RPAR|DOT) -> raise (Expect l)
	     | (Id|Type_or_term _|Sym) as a-> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term LAMBDA) -> let l = [Id] in
	l,(function
	     | Id as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term LPAR) -> let l = [Id;Type_or_term LPAR;Sym] in
	l,(function
	     | (Id|Sym| Type_or_term (LPAR|LAMBDA)) as a -> Type_or_term_in_def (Term,a)
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,Type_or_term RPAR) -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | (Id|Sym|Type_or_term (LPAR|RPAR|LAMBDA)) as a -> Type_or_term_in_def (Term,a)
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def (Term,_) -> failwith "Bug: should not occur"
    | Type_or_term_in_def (Nothing,_) -> failwith "Bug: Mothing should not appear in term"

  let term_transition q v =
    let _,result = term_expectation q in
      result v

  let data_expectation = function
    | No_sig_dec ->
	let l = [EOI;Sig_kwd] in
	  l , (function
		 | EOI -> No_sig_dec
		 | Sig_kwd -> Sig No_sig_dec_id
		 | _ -> raise (Expect l))
	    
    | Sig No_sig_dec_id -> let l = [Id] in
	l,(function
	     | Id -> Sig (Sig_dec_id No_sig_dec_equal)
	     | _ ->  raise (Expect l))

    | Sig (Sig_dec_id No_sig_dec_equal) -> let l = [Equal] in
	l,(function
	     | Equal -> Sig (Sig_dec_id (Sig_dec_equal (No_entry)))
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) -> let l = [Id;Prefix_kwd;Infix_kwd;Binder_kwd;End_kwd] in
	l,(function
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
	     | Prefix_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
	     | Infix_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
	     | Binder_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id)))
	     | End_kwd -> No_sig_dec
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))) -> let l = [Comma;Colon;Equal] in
	l,(function
	     | Comma -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list)))
	     | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
	     | Equal  -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
	     | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list))) -> let l = [Id] in
	l,(function
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
	     | _-> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) ->
	let l = [Type_kwd;Type_or_term ARROW;Id] in
	  l,(function
	       | Type_kwd as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Nothing,a))))))
	       | (Id|Type_or_term LPAR) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Type,a))))))
	       | _ -> raise (Expect [Type_kwd;Type_or_term ARROW;Id]))
	    
    |  Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) ->
	 let l = [Type_or_term LPAR;Id;Sym] in
	   l,(function
		| (Id|Type_or_term LPAR) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Unset,a))))))
		| (Sym|Type_or_term LAMBDA) as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		| _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t))))) ->
	let l = [Colon;Type_or_term LPAR;Id;Sym] in
	  l,(function
	       | Colon -> 
		   (match k_o_t with
		      | (Unset|Term|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
		      | Unset, _ -> raise (Expect [Id;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Id;Type_or_term ARROW])
		      | Term, _ -> raise (Expect [Id;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term RPAR as a-> 
		   (match k_o_t with
		      | (Unset|Term|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (fst k_o_t,a))))))
		      | Unset, _ -> raise (Expect [Id;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Id;Type_or_term ARROW])
		      | Term, _ -> raise (Expect [Id;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term LPAR as a-> 
		   (match k_o_t with
		      | (Unset|Term),(Id|Sym|Type_or_term (LPAR|DOT)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | (Unset|Term) as k,(Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (k,a))))))
		      | (Unset|Type),(Type_or_term ARROW) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | Unset, _ -> raise (Expect [Id;Sym;Type_or_term LPAR])
		      | Type, _ -> raise (Expect [Type_or_term ARROW])
		      | Term, _ -> raise (Expect [Id;Sym;Type_or_term DOT])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))		   
	       | Type_or_term DOT as a ->
		   (match k_o_t with
		      | Type,_ -> raise (Expect [Type_or_term ARROW])
		      | (Unset|Term),Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | (Unset|Term),_ -> raise (Expect [Id])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term LAMBDA as a-> 
		   (match k_o_t with
		      | Type,_ -> raise (Expect [Type_or_term ARROW])
		      | (Unset|Term),(Id|Sym|Type_or_term (LPAR|RPAR)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
		      | (Unset|Term),_ -> raise (Expect [Id;Sym;Type_or_term LAMBDA])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	       | Type_or_term ARROW as a -> 
		   (match k_o_t with
		      | Term,_ -> raise (Expect [Type_or_term DOT])
		      | (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Type,a))))))
		      | (Unset|Type),_ -> raise (Expect [Id;Type_or_term ARROW])
		      | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
| Id as a -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (fst k_o_t,a))))))
| Sym as a -> 
    (match k_o_t with
       | Type,_ -> raise (Expect [Type_or_term ARROW])
       | (Unset|Term),(Id|Sym|Type_or_term (LPAR|RPAR|DOT)) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def (Term,a))))))
       | (Unset|Term),_ -> raise (Expect [Id])
       | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
| _ -> raise (Expect l))
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol))) ->
	let l = [Sym] in
	  l,(function
	       | Sym -> Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol)))
	       | _ -> raise (Expect l))
	    
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol))) ->
	let l =[Colon;Equal] in
	  l,(function
	       | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
	       | Equal -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
	       | _ -> raise (Expect l))
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id))) ->
	let l = [Id] in
	  l,(function
	       | Id -> Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id)))
	       | _ ->  raise (Expect l))
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id))) ->
	let l = [Colon;Equal] in
	  l, (function 
		| Equal -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
		| Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
		| _ -> raise (Expect l))
	    
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t))))) -> 
    let l = [End_kwd;Semi_colon] in
      l,(function
	   | End_kwd -> 
	       (match k_o_t with
		  | Nothing,_ -> No_sig_dec
		  | (Unset|Type),(Id|Type_or_term RPAR) -> No_sig_dec
		  | _ -> raise (Expect [Id;Type_or_term ARROW]))
	   | Semi_colon -> 
	       (match k_o_t with
		  | Nothing,_ -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
		  | (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
		  | _ -> raise (Expect [Id;Type_or_term ARROW]))
	   | Id as a ->
	       (match k_o_t with
		  | (Unset|Type),Type_or_term (LPAR|ARROW) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (fst k_o_t,a))))))
		  | _ -> raise (Expect [Type_or_term ARROW]))
	   | Type_or_term LPAR as a ->
	       (match k_o_t with
		  | (Unset|Type),Type_or_term (LPAR|ARROW) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (fst k_o_t,a))))))
		  | _ -> raise (Expect [Type_or_term ARROW]))
	   | Type_or_term RPAR -> 
	       (match k_o_t with
		  | (Unset|Type),(Id|Type_or_term LPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t)))))
		  | _ -> raise (Expect [Type_or_term ARROW]))
	   | Type_or_term DOT -> raise (Expect [Type_or_term ARROW])
	       (*		   (match k_o_t with
				   | Type,_ -> raise (Expect [Type_or_term ARROW])
				   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
				   | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))*)
	   | Type_or_term LAMBDA -> raise (Expect [Type_or_term ARROW])
	       (*		   (match k_o_t with
				   | Type,_ -> raise (Expect [Type_or_term ARROW])
				   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
				   | Nothing,_ -> raise (Expect [End_kwd;Semi_colon])) *)
	   | Type_or_term ARROW as a-> 
	       (match k_o_t with
		  | Term,_ -> raise (Expect [Type_or_term DOT])
		  | (Unset|Type),(Id|Type_or_term RPAR) -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term (Type,a))))))
		  | (Unset|Type),_ -> raise (Expect [Id;Type_or_term ARROW])
		  | Nothing,_ -> raise (Expect [End_kwd;Semi_colon]))
	   | Sym -> raise (Expect [Type_or_term ARROW])
	       (*		   (match k_o_t with
				   | Type,_ -> raise (Expect [Type_or_term ARROW])
				   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
				   | Nothing,_ -> raise (Expect [End_kwd;Semi_colon])) *)
	   | _ -> raise (Expect l))


  let data_transition q v =
    let _,result = data_expectation q in
      result v


(*
  let data_transition q v = match q,v with
    | No_sig_dec , EOI ->
	No_sig_dec
    | No_sig_dec , Sig_kwd ->
	Sig No_sig_dec_id
    | No_sig_dec , _ -> 
	raise (Expect [Sig_kwd])

    | Sig No_sig_dec_id , Id ->
	Sig (Sig_dec_id No_sig_dec_equal)
    | Sig No_sig_dec_id , _ ->
	raise (Expect [Id])

    | Sig (Sig_dec_id No_sig_dec_equal) , Equal ->
	Sig (Sig_dec_id (Sig_dec_equal (No_entry)))
    | Sig (Sig_dec_id No_sig_dec_equal) , _ ->
	raise (Expect [Equal])

    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , Prefix_kwd ->
	Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , Infix_kwd ->
	Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol)))
    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , Binder_kwd -> 
	Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id)))
    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , End_kwd ->
	No_sig_dec
    | Sig (Sig_dec_id (Sig_dec_equal (No_entry))) , _ -> 
	raise (Expect [Id;Prefix_kwd;Infix_kwd;Binder_kwd;End_kwd])

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))), Comma->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list)))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))), Colon ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))), Equal  ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list))), _ ->
	raise (Expect [Comma;Colon])

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id No_entry_id_list)))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id Comma_in_id_list))) ,  _-> 
	raise (Expect [Id])

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) , Type_kwd ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) , Type_or_term ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term))))
(*	raise (Expect [Type_kwd]) *)
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term))))
(*	raise (Expect [Type_kwd]) *)
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) , Sym ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term)))) , _ ->
	raise (Expect [Type_kwd])
	  

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) , Type_or_term ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) , Sym ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) , _ ->
	raise (Expect [Type_or_term])


    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def)))) , Colon ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def)))) , Type_or_term ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def)))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def)))) , Sym ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def Type_or_term_in_def)))) , _ ->
	raise (Expect [Colon])
	  


    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol))), Sym -> 
	Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol)))
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix No_symbol))), _ ->
	raise (Expect [Sym])


    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol))) , Colon ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol))) , Equal ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Prefix_infix Symbol))) , _ ->
	raise (Expect [Colon;Equal])

    | Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id))) , Id ->
	Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id)))
    | Sig (Sig_dec_id (Sig_dec_equal (Binder No_binder_id))) , _ -> 
	raise (Expect [Id])

    | Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id))) , Equal ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def))))
    | Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id))) , Colon ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
    | Sig (Sig_dec_id (Sig_dec_equal (Binder Binder_id))) , _ ->
	raise (Expect [Colon;Equal])

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term)))), End_kwd ->
	No_sig_dec


    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term)))), Semi_colon ->
	Sig (Sig_dec_id (Sig_dec_equal No_entry))


    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term)))), (Type_or_term|Id|Sym) ->
	Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term))))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment Type_kwd_or_type_or_term)))), _ ->
	raise (Expect [End_kwd;Semi_colon])

*)


end
