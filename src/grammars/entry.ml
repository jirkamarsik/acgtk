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
  and id_in_id_list =
    | No_id_in_id_list
    | Id of entry_id_list
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
    | Type_kwd_or_type_or_term of type_or_term_kind
  and type_or_term_in_def =
    | No_type_or_term_in_def
    | Type_or_term_in_def of type_or_term_kind
  and type_or_term_kind =
    | Nothing
    | Unset
    | Type
    | Term

  type term = type_or_term_in_def

  type valuation =
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
(*  exception Unexpected_token of type_or_term_kind *)

  let start_data () = No_sig_dec

  let start_term () = No_type_or_term_in_def


(*  let term_transition q v = match q,v with
    | No_type_or_term_in_def , Id -> Type_or_term_in_def
    | No_type_or_term_in_def , Type_or_term -> Type_or_term_in_def
    | No_type_or_term_in_def , Sym -> Type_or_term_in_def
    | No_type_or_term_in_def , _ -> raise (Expect [Type_or_term])
    | Type_or_term_in_def , Id -> Type_or_term_in_def
    | type_or_term_in_def , Type_or_term -> Type_or_term_in_def
    | type_or_term_in_def , Sym -> Type_or_term_in_def
    | type_or_term_in_def , EOI -> No_type_or_term_in_def
    | type_or_term_in_def , _ -> raise (Expect [Type_or_term])
*)

  let term_expectation = function
    | No_type_or_term_in_def -> let l = [Id;Type_or_term LPAR;Sym] in
	l,(function
	     | Id -> Type_or_term_in_def Unset
	     | Type_or_term LPAR -> Type_or_term_in_def Unset
	     | Type_or_term RPAR -> Type_or_term_in_def Unset
	     | Type_or_term DOT -> Type_or_term_in_def Term
	     | Type_or_term LAMBDA -> Type_or_term_in_def Term
	     | Type_or_term ARROW -> Type_or_term_in_def Type
	     | Sym -> Type_or_term_in_def Term
	     | _ -> raise (Expect l))
    | Type_or_term_in_def Unset -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Id -> Type_or_term_in_def Unset
	     | Type_or_term (LPAR|RPAR) -> Type_or_term_in_def Unset
	     | Type_or_term DOT -> Type_or_term_in_def Term
	     | Type_or_term LAMBDA -> Type_or_term_in_def Term
	     | Type_or_term ARROW -> Type_or_term_in_def Type
	     | Sym -> Type_or_term_in_def Term
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def Type -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Id -> Type_or_term_in_def Type
	     | Type_or_term (LPAR|RPAR) -> Type_or_term_in_def Type
	     | Type_or_term DOT -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term LAMBDA -> raise (Expect [Type_or_term ARROW])
	     | Type_or_term ARROW -> Type_or_term_in_def Type
	     | Sym -> raise (Expect [Type_or_term DOT])
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def Term -> let l = [Id;Type_or_term LPAR;Sym;EOI] in
	l,(function
	     | Id -> Type_or_term_in_def Term
	     | Type_or_term (LPAR|RPAR) -> Type_or_term_in_def Term
	     | Type_or_term DOT -> Type_or_term_in_def Term
	     | Type_or_term LAMBDA -> Type_or_term_in_def Term
	     | Type_or_term ARROW -> raise (Expect [Type_or_term DOT])
	     | Sym -> Type_or_term_in_def Term
	     | EOI -> No_type_or_term_in_def
	     | _ -> raise (Expect l))
    | Type_or_term_in_def Nothing -> failwith "Bug: Mothing should not appear in term"

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
	let l = [Type_kwd;Type_or_term LPAR;Id;Sym] in
	  l,(function
	     | Type_kwd -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Nothing)))))
	     | Type_or_term LPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Unset)))))
	     | Type_or_term RPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Unset)))))
	     | Type_or_term DOT -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
	     | Type_or_term LAMBDA -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
	     | Type_or_term ARROW -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Type)))))
	     | Sym -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Unset)))))
	     | _ -> raise (Expect l))
	  
    |  Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def No_type_or_term_in_def)))) ->
	 let l = [Type_or_term LPAR;Id;Sym] in
	   l,(function
	      | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Unset)))))
	      | Type_or_term LPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Unset)))))
	      | Type_or_term RPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Unset)))))
	      | Type_or_term DOT -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
	      | Type_or_term LAMBDA -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
	      | Type_or_term ARROW -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Type)))))
	      | Sym -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
	      | _ -> raise (Expect l))

    | Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t))))) ->
	let l = [Colon;Type_or_term LPAR;Id;Sym] in
	  l,(function
	     | Colon -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment No_type_kwd_or_type_or_term))))
	     | Type_or_term LPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t)))))
	     | Type_or_term RPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t)))))
	     | Type_or_term DOT ->
		 (match k_o_t with
		    | Type -> raise (Expect [Type_or_term ARROW])
		    | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Type_or_term LAMBDA-> 
		 (match k_o_t with
		    | Type -> raise (Expect [Type_or_term ARROW])
		    | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Type_or_term ARROW -> 
		 (match k_o_t with
		    | Term -> raise (Expect [Type_or_term DOT])
		    | Unset|Type -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Type)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def k_o_t)))))
	     | Sym -> 
		 (match k_o_t with
		    | Type -> raise (Expect [Type_or_term ARROW])
		    | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Equal_def (Type_or_term_in_def Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
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
	     | End_kwd -> No_sig_dec
	     | Semi_colon -> Sig (Sig_dec_id (Sig_dec_equal No_entry))
	     | Id -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t)))))
	     | Type_or_term LPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t)))))
	     | Type_or_term RPAR -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term k_o_t)))))
	     | Type_or_term DOT -> 
		 (match k_o_t with
		   | Type -> raise (Expect [Type_or_term ARROW])
		   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Type_or_term LAMBDA -> 
		 (match k_o_t with
		   | Type -> raise (Expect [Type_or_term ARROW])
		   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Type_or_term ARROW -> 
		 (match k_o_t with
		    | Term -> raise (Expect [Type_or_term DOT])
		    | Unset|Type -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Type)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
	     | Sym -> 
		 (match k_o_t with
		   | Type -> raise (Expect [Type_or_term ARROW])
		   | Unset|Term -> Sig (Sig_dec_id (Sig_dec_equal (Entry_id (Colon_assignment (Type_kwd_or_type_or_term Term)))))
		    | Nothing -> raise (Expect [End_kwd;Semi_colon]))
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
