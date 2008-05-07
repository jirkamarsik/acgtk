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
    | Type_kwd_or_type_or_term
  and type_or_term_in_def =
    | No_type_or_term_in_def
    | Type_or_term_in_def


  type valuation =
    | Sig_kwd
    | Id
    | Equal
    | Comma
    | Colon
    | Type_kwd
    | End_kwd
    | Semi_colon
    | Type_or_term
    | Prefix_kwd
    | Infix_kwd
    | Binder_kwd
    | Sym


  let valuation_to_string = function
    | Sig_kwd -> "\"signature\""
    | Id -> "Identifier"
    | Equal -> "\"=\""
    | Comma -> "\",\""
    | Colon -> "\":\""
    | Type_kwd -> "\"type\""
    | End_kwd -> "\"end\""
    | Semi_colon -> "\";\""
    | Type_or_term -> "type or term"
    | Prefix_kwd -> "\"prefix\""
    | Infix_kwd -> "\"Infix\""
    | Binder_kwd -> "\"binder\""
    | Sym -> "symbol"

  exception Expect of valuation list

  let start_data () = No_sig_dec

  let transition q v = match q,v with
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
	raise (Expect [Id;Prefix_kwd;Infix_kwd;Binder_kwd])

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



end
