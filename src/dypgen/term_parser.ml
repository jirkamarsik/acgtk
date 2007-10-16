type token =
  | SYMBOL of (string*(Abstract_syntax.Abstract_sig.location))
  | IDENT of (string*(Abstract_syntax.Abstract_sig.location))
  | LIN_ARROW of (Abstract_syntax.Abstract_sig.location)
  | ARROW of (Abstract_syntax.Abstract_sig.location)
  | LAMBDA0 of (Abstract_syntax.Abstract_sig.location)
  | LAMBDA of (Abstract_syntax.Abstract_sig.location)
  | BINDER of (Abstract_syntax.Abstract_sig.location)
  | INFIX of (Abstract_syntax.Abstract_sig.location)
  | PREFIX of (Abstract_syntax.Abstract_sig.location)
  | TYPE of (Abstract_syntax.Abstract_sig.location)
  | END_OF_DEC of (Abstract_syntax.Abstract_sig.location)
  | SIG_OPEN of (Abstract_syntax.Abstract_sig.location)
  | DOT of (Abstract_syntax.Abstract_sig.location)
  | RPAREN of (Abstract_syntax.Abstract_sig.location)
  | LPAREN of (Abstract_syntax.Abstract_sig.location)
  | COMMA of (Abstract_syntax.Abstract_sig.location)
  | COLON of (Abstract_syntax.Abstract_sig.location)
  | SEMICOLON of (Abstract_syntax.Abstract_sig.location)
  | EQUAL of (Abstract_syntax.Abstract_sig.location)
  | EOI

module Dyp_symbols =
struct
  let atomic_type = 1
  let comma_ids = 2
  let idents = 3
  let lambda = 4
  let sig_entries = 5
  let sig_entry = 6
  let signature = 7
  let term = 8
  let term_dec_start = 9
  let term_declaration = 10
  let term_def_start = 11
  let term_definition = 12
  let type_declaration = 13
  let type_definition = 14
  let type_expression = 15
  let t_SYMBOL = 2
  let t_IDENT = 3
  let t_LIN_ARROW = 4
  let t_ARROW = 5
  let t_LAMBDA0 = 6
  let t_LAMBDA = 7
  let t_BINDER = 8
  let t_INFIX = 9
  let t_PREFIX = 10
  let t_TYPE = 11
  let t_END_OF_DEC = 12
  let t_SIG_OPEN = 13
  let t_DOT = 14
  let t_RPAREN = 15
  let t_LPAREN = 16
  let t_COMMA = 17
  let t_COLON = 18
  let t_SEMICOLON = 19
  let t_EQUAL = 20
  let t_EOI = 21
  let get_token_name t = match t with
    | SYMBOL _ -> t_SYMBOL
    | IDENT _ -> t_IDENT
    | LIN_ARROW _ -> t_LIN_ARROW
    | ARROW _ -> t_ARROW
    | LAMBDA0 _ -> t_LAMBDA0
    | LAMBDA _ -> t_LAMBDA
    | BINDER _ -> t_BINDER
    | INFIX _ -> t_INFIX
    | PREFIX _ -> t_PREFIX
    | TYPE _ -> t_TYPE
    | END_OF_DEC _ -> t_END_OF_DEC
    | SIG_OPEN _ -> t_SIG_OPEN
    | DOT _ -> t_DOT
    | RPAREN _ -> t_RPAREN
    | LPAREN _ -> t_LPAREN
    | COMMA _ -> t_COMMA
    | COLON _ -> t_COLON
    | SEMICOLON _ -> t_SEMICOLON
    | EQUAL _ -> t_EQUAL
    | EOI -> t_EOI
  let str_token t = match t with
    | SYMBOL _ -> "SYMBOL"
    | IDENT _ -> "IDENT"
    | LIN_ARROW _ -> "LIN_ARROW"
    | ARROW _ -> "ARROW"
    | LAMBDA0 _ -> "LAMBDA0"
    | LAMBDA _ -> "LAMBDA"
    | BINDER _ -> "BINDER"
    | INFIX _ -> "INFIX"
    | PREFIX _ -> "PREFIX"
    | TYPE _ -> "TYPE"
    | END_OF_DEC _ -> "END_OF_DEC"
    | SIG_OPEN _ -> "SIG_OPEN"
    | DOT _ -> "DOT"
    | RPAREN _ -> "RPAREN"
    | LPAREN _ -> "LPAREN"
    | COMMA _ -> "COMMA"
    | COLON _ -> "COLON"
    | SEMICOLON _ -> "SEMICOLON"
    | EQUAL _ -> "EQUAL"
    | EOI -> "EOI"
end

type ('atomic_type,'comma_ids,'idents,'lambda,'sig_entries,'sig_entry,'term,'term_dec_start,'term_declaration,'term_def_start,'term_definition,'type_declaration,'type_definition,'type_expression) obj =
  | Obj_ARROW of (Abstract_syntax.Abstract_sig.location)
  | Obj_BINDER of (Abstract_syntax.Abstract_sig.location)
  | Obj_COLON of (Abstract_syntax.Abstract_sig.location)
  | Obj_COMMA of (Abstract_syntax.Abstract_sig.location)
  | Obj_DOT of (Abstract_syntax.Abstract_sig.location)
  | Obj_END_OF_DEC of (Abstract_syntax.Abstract_sig.location)
  | Obj_EOI
  | Obj_EQUAL of (Abstract_syntax.Abstract_sig.location)
  | Obj_IDENT of (string*(Abstract_syntax.Abstract_sig.location))
  | Obj_INFIX of (Abstract_syntax.Abstract_sig.location)
  | Obj_LAMBDA of (Abstract_syntax.Abstract_sig.location)
  | Obj_LAMBDA0 of (Abstract_syntax.Abstract_sig.location)
  | Obj_LIN_ARROW of (Abstract_syntax.Abstract_sig.location)
  | Obj_LPAREN of (Abstract_syntax.Abstract_sig.location)
  | Obj_PREFIX of (Abstract_syntax.Abstract_sig.location)
  | Obj_RPAREN of (Abstract_syntax.Abstract_sig.location)
  | Obj_SEMICOLON of (Abstract_syntax.Abstract_sig.location)
  | Obj_SIG_OPEN of (Abstract_syntax.Abstract_sig.location)
  | Obj_SYMBOL of (string*(Abstract_syntax.Abstract_sig.location))
  | Obj_TYPE of (Abstract_syntax.Abstract_sig.location)
  | Obj_atomic_type of 'atomic_type
  | Obj_comma_ids of 'comma_ids
  | Obj_idents of 'idents
  | Obj_lambda of 'lambda
  | Obj_sig_entries of 'sig_entries
  | Obj_sig_entry of 'sig_entry
  | Obj_signature of (Abstract_syntax.Abstract_sig.t)
  | Obj_term of 'term
  | Obj_term_dec_start of 'term_dec_start
  | Obj_term_declaration of 'term_declaration
  | Obj_term_def_start of 'term_def_start
  | Obj_term_definition of 'term_definition
  | Obj_type_declaration of 'type_declaration
  | Obj_type_definition of 'type_definition
  | Obj_type_expression of 'type_expression

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "atomic_type";
    "comma_ids";
    "idents";
    "lambda";
    "sig_entries";
    "sig_entry";
    "signature";
    "term";
    "term_dec_start";
    "term_declaration";
    "term_def_start";
    "term_definition";
    "type_declaration";
    "type_definition";
    "type_expression";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_signature";"SYMBOL";"IDENT";"LIN_ARROW";"ARROW";"LAMBDA0";"LAMBDA";"BINDER";"INFIX";"PREFIX";"TYPE";"END_OF_DEC";"SIG_OPEN";"DOT";"RPAREN";"LPAREN";"COMMA";"COLON";"SEMICOLON";"EQUAL";"EOI"|]
  let test_cons =  [|
    (fun x -> match x with Obj_atomic_type _ -> true | _ -> false);
    (fun x -> match x with Obj_comma_ids _ -> true | _ -> false);
    (fun x -> match x with Obj_idents _ -> true | _ -> false);
    (fun x -> match x with Obj_lambda _ -> true | _ -> false);
    (fun x -> match x with Obj_sig_entries _ -> true | _ -> false);
    (fun x -> match x with Obj_sig_entry _ -> true | _ -> false);
    (fun x -> match x with Obj_signature _ -> true | _ -> false);
    (fun x -> match x with Obj_term _ -> true | _ -> false);
    (fun x -> match x with Obj_term_dec_start _ -> true | _ -> false);
    (fun x -> match x with Obj_term_declaration _ -> true | _ -> false);
    (fun x -> match x with Obj_term_def_start _ -> true | _ -> false);
    (fun x -> match x with Obj_term_definition _ -> true | _ -> false);
    (fun x -> match x with Obj_type_declaration _ -> true | _ -> false);
    (fun x -> match x with Obj_type_definition _ -> true | _ -> false);
    (fun x -> match x with Obj_type_expression _ -> true | _ -> false)|]
  let cons_of_nt =
  [|0;
    0;
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14|]
  let str_cons o = match o with
    | Obj_atomic_type _ -> "Obj_atomic_type"
    | Obj_comma_ids _ -> "Obj_comma_ids"
    | Obj_idents _ -> "Obj_idents"
    | Obj_lambda _ -> "Obj_lambda"
    | Obj_sig_entries _ -> "Obj_sig_entries"
    | Obj_sig_entry _ -> "Obj_sig_entry"
    | Obj_signature _ -> "Obj_signature"
    | Obj_term _ -> "Obj_term"
    | Obj_term_dec_start _ -> "Obj_term_dec_start"
    | Obj_term_declaration _ -> "Obj_term_declaration"
    | Obj_term_def_start _ -> "Obj_term_def_start"
    | Obj_term_definition _ -> "Obj_term_definition"
    | Obj_type_declaration _ -> "Obj_type_declaration"
    | Obj_type_definition _ -> "Obj_type_definition"
    | Obj_type_expression _ -> "Obj_type_expression"
    | _ -> failwith "str_cons, unexpected constructor"
end

module Dyp_parameters =
struct
  let token_nb = 22
  let undef_nt = true
  let entry_points = [(Dyp_symbols.signature,1)]
  let str_token_name t = Dyp_symbols_array.token_name_array.(t)
  let priority_names = [|"default_priority";"app";"arrow_type";"atom";"atom_type";"binder"|]
  let merge_warning = false
end

module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)
module Dyp_engine = Dyp_runtime.Parser_PIA

module Dyp_aux_functions =
struct
  let datadyn = Dyp_runtime.Tools.init_datadyn
["atomic_type",0,"Obj_atomic_type";"comma_ids",1,"Obj_comma_ids";"idents",2,"Obj_idents";"lambda",3,"Obj_lambda";"sig_entries",4,"Obj_sig_entries";"sig_entry",5,"Obj_sig_entry";"signature",6,"Obj_signature";"term",7,"Obj_term";"term_dec_start",8,"Obj_term_dec_start";"term_declaration",9,"Obj_term_declaration";"term_def_start",10,"Obj_term_def_start";"term_definition",11,"Obj_term_definition";"type_declaration",12,"Obj_type_declaration";"type_definition",13,"Obj_type_definition";"type_expression",14,"Obj_type_expression"]
["Obj_atomic_type";"Obj_comma_ids";"Obj_idents";"Obj_lambda";"Obj_sig_entries";"Obj_sig_entry";"Obj_signature";"Obj_term";"Obj_term_dec_start";"Obj_term_declaration";"Obj_term_def_start";"Obj_term_definition";"Obj_type_declaration";"Obj_type_definition";"Obj_type_expression"]
  let get_token_value t = match t with
    | SYMBOL x -> Obj_SYMBOL x
    | IDENT x -> Obj_IDENT x
    | LIN_ARROW x -> Obj_LIN_ARROW x
    | ARROW x -> Obj_ARROW x
    | LAMBDA0 x -> Obj_LAMBDA0 x
    | LAMBDA x -> Obj_LAMBDA x
    | BINDER x -> Obj_BINDER x
    | INFIX x -> Obj_INFIX x
    | PREFIX x -> Obj_PREFIX x
    | TYPE x -> Obj_TYPE x
    | END_OF_DEC x -> Obj_END_OF_DEC x
    | SIG_OPEN x -> Obj_SIG_OPEN x
    | DOT x -> Obj_DOT x
    | RPAREN x -> Obj_RPAREN x
    | LPAREN x -> Obj_LPAREN x
    | COMMA x -> Obj_COMMA x
    | COLON x -> Obj_COLON x
    | SEMICOLON x -> Obj_SEMICOLON x
    | EQUAL x -> Obj_EQUAL x
    | EOI -> Obj_EOI
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  let transform_av_list l =
    let f o = match o with
      | Obj_EOI -> `Dummy_obj
      | x -> `Real_obj x
    in
    List.map f l
end

module Dyp_priority_data =
struct
  let priority_data, default_priority =
    Dyp.insert_priority Dyp.empty_priority_data "default_priority"
  let priority_data, app = Dyp.insert_priority priority_data "app"
  let priority_data, arrow_type = Dyp.insert_priority priority_data "arrow_type"
  let priority_data, atom = Dyp.insert_priority priority_data "atom"
  let priority_data, atom_type = Dyp.insert_priority priority_data "atom_type"
  let priority_data, binder = Dyp.insert_priority priority_data "binder"
  let priority_data = Dyp.add_list_relations priority_data [atom_type;arrow_type]
  let priority_data = Dyp.add_list_relations priority_data [atom;app;binder]
end

let global_data = ref 0
let local_data = ref 0
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_atomic_type _ _ = []
let dyp_merge_comma_ids _ _ = []
let dyp_merge_idents _ _ = []
let dyp_merge_lambda _ _ = []
let dyp_merge_sig_entries _ _ = []
let dyp_merge_sig_entry _ _ = []
let dyp_merge_signature _ _ = []
let dyp_merge_term _ _ = []
let dyp_merge_term_dec_start _ _ = []
let dyp_merge_term_declaration _ _ = []
let dyp_merge_term_def_start _ _ = []
let dyp_merge_term_definition _ _ = []
let dyp_merge_type_declaration _ _ = []
let dyp_merge_type_definition _ _ = []
let dyp_merge_type_expression _ _ = []
let dyp_merge = Dyp.keep_oldest

# 1 "term_parser.dyp"

  open Dyp
  open Abstract_syntax

  module Env = Set.Make(String)

  let pr s = Printf.fprintf stderr "%s\n%!" s

  let abs x l t = function
    | Abstract_sig.Linear -> Abstract_sig.LAbs (x,t,l)
(*    | Abstract_sig.Non_linear -> Abstract_sig.Abs (x,l,t) *)

  let rec multiple_abs e k_a l ids t k =
    match ids with
      | [] -> k (t e)
      | [a,l_a] -> k (abs a l_a (t (Env.add a e)) k_a)
      | (a,l_a)::((_,l_b)::_ as tl) -> 
	  let new_env = Env.add a e in
	    multiple_abs new_env k_a l_b tl t (fun r -> abs a l r k_a)

  let parse_error e = raise (Error.Error (Error.Parse_error e))


# 322                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.signature,[Dyp.Ter Dyp_symbols.t_SIG_OPEN;Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_END_OF_DEC],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SIG_OPEN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 328                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 332                "term_parser.ml"
 as _2));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 336                "term_parser.ml"
 as _3));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 340                "term_parser.ml"
 as _4)));`Real_obj (Obj_END_OF_DEC  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 344                "term_parser.ml"
 as _5))] -> Obj_signature 
# 54 "term_parser.dyp"
(
                                              (_4 (Abstract_sig.empty (fst _2))):Abstract_syntax.Abstract_sig.t)
# 349                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.signature,[Dyp.Ter Dyp_symbols.t_SIG_OPEN;Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Ter Dyp_symbols.t_END_OF_DEC],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SIG_OPEN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 355                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 359                "term_parser.ml"
 as _2));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 363                "term_parser.ml"
 as _3));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 367                "term_parser.ml"
 as _4)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 371                "term_parser.ml"
 as _5));`Real_obj (Obj_END_OF_DEC  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 375                "term_parser.ml"
 as _6))] -> Obj_signature 
# 55 "term_parser.dyp"
(
                                                        (_4 (Abstract_sig.empty (fst _2))):Abstract_syntax.Abstract_sig.t)
# 380                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 386                "term_parser.ml"
 as _1)))] -> Obj_sig_entries 
# 58 "term_parser.dyp"
(
            (fun s -> _1 s):'dypgen__Obj_sig_entries)
# 391                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 397                "term_parser.ml"
 as _1)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 401                "term_parser.ml"
 as _2));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 405                "term_parser.ml"
 as _3)))] -> Obj_sig_entries 
# 59 "term_parser.dyp"
(
                                  (fun s -> _3 (_1 s)):'dypgen__Obj_sig_entries)
# 410                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_declaration)
# 416                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 62 "term_parser.dyp"
(
                   (_1):'dypgen__Obj_sig_entry)
# 421                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_definition)
# 427                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 63 "term_parser.dyp"
(
                  (_1):'dypgen__Obj_sig_entry)
# 432                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.term_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_declaration)
# 438                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 64 "term_parser.dyp"
(
                   (_1):'dypgen__Obj_sig_entry)
# 443                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.term_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_definition)
# 449                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 65 "term_parser.dyp"
(
                  (_1):'dypgen__Obj_sig_entry)
# 454                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.type_declaration,[Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Ter Dyp_symbols.t_TYPE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 460                "term_parser.ml"
 as _1)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 464                "term_parser.ml"
 as _2));`Real_obj (Obj_TYPE  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 468                "term_parser.ml"
 as _3))] -> Obj_type_declaration 
# 68 "term_parser.dyp"
(
                       (fun s -> 
			  List.fold_left
			    (fun acc id ->
			       try
				 Abstract_sig.add_entry (Abstract_sig.Type_decl (fst id,snd id,Abstract_sig.K [])) acc
			       with
				 | Abstract_sig.Duplicate_type_definition -> 
					      let pos1,pos2= snd id in
					      parse_error (Error.Duplicated_type (fst id,pos1,pos2)))
			    s
			    _1):'dypgen__Obj_type_declaration)
# 483                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.comma_ids,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 489                "term_parser.ml"
 as _1))] -> Obj_comma_ids 
# 81 "term_parser.dyp"
(
        ([_1]):'dypgen__Obj_comma_ids)
# 494                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.comma_ids,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_COMMA;Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 500                "term_parser.ml"
 as _1));`Real_obj (Obj_COMMA  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 504                "term_parser.ml"
 as _2));`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 508                "term_parser.ml"
 as _3)))] -> Obj_comma_ids 
# 82 "term_parser.dyp"
(
                        (_1::_3):'dypgen__Obj_comma_ids)
# 513                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.type_definition,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Ter Dyp_symbols.t_TYPE],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 519                "term_parser.ml"
 as _1));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 523                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 527                "term_parser.ml"
 as _3)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 531                "term_parser.ml"
 as _4));`Real_obj (Obj_TYPE  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 535                "term_parser.ml"
 as _5))] -> Obj_type_definition 
# 85 "term_parser.dyp"
(
                                         (fun s ->
					    try
					      Abstract_sig.add_entry (Abstract_sig.Type_def (fst _1,snd _1,fst _3)) s
					    with
					      |Abstract_sig.Duplicate_type_definition -> 
					      let pos1,pos2= snd _1 in
						parse_error (Error.Duplicated_type (fst _1,pos1,pos2))):'dypgen__Obj_type_definition)
# 546                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.type_expression,[Dyp.Non_ter (Dyp_symbols.atomic_type,Dyp.No_priority )],Dyp_priority_data.atom_type),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_atomic_type ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_atomic_type)
# 552                "term_parser.ml"
 as _1)))] -> Obj_type_expression 
# 94 "term_parser.dyp"
(
              (_1):'dypgen__Obj_type_expression)
# 557                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.type_expression,[Dyp.Non_ter (Dyp_symbols.atomic_type,Dyp.Lesseq_priority Dyp_priority_data.atom_type);Dyp.Ter Dyp_symbols.t_LIN_ARROW;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.Lesseq_priority Dyp_priority_data.arrow_type)],Dyp_priority_data.arrow_type),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_atomic_type ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_atomic_type)
# 563                "term_parser.ml"
 as _1)));`Real_obj (Obj_LIN_ARROW  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 567                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 571                "term_parser.ml"
 as _3)))] -> Obj_type_expression 
# 95 "term_parser.dyp"
(
                                                                       (let new_loc = Abstract_sig.new_loc (snd _1) (snd _3) in Abstract_sig.Linear_arrow (fst _1,fst _3,new_loc),new_loc):'dypgen__Obj_type_expression)
# 576                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.atomic_type,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom_type),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 582                "term_parser.ml"
 as _1))] -> Obj_atomic_type 
# 98 "term_parser.dyp"
(
        (Abstract_sig.Type_atom (fst _1,snd _1,[]),snd _1):'dypgen__Obj_atomic_type)
# 587                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.atomic_type,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom_type),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 593                "term_parser.ml"
 as _1));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 597                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 601                "term_parser.ml"
 as _3))] -> Obj_atomic_type 
# 99 "term_parser.dyp"
(
                                (fst _2,Abstract_sig.new_loc _1 _3):'dypgen__Obj_atomic_type)
# 606                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_declaration,[Dyp.Non_ter (Dyp_symbols.term_dec_start,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_dec_start ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_dec_start)
# 612                "term_parser.ml"
 as _1)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 616                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 620                "term_parser.ml"
 as _3)))] -> Obj_term_declaration 
# 102 "term_parser.dyp"
(
                                        (fun s ->
					   List.fold_left
					     (fun acc (id,kind,loc) -> 
						try
						  Abstract_sig.add_entry (Abstract_sig.Term_decl (id,kind,loc,fst _3)) acc
						with
						  | Abstract_sig.Duplicate_term_definition -> 
						      let pos1,pos2= loc in
							parse_error (Error.Duplicated_term (id,pos1,pos2)))
					     s
					     _1):'dypgen__Obj_term_declaration)
# 635                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_dec_start,[Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 641                "term_parser.ml"
 as _1)))] -> Obj_term_dec_start 
# 115 "term_parser.dyp"
(
            (List.map (fun (id,loc) -> (id,Abstract_sig.Default,loc)) _1):'dypgen__Obj_term_dec_start)
# 646                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_PREFIX;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_PREFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 652                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 656                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 116 "term_parser.dyp"
(
               ([fst _2,Abstract_sig.Prefix,snd _2]):'dypgen__Obj_term_dec_start)
# 661                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_INFIX;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_INFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 667                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 671                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 117 "term_parser.dyp"
(
              ([fst _2,Abstract_sig.Infix,snd _2]):'dypgen__Obj_term_dec_start)
# 676                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_BINDER;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_BINDER  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 682                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 686                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 118 "term_parser.dyp"
(
               ([fst _2,Abstract_sig.Binder,snd _2]):'dypgen__Obj_term_dec_start)
# 691                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 697                "term_parser.ml"
 as _1))] -> Obj_term_def_start 
# 121 "term_parser.dyp"
(
        (fst _1,Abstract_sig.Default,snd _1):'dypgen__Obj_term_def_start)
# 702                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_PREFIX;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_PREFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 708                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 712                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 122 "term_parser.dyp"
(
               (fst _2,Abstract_sig.Prefix,snd _2):'dypgen__Obj_term_def_start)
# 717                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_INFIX;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_INFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 723                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 727                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 123 "term_parser.dyp"
(
              (fst _2,Abstract_sig.Infix,snd _2):'dypgen__Obj_term_def_start)
# 732                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_BINDER;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_BINDER  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 738                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 742                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 124 "term_parser.dyp"
(
               (fst _2,Abstract_sig.Binder,snd _2):'dypgen__Obj_term_def_start)
# 747                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_definition,[Dyp.Non_ter (Dyp_symbols.term_def_start,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_def_start ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_def_start)
# 753                "term_parser.ml"
 as _1)));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 757                "term_parser.ml"
 as _2));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 761                "term_parser.ml"
 as _3)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 765                "term_parser.ml"
 as _4));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 769                "term_parser.ml"
 as _5)))] -> Obj_term_definition 
# 127 "term_parser.dyp"
(
                                                  (
    let id,k,l = _1 in
      fun s -> Abstract_sig.add_entry (Abstract_sig.Term_def (id,k,l,_3 Env.empty,fst _5)) s):'dypgen__Obj_term_definition)
# 776                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LAMBDA0;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.binder),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 782                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 786                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 790                "term_parser.ml"
 as _3));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 794                "term_parser.ml"
 as _4)))] -> Obj_term 
# 133 "term_parser.dyp"
(
                          (fun env -> multiple_abs env Abstract_sig.Linear _1 _2 _4 (fun x -> x)):'dypgen__Obj_term)
# 799                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 805                "term_parser.ml"
 as _1))] -> Obj_term 
# 135 "term_parser.dyp"
(
        (let id,l=_1 in
	   fun env -> 
	     match Env.mem id env with
	       | true -> Abstract_sig.Var (id,l)
	       | false -> Abstract_sig.Const (id,l)):'dypgen__Obj_term)
# 814                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 820                "term_parser.ml"
 as _1));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 824                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 828                "term_parser.ml"
 as _3))] -> Obj_term 
# 140 "term_parser.dyp"
(
                     (_2):'dypgen__Obj_term)
# 833                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.atom)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 839                "term_parser.ml"
 as _1)));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 843                "term_parser.ml"
 as _2)))] -> Obj_term 
# 141 "term_parser.dyp"
(
                           (fun e ->
			      let u1 = _1 e in
			      let u2 = _2 e in
				Abstract_sig.App(u1,u2,Abstract_sig.new_loc (Abstract_sig.get_term_location u1) (Abstract_sig.get_term_location u2))):'dypgen__Obj_term)
# 851                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 857                "term_parser.ml"
 as _1))] -> Obj_idents 
# 147 "term_parser.dyp"
(
        ([_1]):'dypgen__Obj_idents)
# 862                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abstract_syntax.Abstract_sig.location))
# 868                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 872                "term_parser.ml"
 as _2)))] -> Obj_idents 
# 148 "term_parser.dyp"
(
               (_1::_2):'dypgen__Obj_idents)
# 877                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 883                "term_parser.ml"
 as _1))] -> Obj_lambda 
# 151 "term_parser.dyp"
(
          (Abstract_sig.Linear):'dypgen__Obj_lambda)
# 888                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_atomic_type ol o =
  let ol2 = dyp_merge_atomic_type ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_comma_ids ol o =
  let ol2 = dyp_merge_comma_ids ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_idents ol o =
  let ol2 = dyp_merge_idents ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lambda ol o =
  let ol2 = dyp_merge_lambda ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_sig_entries ol o =
  let ol2 = dyp_merge_sig_entries ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_sig_entry ol o =
  let ol2 = dyp_merge_sig_entry ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_signature ol o =
  let ol2 = dyp_merge_signature ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_term ol o =
  let ol2 = dyp_merge_term ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_term_dec_start ol o =
  let ol2 = dyp_merge_term_dec_start ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_term_declaration ol o =
  let ol2 = dyp_merge_term_declaration ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_term_def_start ol o =
  let ol2 = dyp_merge_term_def_start ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_term_definition ol o =
  let ol2 = dyp_merge_term_definition ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_type_declaration ol o =
  let ol2 = dyp_merge_type_declaration ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_type_definition ol o =
  let ol2 = dyp_merge_type_definition ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_type_expression ol o =
  let ol2 = dyp_merge_type_expression ol o in
  if ol2 = [] then dyp_merge ol o else ol2

let __dypgen_merge_map = Dyp_runtime.Tools.init_merge_map [(fun ol o -> (
  let f1 o = match o with Obj_type_expression ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_type_expression"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_type_expression ol o in
  let f2 o = Obj_type_expression o in
  List.map f2 ol)),15;(fun ol o -> (
  let f1 o = match o with Obj_type_definition ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_type_definition"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_type_definition ol o in
  let f2 o = Obj_type_definition o in
  List.map f2 ol)),14;(fun ol o -> (
  let f1 o = match o with Obj_type_declaration ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_type_declaration"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_type_declaration ol o in
  let f2 o = Obj_type_declaration o in
  List.map f2 ol)),13;(fun ol o -> (
  let f1 o = match o with Obj_term_definition ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_definition"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_definition ol o in
  let f2 o = Obj_term_definition o in
  List.map f2 ol)),12;(fun ol o -> (
  let f1 o = match o with Obj_term_def_start ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_def_start"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_def_start ol o in
  let f2 o = Obj_term_def_start o in
  List.map f2 ol)),11;(fun ol o -> (
  let f1 o = match o with Obj_term_declaration ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_declaration"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_declaration ol o in
  let f2 o = Obj_term_declaration o in
  List.map f2 ol)),10;(fun ol o -> (
  let f1 o = match o with Obj_term_dec_start ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_dec_start"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_dec_start ol o in
  let f2 o = Obj_term_dec_start o in
  List.map f2 ol)),9;(fun ol o -> (
  let f1 o = match o with Obj_term ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term ol o in
  let f2 o = Obj_term o in
  List.map f2 ol)),8;(fun ol o -> (
  let f1 o = match o with Obj_signature ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_signature"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_signature ol o in
  let f2 o = Obj_signature o in
  List.map f2 ol)),7;(fun ol o -> (
  let f1 o = match o with Obj_sig_entry ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_sig_entry"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_sig_entry ol o in
  let f2 o = Obj_sig_entry o in
  List.map f2 ol)),6;(fun ol o -> (
  let f1 o = match o with Obj_sig_entries ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_sig_entries"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_sig_entries ol o in
  let f2 o = Obj_sig_entries o in
  List.map f2 ol)),5;(fun ol o -> (
  let f1 o = match o with Obj_lambda ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lambda"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lambda ol o in
  let f2 o = Obj_lambda o in
  List.map f2 ol)),4;(fun ol o -> (
  let f1 o = match o with Obj_idents ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_idents"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_idents ol o in
  let f2 o = Obj_idents o in
  List.map f2 ol)),3;(fun ol o -> (
  let f1 o = match o with Obj_comma_ids ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_comma_ids"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_comma_ids ol o in
  let f2 o = Obj_comma_ids o in
  List.map f2 ol)),2;(fun ol o -> (
  let f1 o = match o with Obj_atomic_type ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_atomic_type"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_atomic_type ol o in
  let f2 o = Obj_atomic_type o in
  List.map f2 ol)),1]

let __dypgen_automaton = Dyp_engine.create_parsing_device __dypgen_ra_list Dyp_priority_data.priority_data `LR0 !global_data !local_data __dypgen_merge_map dyp_merge Dyp_aux_functions.datadyn Dyp_symbols_array.str_non_ter Dyp_symbols_array.cons_of_nt

let __dypgen_data_equal = {
  Dyp_runtime.Tools.global_data_equal = global_data_equal;
  Dyp_runtime.Tools.local_data_equal = local_data_equal }

let signature f lexbuf =
  let automaton = Dyp_engine.update_parsing_device_data __dypgen_automaton !global_data
    !local_data in
  let pf = Dyp_engine.glrParse automaton Dyp_aux_functions.get_token_value
    Dyp_symbols.get_token_name Dyp_symbols.str_token
    Dyp_symbols.signature __dypgen_data_equal Dyp_symbols_array.test_cons Dyp_symbols_array.str_cons f lexbuf
    Dyp_aux_functions.lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_signature r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf

