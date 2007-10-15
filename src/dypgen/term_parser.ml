type token =
  | SYMBOL of (string*(Abs.location))
  | IDENT of (string*(Abs.location))
  | LAMBDA0 of (Abs.location)
  | LAMBDA of (Abs.location)
  | BINDER of (Abs.location)
  | INFIX of (Abs.location)
  | PREFIX of (Abs.location)
  | TYPE of (Abs.location)
  | END_OF_DEC of (Abs.location)
  | SIG_OPEN of (Abs.location)
  | DOT of (Abs.location)
  | RPAREN of (Abs.location)
  | LPAREN of (Abs.location)
  | COMMA of (Abs.location)
  | COLON of (Abs.location)
  | SEMICOLON of (Abs.location)
  | EQUAL of (Abs.location)
  | EOI

module Dyp_symbols =
struct
  let derm_declaration = 1
  let idents = 2
  let lambda = 3
  let sig_entries = 4
  let sig_entry = 5
  let signature = 6
  let term = 7
  let term_declaration = 8
  let term_definition = 9
  let type_declaration = 10
  let type_definition = 11
  let t_SYMBOL = 2
  let t_IDENT = 3
  let t_LAMBDA0 = 4
  let t_LAMBDA = 5
  let t_BINDER = 6
  let t_INFIX = 7
  let t_PREFIX = 8
  let t_TYPE = 9
  let t_END_OF_DEC = 10
  let t_SIG_OPEN = 11
  let t_DOT = 12
  let t_RPAREN = 13
  let t_LPAREN = 14
  let t_COMMA = 15
  let t_COLON = 16
  let t_SEMICOLON = 17
  let t_EQUAL = 18
  let t_EOI = 19
  let get_token_name t = match t with
    | SYMBOL _ -> t_SYMBOL
    | IDENT _ -> t_IDENT
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

type ('derm_declaration,'idents,'lambda,'sig_entries,'sig_entry,'term,'term_declaration,'term_definition,'type_declaration,'type_definition) obj =
  | Obj_BINDER of (Abs.location)
  | Obj_COLON of (Abs.location)
  | Obj_COMMA of (Abs.location)
  | Obj_DOT of (Abs.location)
  | Obj_END_OF_DEC of (Abs.location)
  | Obj_EOI
  | Obj_EQUAL of (Abs.location)
  | Obj_IDENT of (string*(Abs.location))
  | Obj_INFIX of (Abs.location)
  | Obj_LAMBDA of (Abs.location)
  | Obj_LAMBDA0 of (Abs.location)
  | Obj_LPAREN of (Abs.location)
  | Obj_PREFIX of (Abs.location)
  | Obj_RPAREN of (Abs.location)
  | Obj_SEMICOLON of (Abs.location)
  | Obj_SIG_OPEN of (Abs.location)
  | Obj_SYMBOL of (string*(Abs.location))
  | Obj_TYPE of (Abs.location)
  | Obj_derm_declaration of 'derm_declaration
  | Obj_idents of 'idents
  | Obj_lambda of 'lambda
  | Obj_sig_entries of 'sig_entries
  | Obj_sig_entry of 'sig_entry
  | Obj_signature of (Abs.term list)
  | Obj_term of 'term
  | Obj_term_declaration of 'term_declaration
  | Obj_term_definition of 'term_definition
  | Obj_type_declaration of 'type_declaration
  | Obj_type_definition of 'type_definition

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "derm_declaration";
    "idents";
    "lambda";
    "sig_entries";
    "sig_entry";
    "signature";
    "term";
    "term_declaration";
    "term_definition";
    "type_declaration";
    "type_definition";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_signature";"SYMBOL";"IDENT";"LAMBDA0";"LAMBDA";"BINDER";"INFIX";"PREFIX";"TYPE";"END_OF_DEC";"SIG_OPEN";"DOT";"RPAREN";"LPAREN";"COMMA";"COLON";"SEMICOLON";"EQUAL";"EOI"|]
  let test_cons =  [|
    (fun x -> match x with Obj_derm_declaration _ -> true | _ -> false);
    (fun x -> match x with Obj_idents _ -> true | _ -> false);
    (fun x -> match x with Obj_lambda _ -> true | _ -> false);
    (fun x -> match x with Obj_sig_entries _ -> true | _ -> false);
    (fun x -> match x with Obj_sig_entry _ -> true | _ -> false);
    (fun x -> match x with Obj_signature _ -> true | _ -> false);
    (fun x -> match x with Obj_term _ -> true | _ -> false);
    (fun x -> match x with Obj_term_declaration _ -> true | _ -> false);
    (fun x -> match x with Obj_term_definition _ -> true | _ -> false);
    (fun x -> match x with Obj_type_declaration _ -> true | _ -> false);
    (fun x -> match x with Obj_type_definition _ -> true | _ -> false)|]
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
    10|]
  let str_cons o = match o with
    | Obj_derm_declaration _ -> "Obj_derm_declaration"
    | Obj_idents _ -> "Obj_idents"
    | Obj_lambda _ -> "Obj_lambda"
    | Obj_sig_entries _ -> "Obj_sig_entries"
    | Obj_sig_entry _ -> "Obj_sig_entry"
    | Obj_signature _ -> "Obj_signature"
    | Obj_term _ -> "Obj_term"
    | Obj_term_declaration _ -> "Obj_term_declaration"
    | Obj_term_definition _ -> "Obj_term_definition"
    | Obj_type_declaration _ -> "Obj_type_declaration"
    | Obj_type_definition _ -> "Obj_type_definition"
    | _ -> failwith "str_cons, unexpected constructor"
end

module Dyp_parameters =
struct
  let token_nb = 20
  let undef_nt = true
  let entry_points = [(Dyp_symbols.signature,1)]
  let str_token_name t = Dyp_symbols_array.token_name_array.(t)
  let priority_names = [|"default_priority";"app";"atom";"binder"|]
  let merge_warning = false
end

module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)
module Dyp_engine = Dyp_runtime.Parser_PIA

module Dyp_aux_functions =
struct
  let datadyn = Dyp_runtime.Tools.init_datadyn
["derm_declaration",0,"Obj_derm_declaration";"idents",1,"Obj_idents";"lambda",2,"Obj_lambda";"sig_entries",3,"Obj_sig_entries";"sig_entry",4,"Obj_sig_entry";"signature",5,"Obj_signature";"term",6,"Obj_term";"term_declaration",7,"Obj_term_declaration";"term_definition",8,"Obj_term_definition";"type_declaration",9,"Obj_type_declaration";"type_definition",10,"Obj_type_definition"]
["Obj_derm_declaration";"Obj_idents";"Obj_lambda";"Obj_sig_entries";"Obj_sig_entry";"Obj_signature";"Obj_term";"Obj_term_declaration";"Obj_term_definition";"Obj_type_declaration";"Obj_type_definition"]
  let get_token_value t = match t with
    | SYMBOL x -> Obj_SYMBOL x
    | IDENT x -> Obj_IDENT x
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
  let priority_data, atom = Dyp.insert_priority priority_data "atom"
  let priority_data, binder = Dyp.insert_priority priority_data "binder"
  let priority_data = Dyp.add_list_relations priority_data [atom;app;binder]
end

let global_data = ref 0
let local_data = ref 0
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_derm_declaration _ _ = []
let dyp_merge_idents _ _ = []
let dyp_merge_lambda _ _ = []
let dyp_merge_sig_entries _ _ = []
let dyp_merge_sig_entry _ _ = []
let dyp_merge_signature _ _ = []
let dyp_merge_term _ _ = []
let dyp_merge_term_declaration _ _ = []
let dyp_merge_term_definition _ _ = []
let dyp_merge_type_declaration _ _ = []
let dyp_merge_type_definition _ _ = []
let dyp_merge = Dyp.keep_oldest

# 1 "term_parser.dyp"

  open Dyp

  module Env = Set.Make(String)

  let pr s = Printf.fprintf stderr "%s\n%!" s

  let abs x l t = function
    | Abs.Linear -> Abs.LAbs (x,l,t)
    | Abs.Non_linear -> Abs.Abs (x,l,t)

  let rec multiple_abs e k_a l ids t k =
    match ids with
      | [] -> k (t e)
      | [a,l_a] -> k (abs a l_a (t (Env.add a e)) k_a)
      | (a,l_a)::((_,l_b)::_ as tl) -> 
	  let new_env = Env.add a e in
	    multiple_abs new_env k_a l_b tl t (fun r -> abs a l r k_a)


# 276                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.signature,[Dyp.Ter Dyp_symbols.t_SIG_OPEN;Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_END_OF_DEC],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SIG_OPEN  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 282                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 286                "term_parser.ml"
 as _2));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 290                "term_parser.ml"
 as _3));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 294                "term_parser.ml"
 as _4)));`Real_obj (Obj_END_OF_DEC  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 298                "term_parser.ml"
 as _5))] -> Obj_signature 
# 31 "term_parser.dyp"
(
                                              (_4 (Abs.Abstract_sig.empty _2)):Abs.term list)
# 303                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 309                "term_parser.ml"
 as _1)))] -> Obj_sig_entries 
# 34 "term_parser.dyp"
(
            (fun s -> add_sig_entry _1 s):'dypgen__Obj_sig_entries)
# 314                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 320                "term_parser.ml"
 as _1)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 324                "term_parser.ml"
 as _2));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 328                "term_parser.ml"
 as _3)))] -> Obj_sig_entries 
# 35 "term_parser.dyp"
(
                                  (fun s -> _3 (add_sig_entry _1 s)):'dypgen__Obj_sig_entries)
# 333                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_declaration)
# 339                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 38 "term_parser.dyp"
(
                   (fun s -> Type_decl ()):'dypgen__Obj_sig_entry)
# 344                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_definition)
# 350                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 39 "term_parser.dyp"
(
                  ():'dypgen__Obj_sig_entry)
# 355                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.derm_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_derm_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_derm_declaration)
# 361                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 40 "term_parser.dyp"
(
                   ():'dypgen__Obj_sig_entry)
# 366                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.term_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_definition)
# 372                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 41 "term_parser.dyp"
(
                  ():'dypgen__Obj_sig_entry)
# 377                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term_declaration,[Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 383                "term_parser.ml"
 as _1)))] -> Obj_term_declaration 
# 44 "term_parser.dyp"
(
        (_1):'dypgen__Obj_term_declaration)
# 388                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LAMBDA0;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.binder),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 394                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 398                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 402                "term_parser.ml"
 as _3));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 406                "term_parser.ml"
 as _4)))] -> Obj_term 
# 48 "term_parser.dyp"
(
                          (fun env -> multiple_abs env Abs.Linear _1 _2 _4 (fun x -> x)):'dypgen__Obj_term)
# 411                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LAMBDA;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.binder),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 417                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 421                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 425                "term_parser.ml"
 as _3));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 429                "term_parser.ml"
 as _4)))] -> Obj_term 
# 49 "term_parser.dyp"
(
                         (fun env -> multiple_abs env Abs.Non_linear _1 _2 _4 (fun x -> x)):'dypgen__Obj_term)
# 434                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 440                "term_parser.ml"
 as _1))] -> Obj_term 
# 50 "term_parser.dyp"
(
        (let id,l=_1 in
	   fun env -> 
	     match Env.mem id env with
	       | true -> Abs.Var (id,l)
	       | false -> Abs.Const (id,l)):'dypgen__Obj_term)
# 449                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 455                "term_parser.ml"
 as _1));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 459                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 463                "term_parser.ml"
 as _3))] -> Obj_term 
# 55 "term_parser.dyp"
(
                     (_2):'dypgen__Obj_term)
# 468                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.term,[Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.atom)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 474                "term_parser.ml"
 as _1)));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 478                "term_parser.ml"
 as _2)))] -> Obj_term 
# 56 "term_parser.dyp"
(
                           (fun e ->
			    let u1 = _1 e in
			    let u2 = _2 e in
			      Abs.App(Abs.new_loc (Abs.get_loc u1) (Abs.get_loc u2),u1,u2)):'dypgen__Obj_term)
# 486                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 492                "term_parser.ml"
 as _1))] -> Obj_idents 
# 62 "term_parser.dyp"
(
        ([_1]):'dypgen__Obj_idents)
# 497                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 503                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 507                "term_parser.ml"
 as _2)))] -> Obj_idents 
# 63 "term_parser.dyp"
(
               (_1::_2):'dypgen__Obj_idents)
# 512                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 518                "term_parser.ml"
 as _1))] -> Obj_lambda 
# 66 "term_parser.dyp"
(
          (Abs.Linear):'dypgen__Obj_lambda)
# 523                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 529                "term_parser.ml"
 as _1))] -> Obj_lambda 
# 67 "term_parser.dyp"
(
         (Abs.Non_linear):'dypgen__Obj_lambda)
# 534                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_derm_declaration ol o =
  let ol2 = dyp_merge_derm_declaration ol o in
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
let dyp_merge_term_declaration ol o =
  let ol2 = dyp_merge_term_declaration ol o in
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

let __dypgen_merge_map = Dyp_runtime.Tools.init_merge_map [(fun ol o -> (
  let f1 o = match o with Obj_type_definition ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_type_definition"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_type_definition ol o in
  let f2 o = Obj_type_definition o in
  List.map f2 ol)),11;(fun ol o -> (
  let f1 o = match o with Obj_type_declaration ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_type_declaration"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_type_declaration ol o in
  let f2 o = Obj_type_declaration o in
  List.map f2 ol)),10;(fun ol o -> (
  let f1 o = match o with Obj_term_definition ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_definition"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_definition ol o in
  let f2 o = Obj_term_definition o in
  List.map f2 ol)),9;(fun ol o -> (
  let f1 o = match o with Obj_term_declaration ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term_declaration"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term_declaration ol o in
  let f2 o = Obj_term_declaration o in
  List.map f2 ol)),8;(fun ol o -> (
  let f1 o = match o with Obj_term ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_term"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_term ol o in
  let f2 o = Obj_term o in
  List.map f2 ol)),7;(fun ol o -> (
  let f1 o = match o with Obj_signature ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_signature"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_signature ol o in
  let f2 o = Obj_signature o in
  List.map f2 ol)),6;(fun ol o -> (
  let f1 o = match o with Obj_sig_entry ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_sig_entry"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_sig_entry ol o in
  let f2 o = Obj_sig_entry o in
  List.map f2 ol)),5;(fun ol o -> (
  let f1 o = match o with Obj_sig_entries ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_sig_entries"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_sig_entries ol o in
  let f2 o = Obj_sig_entries o in
  List.map f2 ol)),4;(fun ol o -> (
  let f1 o = match o with Obj_lambda ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_lambda"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_lambda ol o in
  let f2 o = Obj_lambda o in
  List.map f2 ol)),3;(fun ol o -> (
  let f1 o = match o with Obj_idents ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_idents"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_idents ol o in
  let f2 o = Obj_idents o in
  List.map f2 ol)),2;(fun ol o -> (
  let f1 o = match o with Obj_derm_declaration ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_derm_declaration"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_derm_declaration ol o in
  let f2 o = Obj_derm_declaration o in
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

