
type token =
  | SYMBOL of ((string*Abstract_syntax.Abstract_sig.location))
  | IDENT of ((string*Abstract_syntax.Abstract_sig.location))
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
  let binder = 2
  let comma_ids = 3
  let idents = 4
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
  let ter_string_list = [
      ("ARROW",5);
      ("BINDER",8);
      ("COLON",18);
      ("COMMA",17);
      ("DOT",14);
      ("END_OF_DEC",12);
      ("EOI",21);
      ("EQUAL",20);
      ("IDENT",3);
      ("INFIX",9);
      ("LAMBDA",7);
      ("LAMBDA0",6);
      ("LIN_ARROW",4);
      ("LPAREN",16);
      ("PREFIX",10);
      ("RPAREN",15);
      ("SEMICOLON",19);
      ("SIG_OPEN",13);
      ("SYMBOL",2);
      ("TYPE",11);]
  module Ordered_string =
  struct
    type t = string
    let compare = Pervasives.compare
  end
  module String_ter_map = Map.Make(Ordered_string)
  let ter_of_string =
    List.fold_left (fun tsm (s,i) -> String_ter_map.add s i tsm)
     String_ter_map.empty ter_string_list
end

type ('a_Obj_atomic_type, 'a_Obj_binder, 'a_Obj_comma_ids, 'a_Obj_idents, 'a_Obj_sig_entries, 'a_Obj_sig_entry, 'a_Obj_term, 'a_Obj_term_dec_start, 'a_Obj_term_declaration, 'a_Obj_term_def_start, 'a_Obj_term_definition, 'a_Obj_type_declaration, 'a_Obj_type_definition, 'a_Obj_type_expression) obj =
  | Obj_ARROW of Abstract_syntax.Abstract_sig.location
  | Obj_BINDER of Abstract_syntax.Abstract_sig.location
  | Obj_COLON of Abstract_syntax.Abstract_sig.location
  | Obj_COMMA of Abstract_syntax.Abstract_sig.location
  | Obj_DOT of Abstract_syntax.Abstract_sig.location
  | Obj_END_OF_DEC of Abstract_syntax.Abstract_sig.location
  | Obj_EOI
  | Obj_EQUAL of Abstract_syntax.Abstract_sig.location
  | Obj_IDENT of (string*Abstract_syntax.Abstract_sig.location)
  | Obj_INFIX of Abstract_syntax.Abstract_sig.location
  | Obj_LAMBDA of Abstract_syntax.Abstract_sig.location
  | Obj_LAMBDA0 of Abstract_syntax.Abstract_sig.location
  | Obj_LIN_ARROW of Abstract_syntax.Abstract_sig.location
  | Obj_LPAREN of Abstract_syntax.Abstract_sig.location
  | Obj_PREFIX of Abstract_syntax.Abstract_sig.location
  | Obj_RPAREN of Abstract_syntax.Abstract_sig.location
  | Obj_SEMICOLON of Abstract_syntax.Abstract_sig.location
  | Obj_SIG_OPEN of Abstract_syntax.Abstract_sig.location
  | Obj_SYMBOL of (string*Abstract_syntax.Abstract_sig.location)
  | Obj_TYPE of Abstract_syntax.Abstract_sig.location
  | Obj_atomic_type of 'a_Obj_atomic_type
  | Obj_binder of 'a_Obj_binder
  | Obj_comma_ids of 'a_Obj_comma_ids
  | Obj_idents of 'a_Obj_idents
  | Obj_sig_entries of 'a_Obj_sig_entries
  | Obj_sig_entry of 'a_Obj_sig_entry
  | Obj_signature of Abstract_syntax.Abstract_sig.t
  | Obj_term of 'a_Obj_term
  | Obj_term_dec_start of 'a_Obj_term_dec_start
  | Obj_term_declaration of 'a_Obj_term_declaration
  | Obj_term_def_start of 'a_Obj_term_def_start
  | Obj_term_definition of 'a_Obj_term_definition
  | Obj_type_declaration of 'a_Obj_type_declaration
  | Obj_type_definition of 'a_Obj_type_definition
  | Obj_type_expression of 'a_Obj_type_expression

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "atomic_type";
    "binder";
    "comma_ids";
    "idents";
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
  let str_prio = [|
    "default_priority";
    "app";
    "arrow_type";
    "atom";
    "atom_type";
    "binder"|]
  let token_name_array =
  [|"token_epsilon";
    "dummy_token_signature";
    "SYMBOL";
    "IDENT";
    "LIN_ARROW";
    "ARROW";
    "LAMBDA0";
    "LAMBDA";
    "BINDER";
    "INFIX";
    "PREFIX";
    "TYPE";
    "END_OF_DEC";
    "SIG_OPEN";
    "DOT";
    "RPAREN";
    "LPAREN";
    "COMMA";
    "COLON";
    "SEMICOLON";
    "EQUAL";
    "EOI"|]
  let cons_of_nt =
  [|0;
    19;
    20;
    21;
    22;
    23;
    24;
    25;
    26;
    27;
    28;
    29;
    30;
    31;
    32;
    33|]
  let str_cons o = match o with
    | Obj_ARROW _ -> "Obj_ARROW"
    | Obj_BINDER _ -> "Obj_BINDER"
    | Obj_COLON _ -> "Obj_COLON"
    | Obj_COMMA _ -> "Obj_COMMA"
    | Obj_DOT _ -> "Obj_DOT"
    | Obj_END_OF_DEC _ -> "Obj_END_OF_DEC"
    | Obj_EQUAL _ -> "Obj_EQUAL"
    | Obj_IDENT _ -> "Obj_IDENT"
    | Obj_INFIX _ -> "Obj_INFIX"
    | Obj_LAMBDA _ -> "Obj_LAMBDA"
    | Obj_LAMBDA0 _ -> "Obj_LAMBDA0"
    | Obj_LIN_ARROW _ -> "Obj_LIN_ARROW"
    | Obj_LPAREN _ -> "Obj_LPAREN"
    | Obj_PREFIX _ -> "Obj_PREFIX"
    | Obj_RPAREN _ -> "Obj_RPAREN"
    | Obj_SEMICOLON _ -> "Obj_SEMICOLON"
    | Obj_SIG_OPEN _ -> "Obj_SIG_OPEN"
    | Obj_SYMBOL _ -> "Obj_SYMBOL"
    | Obj_TYPE _ -> "Obj_TYPE"
    | Obj_atomic_type _ -> "Obj_atomic_type"
    | Obj_binder _ -> "Obj_binder"
    | Obj_comma_ids _ -> "Obj_comma_ids"
    | Obj_idents _ -> "Obj_idents"
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
  let entry_points = [
    (Dyp_symbols.signature,1);]
  let str_token_name t = Dyp_symbols_array.token_name_array.(t)
  let merge_warning = false
end

module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)
module Dyp_engine = Dyp_runtime.Parser_PIA

module Dyp_aux_functions =
struct
  let datadyn = Dyp_runtime.Tools.init_datadyn [
    "atomic_type",19,"Obj_atomic_type";
    "binder",20,"Obj_binder";
    "comma_ids",21,"Obj_comma_ids";
    "idents",22,"Obj_idents";
    "sig_entries",23,"Obj_sig_entries";
    "sig_entry",24,"Obj_sig_entry";
    "signature",25,"Obj_signature";
    "term",26,"Obj_term";
    "term_dec_start",27,"Obj_term_dec_start";
    "term_declaration",28,"Obj_term_declaration";
    "term_def_start",29,"Obj_term_def_start";
    "term_definition",30,"Obj_term_definition";
    "type_declaration",31,"Obj_type_declaration";
    "type_definition",32,"Obj_type_definition";
    "type_expression",33,"Obj_type_expression"
    ][
    "Obj_ARROW";
    "Obj_BINDER";
    "Obj_COLON";
    "Obj_COMMA";
    "Obj_DOT";
    "Obj_END_OF_DEC";
    "Obj_EQUAL";
    "Obj_IDENT";
    "Obj_INFIX";
    "Obj_LAMBDA";
    "Obj_LAMBDA0";
    "Obj_LIN_ARROW";
    "Obj_LPAREN";
    "Obj_PREFIX";
    "Obj_RPAREN";
    "Obj_SEMICOLON";
    "Obj_SIG_OPEN";
    "Obj_SYMBOL";
    "Obj_TYPE";
    "Obj_atomic_type";
    "Obj_binder";
    "Obj_comma_ids";
    "Obj_idents";
    "Obj_sig_entries";
    "Obj_sig_entry";
    "Obj_signature";
    "Obj_term";
    "Obj_term_dec_start";
    "Obj_term_declaration";
    "Obj_term_def_start";
    "Obj_term_definition";
    "Obj_type_declaration";
    "Obj_type_definition";
    "Obj_type_expression"]
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

let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_Obj_ARROW = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_BINDER = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_COLON = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_COMMA = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_DOT = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_END_OF_DEC = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_EQUAL = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_IDENT = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_INFIX = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_LAMBDA = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_LAMBDA0 = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_LIN_ARROW = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_LPAREN = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_PREFIX = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_RPAREN = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_SEMICOLON = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_SIG_OPEN = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_SYMBOL = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_TYPE = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_atomic_type = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_binder = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_comma_ids = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_idents = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_sig_entries = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_sig_entry = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_signature = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_term = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_term_dec_start = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_term_declaration = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_term_def_start = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_term_definition = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_type_declaration = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_type_definition = Dyp_runtime.Tools.keep_zero
let dyp_merge_Obj_type_expression = Dyp_runtime.Tools.keep_zero
let dyp_merge = Dyp.keep_one

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
      | [a,l_a] ->
	    k (abs a l_a (t (Env.add a e)) k_a)
      | (a,l_a)::((_,l_b)::_ as tl) -> 
	  let new_env = Env.add a e in
	    multiple_abs new_env k_a l_b tl t (fun r -> k (abs a l r k_a))

  let bind c x l t = Abstract_sig.App (c,Abstract_sig.LAbs (x,t,l),l)

  let rec multiple_bind e binder l ids t k =
    match ids with
      | [] -> k (t e)
      | [a,l_a] ->
	    k (bind binder a l_a (t (Env.add a e)))
      | (a,l_a)::((_,l_b)::_ as tl) ->
	  let new_env = Env.add a e in
	    multiple_bind new_env binder l_b tl t (fun r -> k (bind binder a l r))



  let parse_error e = raise (Error.Error (Error.Parse_error e))


# 465                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.signature,[Dyp.Ter Dyp_symbols.t_SIG_OPEN;Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_END_OF_DEC],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SIG_OPEN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 471                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 475                "term_parser.ml"
 as _2));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 479                "term_parser.ml"
 as _3));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 483                "term_parser.ml"
 as _4)));`Real_obj (Obj_END_OF_DEC  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 487                "term_parser.ml"
 as _5))] -> Obj_signature 
# 68 "term_parser.dyp"
(
                                              (_4 (Abstract_sig.empty (fst _2))):Abstract_syntax.Abstract_sig.t)
# 492                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.signature,[Dyp.Ter Dyp_symbols.t_SIG_OPEN;Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Ter Dyp_symbols.t_END_OF_DEC],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SIG_OPEN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 498                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 502                "term_parser.ml"
 as _2));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 506                "term_parser.ml"
 as _3));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 510                "term_parser.ml"
 as _4)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 514                "term_parser.ml"
 as _5));`Real_obj (Obj_END_OF_DEC  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 518                "term_parser.ml"
 as _6))] -> Obj_signature 
# 69 "term_parser.dyp"
(
                                                        (_4 (Abstract_sig.empty (fst _2))):Abstract_syntax.Abstract_sig.t)
# 523                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 529                "term_parser.ml"
 as _1)))] -> Obj_sig_entries 
# 72 "term_parser.dyp"
(
            (fun s -> _1 s):'dypgen__Obj_sig_entries)
# 534                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entries,[Dyp.Non_ter (Dyp_symbols.sig_entry,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Non_ter (Dyp_symbols.sig_entries,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_sig_entry ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entry)
# 540                "term_parser.ml"
 as _1)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 544                "term_parser.ml"
 as _2));`Real_obj (Obj_sig_entries ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_sig_entries)
# 548                "term_parser.ml"
 as _3)))] -> Obj_sig_entries 
# 73 "term_parser.dyp"
(
                                  (fun s -> _3 (_1 s)):'dypgen__Obj_sig_entries)
# 553                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_declaration)
# 559                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 76 "term_parser.dyp"
(
                   (_1):'dypgen__Obj_sig_entry)
# 564                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.type_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_type_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_definition)
# 570                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 77 "term_parser.dyp"
(
                  (_1):'dypgen__Obj_sig_entry)
# 575                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.term_declaration,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_declaration ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_declaration)
# 581                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 78 "term_parser.dyp"
(
                   (_1):'dypgen__Obj_sig_entry)
# 586                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.sig_entry,[Dyp.Non_ter (Dyp_symbols.term_definition,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_definition ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_definition)
# 592                "term_parser.ml"
 as _1)))] -> Obj_sig_entry 
# 79 "term_parser.dyp"
(
                  (_1):'dypgen__Obj_sig_entry)
# 597                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.type_declaration,[Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Ter Dyp_symbols.t_TYPE],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 603                "term_parser.ml"
 as _1)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 607                "term_parser.ml"
 as _2));`Real_obj (Obj_TYPE  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 611                "term_parser.ml"
 as _3))] -> Obj_type_declaration 
# 82 "term_parser.dyp"
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
# 626                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.comma_ids,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 632                "term_parser.ml"
 as _1))] -> Obj_comma_ids 
# 95 "term_parser.dyp"
(
        ([_1]):'dypgen__Obj_comma_ids)
# 637                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.comma_ids,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_COMMA;Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 643                "term_parser.ml"
 as _1));`Real_obj (Obj_COMMA  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 647                "term_parser.ml"
 as _2));`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 651                "term_parser.ml"
 as _3)))] -> Obj_comma_ids 
# 96 "term_parser.dyp"
(
                        (_1::_3):'dypgen__Obj_comma_ids)
# 656                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.type_definition,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Ter Dyp_symbols.t_TYPE],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 662                "term_parser.ml"
 as _1));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 666                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 670                "term_parser.ml"
 as _3)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 674                "term_parser.ml"
 as _4));`Real_obj (Obj_TYPE  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 678                "term_parser.ml"
 as _5))] -> Obj_type_definition 
# 99 "term_parser.dyp"
(
                                         (fun s ->
					    try
					      Abstract_sig.add_entry (Abstract_sig.Type_def (fst _1,snd _1,fst (_3 s))) s
					    with
					      |Abstract_sig.Duplicate_type_definition -> 
						 let pos1,pos2= snd _1 in
						   parse_error (Error.Duplicated_type (fst _1,pos1,pos2))):'dypgen__Obj_type_definition)
# 689                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.type_expression,[Dyp.Non_ter (Dyp_symbols.atomic_type,Dyp.No_priority )],Dyp_priority_data.atom_type),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_atomic_type ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_atomic_type)
# 695                "term_parser.ml"
 as _1)))] -> Obj_type_expression 
# 108 "term_parser.dyp"
(
              (_1):'dypgen__Obj_type_expression)
# 700                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.type_expression,[Dyp.Non_ter (Dyp_symbols.atomic_type,Dyp.Lesseq_priority Dyp_priority_data.atom_type);Dyp.Ter Dyp_symbols.t_LIN_ARROW;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.Lesseq_priority Dyp_priority_data.arrow_type)],Dyp_priority_data.arrow_type),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_atomic_type ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_atomic_type)
# 706                "term_parser.ml"
 as _1)));`Real_obj (Obj_LIN_ARROW  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 710                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 714                "term_parser.ml"
 as _3)))] -> Obj_type_expression 
# 109 "term_parser.dyp"
(
                                                                       (
    fun sg ->
      let ty1,loc1 = _1 sg in
      let ty2,loc2 = _3 sg in
      let new_loc = Abstract_sig.new_loc loc1 loc2 in Abstract_sig.Linear_arrow (ty1,ty2,new_loc),new_loc):'dypgen__Obj_type_expression)
# 723                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.atomic_type,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom_type),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 729                "term_parser.ml"
 as _1))] -> Obj_atomic_type 
# 116 "term_parser.dyp"
(
        (fun sg ->
	   let id,((pos1,pos2) as l) = _1 in
	     match Abstract_sig.is_type id sg with
	       | true -> Abstract_sig.Type_atom (id,l,[]),l
	       | false -> parse_error (Error.Unknown_type (id,pos1,pos2))):'dypgen__Obj_atomic_type)
# 738                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.atomic_type,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom_type),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 744                "term_parser.ml"
 as _1));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 748                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 752                "term_parser.ml"
 as _3))] -> Obj_atomic_type 
# 121 "term_parser.dyp"
(
                                (fun sg -> fst (_2 sg),Abstract_sig.new_loc _1 _3):'dypgen__Obj_atomic_type)
# 757                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_declaration,[Dyp.Non_ter (Dyp_symbols.term_dec_start,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_dec_start ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_dec_start)
# 763                "term_parser.ml"
 as _1)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 767                "term_parser.ml"
 as _2));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 771                "term_parser.ml"
 as _3)))] -> Obj_term_declaration 
# 124 "term_parser.dyp"
(
                                        (fun s ->
					   List.fold_left
					     (fun acc (id,kind,loc) -> 
						try
						  Abstract_sig.add_entry (Abstract_sig.Term_decl (id,kind,loc,fst (_3 s))) acc
						with
						  | Abstract_sig.Duplicate_term_definition -> 
						      let pos1,pos2= loc in
							parse_error (Error.Duplicated_term (id,pos1,pos2)))
					     s
					     _1):'dypgen__Obj_term_declaration)
# 786                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_dec_start,[Dyp.Non_ter (Dyp_symbols.comma_ids,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_comma_ids ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_comma_ids)
# 792                "term_parser.ml"
 as _1)))] -> Obj_term_dec_start 
# 137 "term_parser.dyp"
(
            (List.map (fun (id,loc) -> (id,Abstract_sig.Default,loc)) _1):'dypgen__Obj_term_dec_start)
# 797                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_PREFIX;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_PREFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 803                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 807                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 138 "term_parser.dyp"
(
               ([fst _2,Abstract_sig.Prefix,snd _2]):'dypgen__Obj_term_dec_start)
# 812                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_INFIX;Dyp.Ter Dyp_symbols.t_SYMBOL],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_INFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 818                "term_parser.ml"
 as _1));`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 822                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 139 "term_parser.dyp"
(
               ([fst _2,Abstract_sig.Infix,snd _2]):'dypgen__Obj_term_dec_start)
# 827                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_dec_start,[Dyp.Ter Dyp_symbols.t_BINDER;Dyp.Ter Dyp_symbols.t_SYMBOL],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_BINDER  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 833                "term_parser.ml"
 as _1));`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 837                "term_parser.ml"
 as _2))] -> Obj_term_dec_start 
# 140 "term_parser.dyp"
(
                ([fst _2,Abstract_sig.Binder,snd _2]):'dypgen__Obj_term_dec_start)
# 842                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 848                "term_parser.ml"
 as _1))] -> Obj_term_def_start 
# 143 "term_parser.dyp"
(
        (fst _1,Abstract_sig.Default,snd _1):'dypgen__Obj_term_def_start)
# 853                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_PREFIX;Dyp.Ter Dyp_symbols.t_SYMBOL],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_PREFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 859                "term_parser.ml"
 as _1));`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 863                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 144 "term_parser.dyp"
(
                (fst _2,Abstract_sig.Prefix,snd _2):'dypgen__Obj_term_def_start)
# 868                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_INFIX;Dyp.Ter Dyp_symbols.t_SYMBOL],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_INFIX  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 874                "term_parser.ml"
 as _1));`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 878                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 145 "term_parser.dyp"
(
               (fst _2,Abstract_sig.Infix,snd _2):'dypgen__Obj_term_def_start)
# 883                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_def_start,[Dyp.Ter Dyp_symbols.t_BINDER;Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_BINDER  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 889                "term_parser.ml"
 as _1));`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 893                "term_parser.ml"
 as _2))] -> Obj_term_def_start 
# 146 "term_parser.dyp"
(
               (fst _2,Abstract_sig.Binder,snd _2):'dypgen__Obj_term_def_start)
# 898                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term_definition,[Dyp.Non_ter (Dyp_symbols.term_def_start,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EQUAL;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_COLON;Dyp.Non_ter (Dyp_symbols.type_expression,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term_def_start ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term_def_start)
# 904                "term_parser.ml"
 as _1)));`Real_obj (Obj_EQUAL  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 908                "term_parser.ml"
 as _2));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 912                "term_parser.ml"
 as _3)));`Real_obj (Obj_COLON  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 916                "term_parser.ml"
 as _4));`Real_obj (Obj_type_expression ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_type_expression)
# 920                "term_parser.ml"
 as _5)))] -> Obj_term_definition 
# 149 "term_parser.dyp"
(
                                                  (
    let id,k,l = _1 in
      fun s -> Abstract_sig.add_entry (Abstract_sig.Term_def (id,k,l,_3 s Env.empty,fst (_5 s))) s):'dypgen__Obj_term_definition)
# 927                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Non_ter (Dyp_symbols.binder,Dyp.No_priority );Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.binder),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_binder ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_binder)
# 933                "term_parser.ml"
 as _1)));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 937                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 941                "term_parser.ml"
 as _3));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 945                "term_parser.ml"
 as _4)))] -> Obj_term 
# 155 "term_parser.dyp"
(
                         (fun s env -> (_1 s) _2 _4 env):'dypgen__Obj_term)
# 950                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 956                "term_parser.ml"
 as _1))] -> Obj_term 
# 156 "term_parser.dyp"
(
        (fun s -> let id,l=_1 in
	   fun env -> 
	     match Env.mem id env with
	       | true -> Abstract_sig.Var (id,l)
	       | false -> 
		   (match Abstract_sig.is_constant id s with
		      | true,_ -> Abstract_sig.Const (id,l)
		      | false,_ -> parse_error (Error.Unknown_constant (id,fst l,snd l)))):'dypgen__Obj_term)
# 968                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 974                "term_parser.ml"
 as _1));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 978                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 982                "term_parser.ml"
 as _3))] -> Obj_term 
# 164 "term_parser.dyp"
(
                     (_2):'dypgen__Obj_term)
# 987                "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.term,Dyp.Lesseq_priority Dyp_priority_data.atom)],Dyp_priority_data.app),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 993                "term_parser.ml"
 as _1)));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 997                "term_parser.ml"
 as _2)))] -> Obj_term 
# 165 "term_parser.dyp"
(
                           (fun s e ->
			      let u1 = _1 s e in
			      let u2 = _2 s e in
				Abstract_sig.App(u1,u2,Abstract_sig.new_loc (Abstract_sig.get_term_location u1) (Abstract_sig.get_term_location u2))):'dypgen__Obj_term)
# 1005               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Ter Dyp_symbols.t_SYMBOL;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 1011               "term_parser.ml"
 as _1));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 1015               "term_parser.ml"
 as _2)))] -> Obj_term 
# 169 "term_parser.dyp"
(
              (fun sg env -> 
		 let id,((pos1,pos2) as l) = _1 in
		   match Abstract_sig.is_constant id sg with
		     | true,Some (Abstract_sig.Prefix) -> 
			 let u2 = _2 sg env in
			   Abstract_sig.App(Abstract_sig.Const(id,l),u2,Abstract_sig.new_loc l (Abstract_sig.get_term_location u2))
		     | _ -> raise Dyp.Giveup):'dypgen__Obj_term)
# 1026               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.term,[Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SYMBOL;Dyp.Non_ter (Dyp_symbols.term,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 1032               "term_parser.ml"
 as _1)));`Real_obj (Obj_SYMBOL  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 1036               "term_parser.ml"
 as _2));`Real_obj (Obj_term ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_term)
# 1040               "term_parser.ml"
 as _3)))] -> Obj_term 
# 177 "term_parser.dyp"
(
                   (fun sg env -> 
		      let id,((pos1,pos2) as l) = _2 in
			match Abstract_sig.is_constant id sg with
			  | true,Some (Abstract_sig.Infix) -> 
			      let u1 = _1 sg env in
			      let u2 = _3 sg env in
				Abstract_sig.App (Abstract_sig.App(Abstract_sig.Const(id,l),u1,Abstract_sig.new_loc (Abstract_sig.get_term_location u1) l),u2,Abstract_sig.new_loc (Abstract_sig.get_term_location u1) (Abstract_sig.get_term_location u2))
			  | _ -> raise Dyp.Giveup):'dypgen__Obj_term)
# 1052               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.idents,[],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [] -> Obj_idents 
# 187 "term_parser.dyp"
(
  ([]):'dypgen__Obj_idents)
# 1059               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 1065               "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 1069               "term_parser.ml"
 as _2)))] -> Obj_idents 
# 188 "term_parser.dyp"
(
               (_1::_2):'dypgen__Obj_idents)
# 1074               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.binder,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abstract_syntax.Abstract_sig.location)
# 1080               "term_parser.ml"
 as _1))] -> Obj_binder 
# 191 "term_parser.dyp"
(
          (fun sg ids t env -> multiple_abs env Abstract_sig.Linear _1 ids (t sg) (fun x -> x)):'dypgen__Obj_binder)
# 1085               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))
;
((Dyp_symbols.binder,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match (Dyp_runtime.Tools.transform_action (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:(string*Abstract_syntax.Abstract_sig.location))
# 1091               "term_parser.ml"
 as _1))] -> Obj_binder 
# 192 "term_parser.dyp"
(
        (fun sg -> let binder,((p1,p2) as l) = _1 in
	   match Abstract_sig.is_constant binder sg with
	     | true,Some Abstract_sig.Binder -> fun ids t env -> multiple_bind env (Abstract_sig.Const(binder,l)) (snd _1) ids (t sg) (fun x -> x)
	       | _  -> parse_error (Error.Binder_expected (binder,p1,p2))):'dypgen__Obj_binder)
# 1099               "term_parser.ml"
,[] | _ -> raise Dyp.Giveup))) with Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h))]

let dyp_merge_Obj_ARROW l =
  match dyp_merge_Obj_ARROW l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_BINDER l =
  match dyp_merge_Obj_BINDER l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_COLON l =
  match dyp_merge_Obj_COLON l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_COMMA l =
  match dyp_merge_Obj_COMMA l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_DOT l =
  match dyp_merge_Obj_DOT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_END_OF_DEC l =
  match dyp_merge_Obj_END_OF_DEC l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_EQUAL l =
  match dyp_merge_Obj_EQUAL l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_IDENT l =
  match dyp_merge_Obj_IDENT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_INFIX l =
  match dyp_merge_Obj_INFIX l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LAMBDA l =
  match dyp_merge_Obj_LAMBDA l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LAMBDA0 l =
  match dyp_merge_Obj_LAMBDA0 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LIN_ARROW l =
  match dyp_merge_Obj_LIN_ARROW l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LPAREN l =
  match dyp_merge_Obj_LPAREN l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_PREFIX l =
  match dyp_merge_Obj_PREFIX l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_RPAREN l =
  match dyp_merge_Obj_RPAREN l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_SEMICOLON l =
  match dyp_merge_Obj_SEMICOLON l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_SIG_OPEN l =
  match dyp_merge_Obj_SIG_OPEN l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_SYMBOL l =
  match dyp_merge_Obj_SYMBOL l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_TYPE l =
  match dyp_merge_Obj_TYPE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_atomic_type l =
  match dyp_merge_Obj_atomic_type l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_binder l =
  match dyp_merge_Obj_binder l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_comma_ids l =
  match dyp_merge_Obj_comma_ids l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_idents l =
  match dyp_merge_Obj_idents l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_sig_entries l =
  match dyp_merge_Obj_sig_entries l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_sig_entry l =
  match dyp_merge_Obj_sig_entry l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_signature l =
  match dyp_merge_Obj_signature l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_term l =
  match dyp_merge_Obj_term l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_term_dec_start l =
  match dyp_merge_Obj_term_dec_start l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_term_declaration l =
  match dyp_merge_Obj_term_declaration l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_term_def_start l =
  match dyp_merge_Obj_term_def_start l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_term_definition l =
  match dyp_merge_Obj_term_definition l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_type_declaration l =
  match dyp_merge_Obj_type_declaration l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_type_definition l =
  match dyp_merge_Obj_type_definition l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_type_expression l =
  match dyp_merge_Obj_type_expression l with
    | ([],_,_) -> dyp_merge l
    | res -> res

let __dypgen_merge_list = [(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ARROW ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ARROW"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ARROW l in
  let f2 o = Obj_ARROW o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_BINDER ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_BINDER"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_BINDER l in
  let f2 o = Obj_BINDER o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_COLON ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_COLON"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_COLON l in
  let f2 o = Obj_COLON o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_COMMA ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_COMMA"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_COMMA l in
  let f2 o = Obj_COMMA o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_DOT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_DOT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_DOT l in
  let f2 o = Obj_DOT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_END_OF_DEC ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_END_OF_DEC"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_END_OF_DEC l in
  let f2 o = Obj_END_OF_DEC o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_EQUAL ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_EQUAL"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_EQUAL l in
  let f2 o = Obj_EQUAL o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_IDENT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_IDENT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_IDENT l in
  let f2 o = Obj_IDENT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_INFIX ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_INFIX"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_INFIX l in
  let f2 o = Obj_INFIX o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LAMBDA ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LAMBDA"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LAMBDA l in
  let f2 o = Obj_LAMBDA o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LAMBDA0 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LAMBDA0"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LAMBDA0 l in
  let f2 o = Obj_LAMBDA0 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LIN_ARROW ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LIN_ARROW"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LIN_ARROW l in
  let f2 o = Obj_LIN_ARROW o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LPAREN ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LPAREN"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LPAREN l in
  let f2 o = Obj_LPAREN o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_PREFIX ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_PREFIX"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_PREFIX l in
  let f2 o = Obj_PREFIX o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_RPAREN ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_RPAREN"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_RPAREN l in
  let f2 o = Obj_RPAREN o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_SEMICOLON ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_SEMICOLON"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_SEMICOLON l in
  let f2 o = Obj_SEMICOLON o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_SIG_OPEN ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_SIG_OPEN"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_SIG_OPEN l in
  let f2 o = Obj_SIG_OPEN o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_SYMBOL ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_SYMBOL"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_SYMBOL l in
  let f2 o = Obj_SYMBOL o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_TYPE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_TYPE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_TYPE l in
  let f2 o = Obj_TYPE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_atomic_type ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_atomic_type"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_atomic_type l in
  let f2 o = Obj_atomic_type o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_binder ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_binder"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_binder l in
  let f2 o = Obj_binder o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_comma_ids ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_comma_ids"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_comma_ids l in
  let f2 o = Obj_comma_ids o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_idents ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_idents"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_idents l in
  let f2 o = Obj_idents o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_sig_entries ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_sig_entries"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_sig_entries l in
  let f2 o = Obj_sig_entries o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_sig_entry ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_sig_entry"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_sig_entry l in
  let f2 o = Obj_sig_entry o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_signature ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_signature"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_signature l in
  let f2 o = Obj_signature o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_term ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_term"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_term l in
  let f2 o = Obj_term o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_term_dec_start ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_term_dec_start"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_term_dec_start l in
  let f2 o = Obj_term_dec_start o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_term_declaration ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_term_declaration"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_term_declaration l in
  let f2 o = Obj_term_declaration o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_term_def_start ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_term_def_start"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_term_def_start l in
  let f2 o = Obj_term_def_start o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_term_definition ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_term_definition"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_term_definition l in
  let f2 o = Obj_term_definition o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_type_declaration ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_type_declaration"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_type_declaration l in
  let f2 o = Obj_type_declaration o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_type_definition ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_type_definition"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_type_definition l in
  let f2 o = Obj_type_definition o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_type_expression ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_type_expression"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_type_expression l in
  let f2 o = Obj_type_expression o in
  (List.map f2 ol, gd, ld)))]



let __dypgen_test_cons =  [|
  (fun x -> match x with Obj_ARROW _ -> true | _ -> false);
  (fun x -> match x with Obj_BINDER _ -> true | _ -> false);
  (fun x -> match x with Obj_COLON _ -> true | _ -> false);
  (fun x -> match x with Obj_COMMA _ -> true | _ -> false);
  (fun x -> match x with Obj_DOT _ -> true | _ -> false);
  (fun x -> match x with Obj_END_OF_DEC _ -> true | _ -> false);
  (fun x -> match x with Obj_EQUAL _ -> true | _ -> false);
  (fun x -> match x with Obj_IDENT _ -> true | _ -> false);
  (fun x -> match x with Obj_INFIX _ -> true | _ -> false);
  (fun x -> match x with Obj_LAMBDA _ -> true | _ -> false);
  (fun x -> match x with Obj_LAMBDA0 _ -> true | _ -> false);
  (fun x -> match x with Obj_LIN_ARROW _ -> true | _ -> false);
  (fun x -> match x with Obj_LPAREN _ -> true | _ -> false);
  (fun x -> match x with Obj_PREFIX _ -> true | _ -> false);
  (fun x -> match x with Obj_RPAREN _ -> true | _ -> false);
  (fun x -> match x with Obj_SEMICOLON _ -> true | _ -> false);
  (fun x -> match x with Obj_SIG_OPEN _ -> true | _ -> false);
  (fun x -> match x with Obj_SYMBOL _ -> true | _ -> false);
  (fun x -> match x with Obj_TYPE _ -> true | _ -> false);
  (fun x -> match x with Obj_atomic_type _ -> true | _ -> false);
  (fun x -> match x with Obj_binder _ -> true | _ -> false);
  (fun x -> match x with Obj_comma_ids _ -> true | _ -> false);
  (fun x -> match x with Obj_idents _ -> true | _ -> false);
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

let __dypgen_automaton = Dyp_engine.create_parsing_device __dypgen_ra_list Dyp_priority_data.priority_data `LR0 global_data local_data Dyp_aux_functions.datadyn Dyp_symbols_array.str_non_ter Dyp_symbols_array.cons_of_nt Dyp_symbols_array.str_prio

let __dypgen_data_equal = {
  Dyp_runtime.Tools.global_data_equal = global_data_equal;
  Dyp_runtime.Tools.local_data_equal = local_data_equal }

let signature ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp_engine.glrParse __dypgen_automaton Dyp_aux_functions.get_token_value
    Dyp_symbols.get_token_name Dyp_symbols.str_token
    Dyp_symbols.signature __dypgen_data_equal __dypgen_test_cons Dyp_symbols_array.str_cons (Dyp_runtime.Tools.array_of_list __dypgen_merge_list) global_data local_data __dypgen_ra_list Dyp_priority_data.priority_data Dyp_symbols_array.str_non_ter f lexbuf
    Dyp_aux_functions.lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_signature r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf

