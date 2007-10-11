type token =
  | SYMBOL of (string)
  | IDENT of (string)
  | LAMBDA0
  | LAMBDA
  | BINDER
  | INFIX
  | PREFIX
  | TYPE
  | END_OF_DEC
  | SIG_OPEN
  | DOT
  | RPAREN of (Lexing.position)
  | LPAREN of (Lexing.position)
  | COMMA
  | COLON
  | SEMICOLON
  | EQUAL
  | EOI

module Dyp_symbols =
struct
  let expr = 1
  let exprs = 2
  let idents = 3
  let lambda = 4
  let main = 5
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
    | LAMBDA0 -> t_LAMBDA0
    | LAMBDA -> t_LAMBDA
    | BINDER -> t_BINDER
    | INFIX -> t_INFIX
    | PREFIX -> t_PREFIX
    | TYPE -> t_TYPE
    | END_OF_DEC -> t_END_OF_DEC
    | SIG_OPEN -> t_SIG_OPEN
    | DOT -> t_DOT
    | RPAREN _ -> t_RPAREN
    | LPAREN _ -> t_LPAREN
    | COMMA -> t_COMMA
    | COLON -> t_COLON
    | SEMICOLON -> t_SEMICOLON
    | EQUAL -> t_EQUAL
    | EOI -> t_EOI
  let str_token t = match t with
    | SYMBOL s -> "SYMBOL("^s^")"
    | IDENT s -> "IDENT("^s^")"
    | LAMBDA0 -> "LAMBDA0"
    | LAMBDA -> "LAMBDA"
    | BINDER -> "BINDER"
    | INFIX -> "INFIX"
    | PREFIX -> "PREFIX"
    | TYPE -> "TYPE"
    | END_OF_DEC -> "END_OF_DEC"
    | SIG_OPEN -> "SIG_OPEN"
    | DOT -> "DOT"
    | RPAREN _ -> "RPAREN"
    | LPAREN _ -> "LPAREN"
    | COMMA -> "COMMA"
    | COLON -> "COLON"
    | SEMICOLON -> "SEMICOLON"
    | EQUAL -> "EQUAL"
    | EOI -> "EOI"
end

type ('expr,'exprs,'idents,'lambda) obj =
  | Obj_BINDER
  | Obj_COLON
  | Obj_COMMA
  | Obj_DOT
  | Obj_END_OF_DEC
  | Obj_EOI
  | Obj_EQUAL
  | Obj_IDENT of (string)
  | Obj_INFIX
  | Obj_LAMBDA
  | Obj_LAMBDA0
  | Obj_LPAREN of (Lexing.position)
  | Obj_PREFIX
  | Obj_RPAREN of (Lexing.position)
  | Obj_SEMICOLON
  | Obj_SIG_OPEN
  | Obj_SYMBOL of (string)
  | Obj_TYPE
  | Obj_expr of 'expr
  | Obj_exprs of 'exprs
  | Obj_idents of 'idents
  | Obj_lambda of 'lambda
  | Obj_main of (Abs.term list)

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "expr";
    "exprs";
    "idents";
    "lambda";
    "main";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_main";"SYMBOL";"IDENT";"LAMBDA0";"LAMBDA";"BINDER";"INFIX";"PREFIX";"TYPE";"END_OF_DEC";"SIG_OPEN";"DOT";"RPAREN";"LPAREN";"COMMA";"COLON";"SEMICOLON";"EQUAL";"EOI"|]
  let test_cons =  [|
    (fun x -> match x with Obj_expr _ -> true | _ -> false);
    (fun x -> match x with Obj_exprs _ -> true | _ -> false);
    (fun x -> match x with Obj_idents _ -> true | _ -> false);
    (fun x -> match x with Obj_lambda _ -> true | _ -> false);
    (fun x -> match x with Obj_main _ -> true | _ -> false)|]
  let cons_of_nt =
  [|0;
    0;
    1;
    2;
    3;
    4|]
  let str_cons o = match o with
    | Obj_expr _ -> "Obj_expr"
    | Obj_exprs _ -> "Obj_exprs"
    | Obj_idents _ -> "Obj_idents"
    | Obj_lambda _ -> "Obj_lambda"
    | Obj_main _ -> "Obj_main"
    | _ -> failwith "str_cons, unexpected constructor"
end

module Dyp_parameters =
struct
  let token_nb = 20
  let undef_nt = true
  let entry_points = [(Dyp_symbols.main,1)]
  let str_token_name t = Dyp_symbols_array.token_name_array.(t)
  let priority_names = [|"default_priority";"app";"at";"bind"|]
  let merge_warning = false
end

module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)
module Dyp_engine = Dyp_runtime.Parser_PIA

module Dyp_aux_functions =
struct
  let datadyn = Dyp_runtime.Tools.init_datadyn
["expr",0,"Obj_expr";"exprs",1,"Obj_exprs";"idents",2,"Obj_idents";"lambda",3,"Obj_lambda";"main",4,"Obj_main"]
["Obj_expr";"Obj_exprs";"Obj_idents";"Obj_lambda";"Obj_main"]
  let get_token_value t = match t with
    | SYMBOL x -> Obj_SYMBOL x
    | IDENT x -> Obj_IDENT x
    | LAMBDA0 -> Obj_LAMBDA0
    | LAMBDA -> Obj_LAMBDA
    | BINDER -> Obj_BINDER
    | INFIX -> Obj_INFIX
    | PREFIX -> Obj_PREFIX
    | TYPE -> Obj_TYPE
    | END_OF_DEC -> Obj_END_OF_DEC
    | SIG_OPEN -> Obj_SIG_OPEN
    | DOT -> Obj_DOT
    | RPAREN x -> Obj_RPAREN x
    | LPAREN x -> Obj_LPAREN x
    | COMMA -> Obj_COMMA
    | COLON -> Obj_COLON
    | SEMICOLON -> Obj_SEMICOLON
    | EQUAL -> Obj_EQUAL
    | EOI -> Obj_EOI
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  let transform_av_list l =
    let f o = match o with
      | Obj_BINDER -> `Dummy_obj
      | Obj_COLON -> `Dummy_obj
      | Obj_COMMA -> `Dummy_obj
      | Obj_DOT -> `Dummy_obj
      | Obj_END_OF_DEC -> `Dummy_obj
      | Obj_EOI -> `Dummy_obj
      | Obj_EQUAL -> `Dummy_obj
      | Obj_INFIX -> `Dummy_obj
      | Obj_LAMBDA -> `Dummy_obj
      | Obj_LAMBDA0 -> `Dummy_obj
      | Obj_PREFIX -> `Dummy_obj
      | Obj_SEMICOLON -> `Dummy_obj
      | Obj_SIG_OPEN -> `Dummy_obj
      | Obj_TYPE -> `Dummy_obj
      | x -> `Real_obj x
    in
    List.map f l
end

module Dyp_priority_data =
struct
  let priority_data, default_priority =
    Dyp.insert_priority Dyp.empty_priority_data "default_priority"
  let priority_data, app = Dyp.insert_priority priority_data "app"
  let priority_data, at = Dyp.insert_priority priority_data "at"
  let priority_data, bind = Dyp.insert_priority priority_data "bind"
  let priority_data = Dyp.add_list_relations priority_data [at;bind;app]
end

let global_data = ref 0
let local_data = ref 0
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_expr _ _ = []
let dyp_merge_exprs _ _ = []
let dyp_merge_idents _ _ = []
let dyp_merge_lambda _ _ = []
let dyp_merge_main _ _ = []
let dyp_merge = Dyp.keep_oldest

# 1 "term_parser.dyp"

  open Dyp


# 231                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.main,[Dyp.Non_ter (Dyp_symbols.exprs,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_exprs ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_exprs)
# 237                "term_parser.ml"
 as _1))); _2] -> Obj_main 
# 14 "term_parser.dyp"
(
            (_1):Abs.term list)
# 242                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.main,[Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_main 
# 15 "term_parser.dyp"
(
      ([]):Abs.term list)
# 249                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.exprs,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 255                "term_parser.ml"
 as _1)))] -> Obj_exprs 
# 18 "term_parser.dyp"
(
       ([_1]):'dypgen__Obj_exprs)
# 260                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.exprs,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Non_ter (Dyp_symbols.exprs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 266                "term_parser.ml"
 as _1))); _2;`Real_obj (Obj_exprs ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_exprs)
# 270                "term_parser.ml"
 as _3)))] -> Obj_exprs 
# 19 "term_parser.dyp"
(
                       ((_1)::(_3)):'dypgen__Obj_exprs)
# 275                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA0;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.bind),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 281                "term_parser.ml"
 as _2))); _3;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 285                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 22 "term_parser.dyp"
(
                          (List.fold_right (fun x t -> Abs.LAbs (x,t)) (_2:string list) (_4:Abs.term)):'dypgen__Obj_expr)
# 290                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.bind),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 296                "term_parser.ml"
 as _2))); _3;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 300                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 23 "term_parser.dyp"
(
                         (List.fold_right (fun x t -> Abs.Abs (x,t)) (_2:string list) (_4:Abs.term)):'dypgen__Obj_expr)
# 305                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 311                "term_parser.ml"
 as _1))] -> Obj_expr 
# 24 "term_parser.dyp"
(
        (Abs.Var _1):'dypgen__Obj_expr)
# 316                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Lexing.position)
# 322                "term_parser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 326                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Lexing.position)
# 330                "term_parser.ml"
 as _3))] -> Obj_expr 
# 25 "term_parser.dyp"
(
                     (_2:Abs.term):'dypgen__Obj_expr)
# 335                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.at)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 341                "term_parser.ml"
 as _1)));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 345                "term_parser.ml"
 as _2)))] -> Obj_expr 
# 26 "term_parser.dyp"
(
                         (Abs.App(_1,_2)):'dypgen__Obj_expr)
# 350                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 356                "term_parser.ml"
 as _1))] -> Obj_idents 
# 29 "term_parser.dyp"
(
        ([_1]:string list):'dypgen__Obj_idents)
# 361                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 367                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 371                "term_parser.ml"
 as _2)))] -> Obj_idents 
# 30 "term_parser.dyp"
(
               (_1::_2):'dypgen__Obj_idents)
# 376                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_lambda 
# 33 "term_parser.dyp"
(
          (Abs.Linear):'dypgen__Obj_lambda)
# 383                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_lambda 
# 34 "term_parser.dyp"
(
         (Abs.Non_linear):'dypgen__Obj_lambda)
# 390                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_expr ol o =
  let ol2 = dyp_merge_expr ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_exprs ol o =
  let ol2 = dyp_merge_exprs ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_idents ol o =
  let ol2 = dyp_merge_idents ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_lambda ol o =
  let ol2 = dyp_merge_lambda ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_main ol o =
  let ol2 = dyp_merge_main ol o in
  if ol2 = [] then dyp_merge ol o else ol2

let __dypgen_merge_map = Dyp_runtime.Tools.init_merge_map [(fun ol o -> (
  let f1 o = match o with Obj_main ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_main"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_main ol o in
  let f2 o = Obj_main o in
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
  let f1 o = match o with Obj_exprs ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_exprs"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_exprs ol o in
  let f2 o = Obj_exprs o in
  List.map f2 ol)),2;(fun ol o -> (
  let f1 o = match o with Obj_expr ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_expr"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_expr ol o in
  let f2 o = Obj_expr o in
  List.map f2 ol)),1]

let __dypgen_automaton = Dyp_engine.create_parsing_device __dypgen_ra_list Dyp_priority_data.priority_data `LR0 !global_data !local_data __dypgen_merge_map dyp_merge Dyp_aux_functions.datadyn Dyp_symbols_array.str_non_ter Dyp_symbols_array.cons_of_nt

let __dypgen_data_equal = {
  Dyp_runtime.Tools.global_data_equal = global_data_equal;
  Dyp_runtime.Tools.local_data_equal = local_data_equal }

let main f lexbuf =
  let automaton = Dyp_engine.update_parsing_device_data __dypgen_automaton !global_data
    !local_data in
  let pf = Dyp_engine.glrParse automaton Dyp_aux_functions.get_token_value
    Dyp_symbols.get_token_name Dyp_symbols.str_token
    Dyp_symbols.main __dypgen_data_equal Dyp_symbols_array.test_cons Dyp_symbols_array.str_cons f lexbuf
    Dyp_aux_functions.lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_main r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf

