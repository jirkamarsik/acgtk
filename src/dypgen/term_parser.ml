type token =
  | DOT
  | LAMBDA0 of (string)
  | LAMBDA of (string)
  | IDENT of (string)
  | RPAREN
  | LPAREN
  | EOI

module Dyp_symbols =
struct
  let binder = 1
  let expr = 2
  let main = 3
  let t_DOT = 2
  let t_LAMBDA0 = 3
  let t_LAMBDA = 4
  let t_IDENT = 5
  let t_RPAREN = 6
  let t_LPAREN = 7
  let t_EOI = 8
  let get_token_name t = match t with
    | DOT -> t_DOT
    | LAMBDA0 _ -> t_LAMBDA0
    | LAMBDA _ -> t_LAMBDA
    | IDENT _ -> t_IDENT
    | RPAREN -> t_RPAREN
    | LPAREN -> t_LPAREN
    | EOI -> t_EOI
  let str_token t = match t with
    | DOT -> "DOT"
    | LAMBDA0 s -> "LAMBDA0("^s^")"
    | LAMBDA s -> "LAMBDA("^s^")"
    | IDENT s -> "IDENT("^s^")"
    | RPAREN -> "RPAREN"
    | LPAREN -> "LPAREN"
    | EOI -> "EOI"
end

type ('binder,'expr) obj =
  | Obj_DOT
  | Obj_EOI
  | Obj_IDENT of (string)
  | Obj_LAMBDA of (string)
  | Obj_LAMBDA0 of (string)
  | Obj_LPAREN
  | Obj_RPAREN
  | Obj_binder of 'binder
  | Obj_expr of 'expr
  | Obj_main of (term)

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "binder";
    "expr";
    "main";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_main";"DOT";"LAMBDA0";"LAMBDA";"IDENT";"RPAREN";"LPAREN";"EOI"|]
  let test_cons =  [|
    (fun x -> match x with Obj_binder _ -> true | _ -> false);
    (fun x -> match x with Obj_expr _ -> true | _ -> false);
    (fun x -> match x with Obj_main _ -> true | _ -> false)|]
  let cons_of_nt =
  [|0;
    0;
    1;
    2|]
  let str_cons o = match o with
    | Obj_binder _ -> "Obj_binder"
    | Obj_expr _ -> "Obj_expr"
    | Obj_main _ -> "Obj_main"
    | _ -> failwith "str_cons, unexpected constructor"
end

module Dyp_parameters =
struct
  let token_nb = 9
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
["binder",0,"Obj_binder";"expr",1,"Obj_expr";"main",2,"Obj_main"]
["Obj_binder";"Obj_expr";"Obj_main"]
  let get_token_value t = match t with
    | DOT -> Obj_DOT
    | LAMBDA0 x -> Obj_LAMBDA0 x
    | LAMBDA x -> Obj_LAMBDA x
    | IDENT x -> Obj_IDENT x
    | RPAREN -> Obj_RPAREN
    | LPAREN -> Obj_LPAREN
    | EOI -> Obj_EOI
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  let transform_av_list l =
    let f o = match o with
      | Obj_DOT -> `Dummy_obj
      | Obj_EOI -> `Dummy_obj
      | Obj_LPAREN -> `Dummy_obj
      | Obj_RPAREN -> `Dummy_obj
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

let dyp_merge_binder _ _ = []
let dyp_merge_expr _ _ = []
let dyp_merge_main _ _ = []
let dyp_merge = Dyp.keep_oldest

# 1 "term_parser.dyp"

  type term =
    | Var of string
    | Abs of (string,term)
    | LAbs of (string,term)
    | App of (term,term)

  type abs =
    | Linear of string
    | Non_linear of string

  let wrap abs t = match abs with
    | Linear s -> LAbs (s,t)
    | Non_linear s -> Abs (s,t)


# 153                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.main,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 159                "term_parser.ml"
 as _1))); _2] -> Obj_main 
# 26 "term_parser.dyp"
(
                  (_1):term)
# 164                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.binder,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.bind),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_binder ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_binder)
# 170                "term_parser.ml"
 as _1))); _2;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 174                "term_parser.ml"
 as _3)))] -> Obj_expr 
# 29 "term_parser.dyp"
(
                  (wrap _1_ _3_):'dypgen__Obj_expr)
# 179                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 185                "term_parser.ml"
 as _1))] -> Obj_expr 
# 30 "term_parser.dyp"
(
        (Var (_1)):'dypgen__Obj_expr)
# 190                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 196                "term_parser.ml"
 as _2))); _3] -> Obj_expr 
# 31 "term_parser.dyp"
(
                     (_2):'dypgen__Obj_expr)
# 201                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.expr,Dyp.Less_priority Dyp_priority_data.at)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 207                "term_parser.ml"
 as _1)));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 211                "term_parser.ml"
 as _2)))] -> Obj_expr 
# 32 "term_parser.dyp"
(
                        (App(_1,_2)):'dypgen__Obj_expr)
# 216                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.binder,[Dyp.Ter Dyp_symbols.t_LAMBDA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA  (
# 0 "term_parser.dyp"
(_:string)
# 222                "term_parser.ml"
 as _1))] -> Obj_binder 
# 36 "term_parser.dyp"
(
         (Non_linear _1):'dypgen__Obj_binder)
# 227                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.binder,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:string)
# 233                "term_parser.ml"
 as _1))] -> Obj_binder 
# 37 "term_parser.dyp"
(
          (Linear _1):'dypgen__Obj_binder)
# 238                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_binder ol o =
  let ol2 = dyp_merge_binder ol o in
  if ol2 = [] then dyp_merge ol o else ol2
let dyp_merge_expr ol o =
  let ol2 = dyp_merge_expr ol o in
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
  List.map f2 ol)),3;(fun ol o -> (
  let f1 o = match o with Obj_expr ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_expr"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_expr ol o in
  let f2 o = Obj_expr o in
  List.map f2 ol)),2;(fun ol o -> (
  let f1 o = match o with Obj_binder ob -> ob
    | _ -> failwith "type error, bad obj in dyp_merge_binder"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_binder ol o in
  let f2 o = Obj_binder o in
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

