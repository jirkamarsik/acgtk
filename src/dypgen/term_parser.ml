type token =
  | DOT
  | LAMBDA0
  | LAMBDA
  | IDENT of (string)
  | RPAREN
  | LPAREN
  | EOI

module Dyp_symbols =
struct
  let expr = 1
  let idents = 2
  let lambda = 3
  let main = 4
  let t_DOT = 2
  let t_LAMBDA0 = 3
  let t_LAMBDA = 4
  let t_IDENT = 5
  let t_RPAREN = 6
  let t_LPAREN = 7
  let t_EOI = 8
  let get_token_name t = match t with
    | DOT -> t_DOT
    | LAMBDA0 -> t_LAMBDA0
    | LAMBDA -> t_LAMBDA
    | IDENT _ -> t_IDENT
    | RPAREN -> t_RPAREN
    | LPAREN -> t_LPAREN
    | EOI -> t_EOI
  let str_token t = match t with
    | DOT -> "DOT"
    | LAMBDA0 -> "LAMBDA0"
    | LAMBDA -> "LAMBDA"
    | IDENT s -> "IDENT("^s^")"
    | RPAREN -> "RPAREN"
    | LPAREN -> "LPAREN"
    | EOI -> "EOI"
end

type ('expr,'idents,'lambda) obj =
  | Obj_DOT
  | Obj_EOI
  | Obj_IDENT of (string)
  | Obj_LAMBDA
  | Obj_LAMBDA0
  | Obj_LPAREN
  | Obj_RPAREN
  | Obj_expr of 'expr
  | Obj_idents of 'idents
  | Obj_lambda of 'lambda
  | Obj_main of (Abs.term option)

module Dyp_symbols_array =
struct
  let str_non_ter =
  [|"S'";
    "expr";
    "idents";
    "lambda";
    "main";|]
  let token_name_array =
    [|"token_epsilon";"dummy_token_main";"DOT";"LAMBDA0";"LAMBDA";"IDENT";"RPAREN";"LPAREN";"EOI"|]
  let test_cons =  [|
    (fun x -> match x with Obj_expr _ -> true | _ -> false);
    (fun x -> match x with Obj_idents _ -> true | _ -> false);
    (fun x -> match x with Obj_lambda _ -> true | _ -> false);
    (fun x -> match x with Obj_main _ -> true | _ -> false)|]
  let cons_of_nt =
  [|0;
    0;
    1;
    2;
    3|]
  let str_cons o = match o with
    | Obj_expr _ -> "Obj_expr"
    | Obj_idents _ -> "Obj_idents"
    | Obj_lambda _ -> "Obj_lambda"
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
["expr",0,"Obj_expr";"idents",1,"Obj_idents";"lambda",2,"Obj_lambda";"main",3,"Obj_main"]
["Obj_expr";"Obj_idents";"Obj_lambda";"Obj_main"]
  let get_token_value t = match t with
    | DOT -> Obj_DOT
    | LAMBDA0 -> Obj_LAMBDA0
    | LAMBDA -> Obj_LAMBDA
    | IDENT x -> Obj_IDENT x
    | RPAREN -> Obj_RPAREN
    | LPAREN -> Obj_LPAREN
    | EOI -> Obj_EOI
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  let transform_av_list l =
    let f o = match o with
      | Obj_DOT -> `Dummy_obj
      | Obj_EOI -> `Dummy_obj
      | Obj_LAMBDA -> `Dummy_obj
      | Obj_LAMBDA0 -> `Dummy_obj
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

let dyp_merge_expr _ _ = []
let dyp_merge_idents _ _ = []
let dyp_merge_lambda _ _ = []
let dyp_merge_main _ _ = []
let dyp_merge = Dyp.keep_oldest

# 1 "term_parser.dyp"

  open Dyp

  let parse_error s = 
    Printf.fprintf stderr "I found the error %s\m" s
      
  type error_description =
      {mutable start:int;
       mutable ending:int}

  let error_loc = {start= -1;ending= -1}       

  let set_error d = let () = error_loc.start <- (d.symbol_start ()) in
		     error_loc.ending <- (d.symbol_end ())

  let error_msg {start=s;ending=e} = Printf.sprintf "between %d and %d" s e

  let error () = error_msg error_loc

# 165                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.main,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 171                "term_parser.ml"
 as _1))); _2] -> Obj_main 
# 29 "term_parser.dyp"
(
           (Some _1):Abs.term option)
# 176                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.main,[Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_main 
# 30 "term_parser.dyp"
(
      (None):Abs.term option)
# 183                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA0;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.bind),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 189                "term_parser.ml"
 as _2))); _3;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 193                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 33 "term_parser.dyp"
(
                          (let () = set_error dyp in List.fold_right (fun x t -> Abs.LAbs (x,t)) (_2:string list) (_4:Abs.term)):'dypgen__Obj_expr)
# 198                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.bind),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 204                "term_parser.ml"
 as _2))); _3;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 208                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 34 "term_parser.dyp"
(
                         (let () = set_error dyp in List.fold_right (fun x t -> Abs.Abs (x,t)) (_2:string list) (_4:Abs.term)):'dypgen__Obj_expr)
# 213                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 219                "term_parser.ml"
 as _1))] -> Obj_expr 
# 35 "term_parser.dyp"
(
        (let () = set_error dyp in Abs.Var _1):'dypgen__Obj_expr)
# 224                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.at),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1;`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 230                "term_parser.ml"
 as _2))); _3] -> Obj_expr 
# 36 "term_parser.dyp"
(
                     (let () = set_error dyp in _2:Abs.term):'dypgen__Obj_expr)
# 235                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.at)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 241                "term_parser.ml"
 as _1)));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 245                "term_parser.ml"
 as _2)))] -> Obj_expr 
# 37 "term_parser.dyp"
(
                         (let () = set_error dyp in Abs.App(_1,_2)):'dypgen__Obj_expr)
# 250                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 256                "term_parser.ml"
 as _1))] -> Obj_idents 
# 40 "term_parser.dyp"
(
        (let () = set_error dyp in [_1]:string list):'dypgen__Obj_idents)
# 261                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string)
# 267                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 271                "term_parser.ml"
 as _2)))] -> Obj_idents 
# 41 "term_parser.dyp"
(
               (let () = set_error dyp in _1::_2):'dypgen__Obj_idents)
# 276                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_lambda 
# 44 "term_parser.dyp"
(
          (let () = set_error dyp in Abs.Linear):'dypgen__Obj_lambda)
# 283                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_lambda 
# 45 "term_parser.dyp"
(
         (let () = set_error dyp in Abs.Non_linear):'dypgen__Obj_lambda)
# 290                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))]

let dyp_merge_expr ol o =
  let ol2 = dyp_merge_expr ol o in
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

