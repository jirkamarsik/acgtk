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

type ('expr,'exprs,'idents,'lambda) obj =
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
  let priority_names = [|"default_priority";"app";"atom";"binder"|]
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

let dyp_merge_expr _ _ = []
let dyp_merge_exprs _ _ = []
let dyp_merge_idents _ _ = []
let dyp_merge_lambda _ _ = []
let dyp_merge_main _ _ = []
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


# 234                "term_parser.ml"
let __dypgen_ra_list =
[
((Dyp_symbols.main,[Dyp.Non_ter (Dyp_symbols.exprs,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_exprs ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_exprs)
# 240                "term_parser.ml"
 as _1))); _2] -> Obj_main 
# 31 "term_parser.dyp"
(
            (_1):Abs.term list)
# 245                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.main,[Dyp.Ter Dyp_symbols.t_EOI],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [ _1] -> Obj_main 
# 32 "term_parser.dyp"
(
      ([]):Abs.term list)
# 252                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.exprs,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 258                "term_parser.ml"
 as _1)))] -> Obj_exprs 
# 35 "term_parser.dyp"
(
       ([_1 Env.empty]):'dypgen__Obj_exprs)
# 263                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.exprs,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_SEMICOLON;Dyp.Non_ter (Dyp_symbols.exprs,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 269                "term_parser.ml"
 as _1)));`Real_obj (Obj_SEMICOLON  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 273                "term_parser.ml"
 as _2));`Real_obj (Obj_exprs ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_exprs)
# 277                "term_parser.ml"
 as _3)))] -> Obj_exprs 
# 36 "term_parser.dyp"
(
                       ((_1 Env.empty)::(_3)):'dypgen__Obj_exprs)
# 282                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA0;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.binder),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 288                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 292                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 296                "term_parser.ml"
 as _3));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 300                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 39 "term_parser.dyp"
(
                          (fun env -> multiple_abs env Abs.Linear _1 _2 _4 (fun x -> x)):'dypgen__Obj_expr)
# 305                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LAMBDA;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_DOT;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority )],Dyp_priority_data.binder),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 311                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 315                "term_parser.ml"
 as _2)));`Real_obj (Obj_DOT  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 319                "term_parser.ml"
 as _3));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 323                "term_parser.ml"
 as _4)))] -> Obj_expr 
# 40 "term_parser.dyp"
(
                         (fun env -> multiple_abs env Abs.Non_linear _1 _2 _4 (fun x -> x)):'dypgen__Obj_expr)
# 328                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 334                "term_parser.ml"
 as _1))] -> Obj_expr 
# 41 "term_parser.dyp"
(
        (let id,l=_1 in
	   fun env -> 
	     match Env.mem id env with
	       | true -> Abs.Var (id,l)
	       | false -> Abs.Const (id,l)):'dypgen__Obj_expr)
# 343                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Ter Dyp_symbols.t_LPAREN;Dyp.Non_ter (Dyp_symbols.expr,Dyp.No_priority );Dyp.Ter Dyp_symbols.t_RPAREN],Dyp_priority_data.atom),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LPAREN  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 349                "term_parser.ml"
 as _1));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 353                "term_parser.ml"
 as _2)));`Real_obj (Obj_RPAREN  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 357                "term_parser.ml"
 as _3))] -> Obj_expr 
# 46 "term_parser.dyp"
(
                     (_2):'dypgen__Obj_expr)
# 362                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.expr,[Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.app);Dyp.Non_ter (Dyp_symbols.expr,Dyp.Lesseq_priority Dyp_priority_data.atom)],Dyp_priority_data.app),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 368                "term_parser.ml"
 as _1)));`Real_obj (Obj_expr ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_expr)
# 372                "term_parser.ml"
 as _2)))] -> Obj_expr 
# 47 "term_parser.dyp"
(
                           (fun e ->
			    let u1 = _1 e in
			    let u2 = _2 e in
			      Abs.App(Abs.new_loc (Abs.get_loc u1) (Abs.get_loc u2),u1,u2)):'dypgen__Obj_expr)
# 380                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 386                "term_parser.ml"
 as _1))] -> Obj_idents 
# 53 "term_parser.dyp"
(
        ([_1]):'dypgen__Obj_idents)
# 391                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.idents,[Dyp.Ter Dyp_symbols.t_IDENT;Dyp.Non_ter (Dyp_symbols.idents,Dyp.No_priority )],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_IDENT  (
# 0 "term_parser.dyp"
(_:string*(Abs.location))
# 397                "term_parser.ml"
 as _1));`Real_obj (Obj_idents ( (
# 0 "term_parser.dyp"
(_:'dypgen__Obj_idents)
# 401                "term_parser.ml"
 as _2)))] -> Obj_idents 
# 54 "term_parser.dyp"
(
               (_1::_2):'dypgen__Obj_idents)
# 406                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA0],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA0  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 412                "term_parser.ml"
 as _1))] -> Obj_lambda 
# 57 "term_parser.dyp"
(
          (Abs.Linear):'dypgen__Obj_lambda)
# 417                "term_parser.ml"
 | _ -> raise Dyp.Giveup)))
;
((Dyp_symbols.lambda,[Dyp.Ter Dyp_symbols.t_LAMBDA],Dyp_priority_data.default_priority),Dyp_runtime.Tools.transform_action  (fun dyp __dypgen_av_list -> (match (Dyp_aux_functions.transform_av_list __dypgen_av_list) with [`Real_obj (Obj_LAMBDA  (
# 0 "term_parser.dyp"
(_:Abs.location)
# 423                "term_parser.ml"
 as _1))] -> Obj_lambda 
# 58 "term_parser.dyp"
(
         (Abs.Non_linear):'dypgen__Obj_lambda)
# 428                "term_parser.ml"
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

