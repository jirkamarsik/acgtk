type location = Lexing.position*Lexing.position

type term =
    | Var of string * location
    | Const of string * location
    | Abs of (string * location * term)
    | LAbs of (string * location *  term)
    | App of (location * term *  term)


let rec to_string = function
  | Var (s,_) -> s^"_x"
  | Const (s,_) -> s^"_c"
  | Abs (s,_,t) -> Printf.sprintf "(Lambda %s.%s)" s (to_string t)
  | LAbs (s,_,t) -> Printf.sprintf "(lambda %s.%s)" s (to_string t)
  | App (_,u,v) -> Printf.sprintf "(%s %s)" (to_string u) (to_string v)


let rec get_loc = function
  | Var (_,l) -> l
  | Const (_,l) -> l
  | Abs (_,l,_) -> l
  | LAbs (_,l,_) -> l
  | App (l,_,_) -> l



type abs = Linear | Non_linear

let new_loc (s,_) (_,e) = (s,e)

let abstract l x t = function
  | Linear -> LAbs (x,new_loc l (get_loc t),t)
  | Non_linear -> Abs (x,new_loc l (get_loc t),t)

