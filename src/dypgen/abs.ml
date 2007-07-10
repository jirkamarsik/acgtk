type term =
    | Var of string
    | Abs of (string * term)
    | LAbs of (string * term)
    | App of (term * term)


let rec to_string = function
  | Var s -> s
  | Abs (s,t) -> Printf.sprintf "(Lambda %s.%s)" s (to_string t)
  | LAbs (s,t) -> Printf.sprintf "(lambda %s.%s)" s (to_string t)
  | App (u,v) -> Printf.sprintf "(%s %s)" (to_string u) (to_string v)


type abs = Linear | Non_linear

let abstract x t = function
  | Linear -> LAbs (x,t)
  | Non_linear -> Abs (x,t)

