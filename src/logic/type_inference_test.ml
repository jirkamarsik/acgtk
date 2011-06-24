open Lambda
open Type_inference

module Test =
struct

open Lambda
open TypeInference

let m0 = App(LAbs("x",LVar 0),LAbs("y",Const 0))
let m1 = LAbs("x",LAbs("y",LAbs("z",App(LVar 0,App(LVar 2,LVar 1)))))
let m2 = App(m0,m1)

let id_to_string i =
  Abstract_syntax.Abstract_syntax.Default,Printf.sprintf "Const %d" i

let print_aux x =
  let f t = (Printf.printf "\n\t\t"; Printf.printf "%s" (type_to_string t id_to_string)) in
  match x with
    | (t,l) -> (Printf.printf "%s" (type_to_string t id_to_string);Printf.printf "\n\tconstants :";
              List.iter f l)

let print = function
  | (m,n) -> (Printf.printf "%s" n; Printf.printf "%s" (term_to_string m id_to_string); Printf.printf "\n\t"; print_aux (type_inference m); Printf.printf "\n")

let main () =
  (print (m0,"m0"); print (m1,"m1"); print (m2,"m2"))

end

let () = Test.main()
