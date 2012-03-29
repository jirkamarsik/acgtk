open Lambda
open Type_inference

module Test =
struct

open Lambda
open TypeInference

(*let m0 = App(LAbs("x",LVar 0),LAbs("y",Const 0))
let m1 = LAbs("x",LAbs("y",LAbs("z",App(LVar 0,App(LVar 2,LVar 1)))))
let m2 = App(m0,m1)*)

(*let m =
  LAbs("x",
       App(
	 LAbs("y",
	      App(
		LAbs("z",
		     App(Const 0,
			 App(Const 0, LVar 0))),
		App(Const 1, LVar 0))),
	 App(Const 1, LVar 0)))*)

let m =
  LAbs("x",
       App(
	 LAbs("y",
	      App(
		LAbs("z",
		     App(
		       LAbs("t",
			    App(Const 0,
				App(Const 0, LVar 0))),
		       App(Const 0, LVar 0))),
		App(Const 1, LVar 0))),
	 App(Const 1, LVar 0)))

let t = LFun(LFun(Atom 0,Atom 1),LFun(Atom 0,Atom 1))

let v = [|"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"|]

let id_to_string i =
  let rec aux = function
    | i when i < 26 -> v.(i)
    | i -> (aux (i/26))^(v.(i mod 26)) in
  Abstract_syntax.Abstract_syntax.Default,aux i

let id_to_string_2 i =
  let (b,s) = id_to_string i in
  b,"'"^s

let print_aux x =
  let f t = (Printf.printf "\n\t\t"; Printf.printf "%s" (type_to_string t id_to_string_2)) in
  match x with
    | (t,l) -> (Printf.printf "%s" (type_to_string t id_to_string_2);Printf.printf "\n\tconstants :";
              List.iter f l)

let print m =
  (Printf.printf "%s" (term_to_string m id_to_string); Printf.printf "\n\t"; print_aux (type_inference m); Printf.printf "\n")

let main () =
  (*(print (m0,"m0"); print (m1,"m1"); print (m2,"m2"))*)
  print m

end

let () = Test.main()
