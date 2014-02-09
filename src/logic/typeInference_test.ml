open TypeInference
open Lambda.Lambda



let test t =
  let () = Printf.printf "Testing \"%s\"...\n" (raw_to_string t) in
  let ty,map = Type.inference t in
  let () = Printf.printf "Got \"%s\".\n" (raw_type_to_string ty) in
  let () = Printf.printf "In the context of:\n" in
  let () = 
    Utils.IntMap.iter
      (fun k (t,ty) -> Printf.printf "%d --> %s : %s\n" k (raw_to_string t) (raw_type_to_string ty))
      map in
  ()


(* grep Atom acg.log | grep -o "(.*" | sed -e 's/\(.*\):.*/\1;/' > ~/tmp/terms *)

(*let term_list=[
  (Const 3) ;
(Const 4) ;
(LAbs ("o",(LAbs ("s",(App ((App ((Const 1),(LVar 0))),(App ((App ((Const 1),(Const 5))),(LVar 1))))))))) ;
(LAbs ("n",(LAbs ("P",(App ((LVar 0),(App ((App ((Const 1),(Const 6))),(LVar 1))))))))) ;
(LAbs ("n",(LAbs ("P",(App ((LVar 0),(App ((App ((Const 1),(Const 7))),(LVar 1))))))))) ;
(Const 8) ;
(Const 9) ;
(Const 3) ;
(Const 4) ;
(Const 5) ;
(Const 6) ;
(LAbs ("x",(LAbs ("y",(App ((App ((Const 2),(LVar 0))),(LVar 1))))))) ;
(LAbs ("P",(LAbs ("Q",(App ((Const 9),(Abs ("x",(App ((App ((Const 8),(App ((LVar 1),(Var 0))))),(App ((LVar 0),(Var 0))))))))))))) ;
(LAbs ("P",(LAbs ("Q",(App ((Const 10),(Abs ("x",(App ((App ((Const 7),(App ((LVar 1),(Var 0))))),(App ((LVar 0),(Var 0))))))))))))) ;
(LAbs ("x",(LAbs ("y",(App ((LVar 0),(LVar 1))))))) ;
(LAbs ("s",(LAbs ("a",(LAbs ("S",(App ((LVar 2),(App ((LVar 0),(App ((LVar 1),(LAbs ("x",(App ((Const 4),(LVar 0)))))))))))))))));
(LAbs ("s",(LAbs ("a",(LAbs ("S",(App ((LVar 2),(App ((LVar 0),(LAbs ("x",(App ((App ((LVar 2),(LAbs ("x",(App ((Const 4),(LVar 0))))))),(LVar 0)))))))))))))));
]
*)

let term_list =
  [
    (LAbs ("s",(LAbs ("a",(App ((LVar 1),App (LVar 0,(LAbs ("x",(App ((Const 4),(LVar 0))))))))))));
    (LAbs ("a",(App ((LVar 0),(LAbs ("x",(App ((Const 4),(LVar 0)))))))));(App ((App ((Const 1),(App ((App ((Const 1),(Const 5))),(App ((App ((Const 1),(Const 9))),(App ((App ((Const 1),(Const 10))),(App ((App ((Const 1),(Const 11))),(Const 3))))))))))),(Const 7)));
    App ((Const 1),(Const 11));

  ]

(*let () = test (LAbs ("o",(LAbs ("s",(App ((App ((Const 1),(LVar 0))),(App ((App ((Const 1),(Const 5))),(LVar 1))))))))) *)

let () = List.iter test term_list
