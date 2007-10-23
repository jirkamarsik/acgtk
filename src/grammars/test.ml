let options = []

let usg_msg = ""


let parse filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
    try
      let sgs = fst (List.hd (Parser.signatures Lexer.lexer lexbuf)) in
	List.iter (fun sg -> Printf.printf "%s\n" (Abstract_syntax.Abstract_sig.to_string sg)) sgs
    with
      | Error.Error e -> Printf.fprintf stderr "Error: %s\n" (Error.error_msg e lexbuf "test.dat")
      | Dyp.Syntax_error -> Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf "test.dat")



let () = Arg.parse options parse usg_msg
