let options = []

let usg_msg = ""


let parse filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
    try
      let sg = fst (List.hd (Parser.signature Lexer.lexer lexbuf)) in
	Printf.printf "%s\n" (Abstract_syntax.Abstract_sig.to_string sg)
    with
      | Error.Error e -> Printf.fprintf stderr "Error: %s\n" (Error.error_msg e lexbuf "test.dat")
      | Dyp.Syntax_error -> Printf.fprintf stderr "Dyp: %s\n" (Error.error lexbuf "test.dat")



let () = Arg.parse options parse usg_msg
