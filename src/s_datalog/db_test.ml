open IdGenerator
open Rules

let parse_file filename =
    let in_ch = 
      let fullname = Utils.find_file filename [""]  in
      open_in fullname in
    let lexbuf = Lexing.from_channel in_ch in
    let () = Printf.printf "Parsing \"%s\"...\n%!" filename in
    let proto_rules,pred_id_table,i_preds=Db_parser.program Db_lexer.lexer lexbuf [] (AbstractSyntax.Predicate.PredIdTable.empty,IntIdGen.init()) AbstractSyntax.Predicate.PredIds.empty in 
    let () = Printf.printf "Done.\n%!" in
    let () = AbstractSyntax.Predicate.PredIdTable.print_table pred_id_table in
    let sep=String.make 15 '*' in
    let () = Printf.printf "%s\n" sep in
    let program = AbstractSyntax.Program.make_program proto_rules pred_id_table i_preds in
    let () = AbstractSyntax.Program.print_program program in
    Printf.printf "%s\n" sep
      


let () =
  let () = Printf.printf "Now running!\n%!" in
  parse_file "essai.dl"
