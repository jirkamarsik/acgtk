open IdGenerator
open Datalog_AbstractSyntax

(*module Store = (*: UnionFind.Store with type 'a t ='a PersistentArray.PersistentArray.t *)
struct 
(*  module type PA_SIG=module type of PersistentArray.PersistentArray*)
  include PersistentArray.PersistentArray (*: PA_SIG (*with type 'a t = ConstGen.id PersistentArray.PersistentArray.t*)*)
  let empty i =
  let value,_ = ConstGen.get_fresh_id (ConstGen.init ()) in
  init i (fun _ -> value)
  end
*)

module Store =
struct 
  include PersistentArray.PersistentArray
  let empty i =
    let value,_ = ConstGen.get_fresh_id (ConstGen.init ()) in
    init i (fun _ -> value)
end



module Datalog=Unify.Unify(UnionFind.Make(Store))

let parse_file filename =
    let in_ch = 
      let fullname = Utils.find_file filename [""]  in
      open_in fullname in
    let lexbuf = Lexing.from_channel in_ch in
    let () = Printf.printf "Parsing \"%s\"...\n%!" filename in
    let proto_rules,pred_id_table,i_preds,_=Db_parser.program Db_lexer.lexer lexbuf [] (AbstractSyntax.Predicate.PredIdTable.empty,IntIdGen.init(),ConstGen.Table.empty) AbstractSyntax.Predicate.PredIds.empty in 
    let () = Printf.printf "Done.\n%!" in
    let () = AbstractSyntax.Predicate.PredIdTable.print_table pred_id_table in
    let sep=String.make 15 '*' in
    let () = Printf.printf "%s\n" sep in
    let abs_program = AbstractSyntax.Program.make_program proto_rules pred_id_table i_preds in
    let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer abs_program) in
    let () = Printf.printf "%s\n" sep in
(*    let program=Datalog.Program.make_program abs_program in 
    let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract program)) in *)
     Printf.printf "%s\n" sep
      

let usage_msg="Usage: db_test file"

let () =
  Arg.parse [] parse_file usage_msg
