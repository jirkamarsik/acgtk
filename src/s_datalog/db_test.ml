(*let () =
  Bolt.Logger.register
    ""
    Bolt.Level.TRACE
    "all"
    "default"
    (Bolt.Mode.direct ())
    "file"
    ("db_test.log",
     {Bolt.Output.seconds_elapsed= Some 1.0;
      Bolt.Output.signal_caught=None})
*)

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

module Store = UnionFind.StoreAsMap
(*struct 
  include PersistentArray.PersistentArray
  let empty i =
    let value,_ = ConstGen.get_fresh_id (ConstGen.init ()) in
    init i (fun _ -> value)
end
*)


module Datalog=Datalog.Make(Store)

let parse_file filename =
    let in_ch = 
      let fullname = Utils.find_file filename [""]  in
      open_in fullname in
    let lexbuf = Lexing.from_channel in_ch in
    LOG "Parsing \"%s\"..." filename LEVEL INFO;
    let prog=Db_parser.program Db_lexer.lexer lexbuf AbstractSyntax.Proto_Program.empty in 
    LOG "Done." LEVEL INFO;
    LOG "Current symbol tables:" LEVEL DEBUG ;
    let () = 
      List.iter
	(fun s -> LOG s LEVEL DEBUG)
	(Bolt.Utils.split "\n" (AbstractSyntax.Predicate.PredIdTable.to_string prog.AbstractSyntax.Proto_Program.pred_table)) in
    let sep=String.make 15 '*' in
    let () = Printf.printf "%s\n%!" sep in
    let () = Printf.printf "Create the abstract program and print it...\n" in
    let abs_program = AbstractSyntax.Program.make_program prog in
    let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer abs_program) in
    let () = Printf.printf "Done.\n" in
    let () = Printf.printf "%s\n" sep in
    let () = Printf.printf "Create the internal program and print it...\n" in
    let program=Datalog.Program.make_program abs_program in 
    let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract program)) in 
    let () = Printf.printf "Done.\n" in
    let () = Printf.printf "%s\n" sep in
    let derived_facts,derivations = Datalog.Program.seminaive program in
    let () = Printf.printf "I could derive the following facts:\n%s\n" (Datalog.Predicate.facts_to_string derived_facts program.Datalog.Program.pred_table program.Datalog.Program.const_table) in
    let buff = Buffer.create 80 in
    let () = Datalog.Predicate.add_map_to_premises_to_buffer buff program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
    let () = Printf.printf "With the following derivations:\n%s\n" (Buffer.contents buff) in
    let () = Datalog.Predicate.format_derivations2 program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
    let () = Printf.printf "%s\n" (Buffer.contents (Format.stdbuf)) in
    
    ()
      

let usage_msg="Usage: db_test file"

let () =
  Arg.parse [] parse_file usage_msg
