(*#use "signature.ml";;
#use "string_map.ml";;
#use "int_map.ml";;
#use "int_set.ml";;
#use "program.ml";;
#use "pmcfg.ml";;
#use "pmcfg_to_datalog.ml";;
#use "oriented_pmcfg.ml";;
#use "lexicalizer.ml";;
#use "pmcfg_syn.ml";;
#use "program_printer.ml";;
#use "prefix_correct_program.ml";;
#use "adornment2.ml";;
#use "magic_set_rewritting2.ml";;
#use "kanazawa_transform.ml";;
#use "datalog_solver.ml";;*)

open Pmcfg_syn
open Kanazawa_transform
open Program_printer
open Datalog_solver

(*opening the file containing the grammar, here gram.pmcfg*)
let ch_in = open_in "kan.pmcfg";;
(*parsing the grammar*)
let grammar = PMCFG_syn.parse (Stream.of_channel ch_in);;
(*transforming the grammar *)
(*Kanazawa_transform.transform_pmcfg_1 is for simple equality*)
(*Kanazawa_transform.transform_pmcfg_2 is for structural equality*)
let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_1 grammar;;
(*let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_2 grammar;;*)
(*printing the resulting datalog program*)
print_string (Program_printer.print_program program);;
(*sentence to analyse as a list of extensional predicate*)
(*the list [a1;...;an] is translated in the extensional db a1(0,1),...,an(n-1,n)*)
(*NB: ["ab"] is different from ["a";"b"]*)
let sentence = ["a1";"a2";"b1";"b2";"a3";"a4";"b3";"b4"];;
(*analysing sentence*)
let mem = Datalog_solver.solve2 program sentence magic;;
(*presenting the chart in a stepwise fashion*)
let res = Datalog_solver.print_chart mem;;
