open Lambda
open Program
open Program_printer
open Reduction
open Datalog_solver

(** Ce module donne un executable interactif de test,
la plus grande partie de ce module est empruntée au module Interactive,
si possible, écrire des fonctions de parsing pour parser seulement un type ou seulement un terme pour les utiliser dans ce module (ce serait plus approprié) *)
module Test =
struct

  open Lambda

  module Actual_env = Environment.Make(Acg_lexicon.Sylvain_lexicon)
  module Actual_parser = Data_parser.Make(Actual_env)

  let dirs = ref [""]

  let options = [("-I", Arg.String (fun dir -> dirs := dir::(!dirs)) , " -I dir sets dir as a directory in which file arguments can be looked for")]

  let usg_msg = Printf.sprintf "%s [options] file\n\nThis will test the reduction of second-order ACGs to Datalog." Sys.executable_name

  let env = ref Actual_env.empty

  let print_item s (Datalog_solver.It(i,l)) =
    Printf.printf "%s%!" (Program_printer.print_pred s (Program.Pred(i,l)))

  let parse filename =
    env := Actual_parser.parse_data filename !dirs !env

  (* parse un terme (le type du terme doit aussi lui être fourni, pour l'instant) *)
  let parse_term sg =
    let t = ref None in
    let rec parse_rec = function
      | true ->
	  let () = Printf.printf "Enter a term (and a type): %!" in
	  let term_string = read_line () in
	    (match Actual_parser.parse_term term_string sg with
	       | None -> parse_rec true
	       | Some (ta,_) -> let () = t:= (Some ta) in false )
      | false -> false in
    let () =
      while (parse_rec true) do
	()
      done in
      match !t with
	| Some u -> u
	| _ -> failwith "Strange..."

  (* parse un type (un terme qui lui correspond doit aussi lui être fourni, pour l'instant)*)
  let parse_type sg =
    let t = ref None in
    let rec parse_rec = function
      | true ->
	  let () = Printf.printf "Enter (a term and) a type: %!" in
	  let term_string = read_line () in
	    (match Actual_parser.parse_term term_string sg with
	       | None -> parse_rec true
	       | Some (_,ta) -> let () = t:= (Some ta) in false )
      | false -> false in
    let () =
      while (parse_rec true) do
	()
      done in
      match !t with
	| Some u -> u
	| _ -> failwith "Strange..."

  (* la fonction principale interactive *)
  let term_parsing env =
    let n = Actual_env.sig_number env in
    let m = Actual_env.lex_number env in
    let available_data =
      Utils.string_of_list
	"\n"
	(fun x -> x)
	(Actual_env.fold
	   (fun d a -> 
	     match d with
	       | Actual_env.Signature sg -> (*(Printf.sprintf "\tSignature\t%s%!" (fst (Actual_env.Signature1.name sg)))::a*) a
	       | Actual_env.Lexicon lx -> (Printf.sprintf "\tLexicon\t\t%s%!" (fst (Actual_env.Lexicon.name lx)))::a)
	   []
	   env) in
    let chosen_sig=Actual_env.choose_signature env in
    let chosen_sig_name_loaded =
      match chosen_sig with
	| None -> ""
	| Some s -> Printf.sprintf "Signature \"%s\" loaded.%!" (fst (Actual_env.Signature1.name s))  in
    if n+m=0
    then
      ()
    else
      try
	let () = if (n=1)&&(m=0) then Printf.printf "%s\n%!" chosen_sig_name_loaded else () in
	while true do
	  try
	    let () = Printf.printf "Available data:\n%s\n%!" available_data in
	    let entry =
		  let () = Printf.printf "Enter a name: %!" in
		  let sig_string = read_line () in 
		  Actual_env.get sig_string env in
	    match entry with
	      | Actual_env.Signature sg -> failwith "This is a signature, not a lexicon"
              | Actual_env.Lexicon lex -> let (abs,obj) = Actual_env.Lexicon.get_sig lex in
					  let t = parse_term obj in
					  let ty = parse_type abs in
                                          let (p,d,q) = Actual_reduction.reduction lex t ty in
					  let _ = match p with Program.Prog(s,_) -> Printf.printf "Signature\n%s\n%!" (Program_printer.print_signature s) in
					  let _ = Printf.printf "Program\n%s\n%!" (Program_printer.print_program p) in
					  let _ = Printf.printf "Database :\n%!" in
                                          let _ = List.iter (function x -> print_item (Program.get_signature p) x; Printf.printf "\t") d in
					  let _ = Printf.printf "\n\nQuery :\n%!" in
					  let _ = print_item (Program.get_signature p) q in
					  Printf.printf "\n\n%!"
	  with
	    | Actual_env.Signature_not_found sig_name -> Printf.printf "No such signature in %s\n" sig_name
	done
      with
	| End_of_file -> let () = print_newline () in ()

  let main () =
    let () = Arg.parse options parse usg_msg in
    term_parsing !env

end

let () = Test.main ()


