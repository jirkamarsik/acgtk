(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

open Functions

let () = Sys.catch_break true


let dirs = ref [""]

let pp_output = ref true


module Lex = Acg_lexicon.Sylvain_lexicon
  
module E = Environment.Make(Lex)

module P = Script_parser.Make(E)

module F = P.F

let options =
  [
    ("-version", Arg.Unit (fun () -> Printf.printf "%s\n" Version.version;exit 0), " Prints the version number");
    ("-I", Arg.String (fun dir -> dirs := (!dirs)@[dir]) , " -I dir sets dir as a directory in which file arguments can be looked for");
    ("-nc"), Arg.Unit (fun () -> F.color_output false), " toggle off the colors on the output";
    ("-npp"), Arg.Unit (fun () -> let () = Utils.no_pp () in pp_output:=false), " toggle off the pretty printing of the output";
  ]

let usg_msg = Printf.sprintf "%s [options] file1 file2 ...\n\nThis will parse the files which are supposed to be files acripting commands and then run the ACG command interpreter." Sys.executable_name



let welcome_msg = 
  Printf.sprintf
    "\n\t\t\tWelcome to the ACG toplevel\n\t\t\t    Version %s\n\t\t\t\t©INRIA 2008-2014\nPlease send your comments or bug reports or featrure requests to sylvain.pogodalla@inria.fr\n\n\nType\n\t\thelp ;\nto get help.\n\n\n\n"
    Version.version


let env = ref E.empty

let resize_terminal () =
  if !pp_output then
    let () = Utils.sterm_set_size () in
    Utils.term_set_size ()
  else
    ()


let anon_fun s =
  let () = resize_terminal () in
  env := P.parse_file s !dirs !env
  
let _ =
  let () = Printf.printf "%s%!" welcome_msg in
  (* ANSITerminal get the size info from stdin In case of redirection,
     the latter may not be set. That's why it is first duplicated and
     stdin is then duplicated from stdout *)
  let stdin_tmp=Unix.dup Unix.stdin in
  let stdin_tmp_in_ch = Unix.in_channel_of_descr stdin_tmp in
  let () = Unix.dup2 Unix.stdout Unix.stdin in
  let () = Arg.parse options anon_fun usg_msg in
  let continue = ref true in
  let () = resize_terminal () in
  let () = 
    while !continue do
      try
	let () = env := P.parse_entry ~resize:!pp_output stdin_tmp_in_ch !dirs !env in
	Format.print_flush ()
      with
      | End_of_file
      | Sys.Break -> continue := false
    done in
  Printf.printf "\n%!"
    
