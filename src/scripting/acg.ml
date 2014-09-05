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


let welcome_string = "Welcome to the ACG toplevel"
let version_string = Printf.sprintf "Version %s" Version.version
let copyright_string = "©INRIA 2008-2014"
let bug_string = "Please send your comments or bug reports or feature requests to sylvain.pogodalla@inria.fr"


let env = ref E.empty

let resize_terminal () =
  if !pp_output then
    let () = Utils.sterm_set_size () in
    Utils.term_set_size ()
  else
    ()

let welcome_msg () =
  let () = resize_terminal () in
  let l = 
    let l = Format.get_margin () in
    if l > 1000 then
      ((String.length bug_string)+10)
    else
      l in
  let _ = Format.flush_str_formatter () in
  let () = Format.printf "@[<v>" in
  let () = 
    List.iter
      (fun s -> 
	let prepend =
	  try
	    (String.make ((l-(String.length s))/2) ' ')
	  with
	  | Invalid_argument _ -> "" in
	Format.printf "@[<h>%s%s@]@," prepend s)
      [welcome_string;version_string;copyright_string;bug_string] in
  Format.printf "@,@[<v>@[<v15>Type@,help;@]@,@[to@ get@ help.@]@]@,@]@.@?"
    
    
let printed_welcome_message = ref false

let print_welcome_message () = 
  if !printed_welcome_message then
    ()
  else
    let () = welcome_msg ()in
    printed_welcome_message := true


let anon_fun s =
  let () = print_welcome_message () in
  let () = resize_terminal () in
  env := P.parse_file s !dirs !env
  
let _ =
  (* ANSITerminal get the size info from stdin In case of redirection,
     the latter may not be set. That's why it is first duplicated and
     stdin is then duplicated from stdout *)
  let stdin_tmp=Unix.dup Unix.stdin in
  let stdin_tmp_in_ch = Unix.in_channel_of_descr stdin_tmp in
  let () = Unix.dup2 Unix.stdout Unix.stdin in
  let () = Arg.parse options anon_fun usg_msg in
  let () = print_welcome_message () in
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
    
