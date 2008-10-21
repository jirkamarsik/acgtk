(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
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

open Parser
open Environment


let parameter = ref false
let environment = ref Environment.empty
let default_dirs = ref [""]



let spec_list = 
  Arg.align
    [
      "-I",Arg.String (fun dir -> default_dirs:=dir::!default_dirs)," -I dir add dir as directory in which the lookup for parameters file is done";
    ]

let usage_msg =
  Printf.sprintf
    "Usage: \"./test %sfile\" where file is the signature file to be parsed"
    (if spec_list = [] then "" else "[option] ")



let _ = 
  let () =
    Arg.parse
      spec_list
      (fun file_name ->
	 let file = Utils.find_file file_name !default_dirs "No such file" in
	 let () = parameter := true in
	   try
	     let new_env,data = Parser.parse file !environment  in
	     let () = List.iter (fun s -> Printf.printf "%s\n%!" (Parser.to_string s)) data in
	     let _ = read_line () in
	       environment := new_env
	   with
	     | Parser.Parse_Error t -> Printf.fprintf stderr "%s\n%!" t)
      usage_msg in
    ()
      if !parameter then () else Arg.usage spec_list usage_msg
      
