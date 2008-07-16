open Abstract_syntax

type command =
  | Load

type error =
  | Missing_option of command
  | Not_in_environment of string
  | No_such_lexicon of string
  | Command_expected
  | Not_yet_implemented of string

exception Error of (error * Abstract_syntax.location)

let error_msg er (s,e) =
  let loc = Error.compute_comment_for_position s e in
  let msg = match er with
    | Missing_option Load -> "Option (\"data\" or \"d\" or \"script\" or \"s\") is missing to the load command" 
    | Not_in_environment s -> Printf.sprintf "No %s entry in the current environment" s 
    | No_such_lexicon  s -> Printf.sprintf "No lexicon \"%s\" in the current environmnet" s 
    | Command_expected -> "Command expected" 
    | Not_yet_implemented s -> Printf.sprintf "\"%s\": Command not yet implemented" s in
    Printf.sprintf "%s:\n%s\n%!" loc msg
