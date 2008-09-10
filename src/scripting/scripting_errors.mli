type command =
  | Load

type error =
  | Missing_option of command
  | Not_in_environment of string
  | No_such_lexicon of string
  | Command_expected
  | Not_yet_implemented of string
  | No_focus


exception Error of (error * (Lexing.position * Lexing.position))


val error_msg : error -> (Lexing.position * Lexing.position) -> string

