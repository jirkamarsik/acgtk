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

(** This module provides some useful modules or functions *)

(** [StringSet] is a module for sets of [strings] *)
module StringSet : Set.S with type elt = String.t
  
(** [StringMap] is module for maps from [strings] to type ['a] *)
module StringMap : Map.S with type key = String.t

(** [IntMap] is a module for maps from [int] to type ['a] *)
module IntMap : Map.S with type key = int

(** [IntSet] is a module for sets of [int] *)
module IntSet : Set.S with type elt = int
  
(** [string_of_list sep to_string [a_1;...;a_n]] returns a string made
    of the strings [to_string a_1] ... [to_string a_n] concatenated with
    the separator [sep] between each of the elements (if the list is of
    length greater than 2) *)
val string_of_list : string -> ('a -> string) -> ('a list) -> string

(** [string_of_list_rev sep to_string [a_1;...;a_n]] returns a string
    made of the strings [to_string a_n] ... [to_string a_1]
    concatenated with the separator [sep] between each of the elements
    (if the list is of length greater than 2) *)
val string_of_list_rev : string -> ('a -> string) -> ('a list) -> string

(** [No_file (file_name,msg)] is raised when the file [file_name] is
    not found in any of the directories given to the {!Utils.find_file}
    function with the message [msg] *)
exception No_file of  (string * string)


(** [find_file f dirs] tries to find a file with the name [f] in the
    directories listed in [dirs]. If it finds it in [dir], it returns
    the full name [Filename.concat dir f]. To check in the current
    directory, add [""] to the list. It tries in the directories of
    [dirs] in this order and stops when it finds such a file. If it
    can't find any such file, raise the exception [No_file(f,msg)]
    where [msg] contains a string describing where the file [f] was
    looked for.*)
val find_file : string -> string list -> string

val (>>) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val log_iteration : (string -> unit) -> string -> unit
