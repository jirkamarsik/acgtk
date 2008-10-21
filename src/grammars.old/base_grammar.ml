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

module Base_grammar =
struct

  module type Base_grammar =
  sig
    include Grammar.S with type te = Token.t
    exception Parse_Error of string
    val luident : string Entry.e
    val luident_loc : (string*Token.flocation) Entry.e
    val symbol : string list Entry.e
  end
    
    
  module Make(G:Grammar.S with type te = Token.t) =
  struct
    
    include G
      
    exception Parse_Error of string

    exception Stop
      
    let luident = G.Entry.create "identifier"
    let luident_loc = G.Entry.create "identifier"
    let symbol =
      let rec symbol_parser strm =
	match Stream.peek strm with
	  | Some ("",s) when (s <> ":") && (s <> ".") -> let () = Stream.junk strm in
	      s::(try
		    symbol_parser strm
		  with
		    | Stream.Failure -> [])
	  | _ -> raise Stream.Failure in
	G.Entry.of_parser "symbol" symbol_parser

      
      GEXTEND G
      luident :
      [ [ x = UIDENT -> x
	| x = LIDENT -> x ] ];
      luident_loc :
	[ [ x = UIDENT -> x,loc
	  | x = LIDENT -> x,loc ] ];
      END
	
  end
end



