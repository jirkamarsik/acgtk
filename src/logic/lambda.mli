open Abstract_syntax
module Lambda : 
sig
  type kind = 
      Type 
    | Depend of stype * kind  (* the kind of a dependant type *)
	
	
  and  stype =
      Atom of int                       (* atomic type *)
    | DAtom of int                      (* defined atomic type *)
    | LFun of stype * stype             (* linear functional type *)
    | Fun of stype * stype              (* non linear functional type *)
    | Dprod of string * stype * stype   (* dependant product *)
    | Record of int * stype list        (* records *)
    | Variant of int * stype list       (* variants *) 
    | TAbs of string * stype            (* type abstraction *)
    | TApp of stype * term              (* type application *)
	
	
  and  term =
      Var of int                (* lambda variable *)
    | LVar of int               (* linear lambda variable *)
    | Const of int              (* constant *)
    | DConst of int             (* defined constant *)
    | Abs of string * term      (* lambda-abstraction *)
    | LAbs of string * term     (* linear lambda abstraction *)
    | App of term * term        (* application *)
    | Rcons of int * term list  (* record constructor:         *)
        (* - the integer is the tag of *)
        (*   the corresponding type.   *)
    | Proj of int * int *term   (* projection:                        *)
        (* - the first integer is the tag of  *)
        (*   the corresponding type;          *)
        (* - the second integer is the index  *)
        (*   of the projection.               *)
    | Vcons of int * int * term (* variant constructor:               *)
        (* - the first integer is the tag of  *)
        (*   the corresponding type;          *)
        (* - the second integer is the number *)
        (*   of the constructor.              *)
    | Case of int * term * (string * term) list
        (* case analysis:              *)      
        (* - the integer is the tag of *)
        (*   the corresponding type.   *)
    | Unknown of int            (* meta-variable - used in higher-order  *)
        (* matching                              *) 

  val kind_to_string :  kind -> (int -> Abstract_syntax.syntactic_behavior * string ) -> string
  val type_to_string : stype -> (int -> Abstract_syntax.syntactic_behavior * string ) -> string
  val term_to_string : term -> (int -> Abstract_syntax.syntactic_behavior * string) -> string
  val normalize : ?id_to_term:(int -> term) -> term -> term

end
