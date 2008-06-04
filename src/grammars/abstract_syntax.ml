module Abstract_syntax =
struct

  type location = Lexing.position*Lexing.position

  type syntactic_behavior =
    | Default
    | Prefix
    | Infix
    | Binder

  type term =
    | Var of string * location
	(** If the term is variable (bound by a binder)*)
    | Const of string * location
	(** If the term is a constant (not bound by a binder) *)
(*    | Abs of string * term * location
	(** If the term is a intuitionistic abstraction *) *)
    | LAbs of string * term * location
	(** If the term is a linear abstraction *)
    | App of term * term * location
	(** If the term is an application *)	

  type type_def =
    | Type_atom of string * location * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def * location
	(** If the type is described with a linear abstraction *)
(*    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *) *)



  type sig_entry = 
    | Type_decl of (string * location * kind )
	(** The first parameter ([string]) is the name of the type,
	    the second parameter is the place in the file where it was
	    defined *)
    | Type_def of (string * location * type_def)
	(** Tthe first parameter ([string]) is the name of the defined type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
    | Term_decl of (string * syntactic_behavior * location * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its type *)
    | Term_def of (string * syntactic_behavior * location * term * type_def)
	(** The first parameter ([string]) is the name of the constant,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its value *)
  and kind = K of type_def list
    
  type lex_entry = 
    | Type of (string*location * type_def)
    | Constant of (string*location * term)
end



module Token =
struct
  type t =    
    | SYMBOL of (string*Abstract_syntax.location)
    | IDENT of (string*Abstract_syntax.location)
    | LIN_ARROW of (Abstract_syntax.location)
    | COLON_EQUAL of (Abstract_syntax.location)
    | ARROW of (Abstract_syntax.location)
    | LAMBDA0 of (Abstract_syntax.location)
    | LAMBDA of (Abstract_syntax.location)
    | BINDER of (Abstract_syntax.location)
    | INFIX of (Abstract_syntax.location)
    | PREFIX of (Abstract_syntax.location)
    | TYPE of (Abstract_syntax.location)
    | END_OF_DEC of (Abstract_syntax.location)
    | LEX_OPEN of (Abstract_syntax.location)
    | SIG_OPEN of (Abstract_syntax.location)
    | DOT of (Abstract_syntax.location)
    | RPAREN of (Abstract_syntax.location)
    | LPAREN of (Abstract_syntax.location)
    | COMMA of (Abstract_syntax.location)
    | COLON of (Abstract_syntax.location)
    | SEMICOLON of (Abstract_syntax.location)
    | EQUAL of (Abstract_syntax.location)
    | EOI
end	
