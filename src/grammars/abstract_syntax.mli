open Tries

(** This modules implements the abstract syntax and the build function for the signatures *)

module Abstract_sig :
sig

  (** The type of location in the signature files *)
  type location = Token.flocation
      
      
  (** The type of terms provided by the parser. Note their is no
      distinction between variables and constants, as we require this to
      be made by the signature *)
  type symbol =
    | Prefix of string
    | Infix of string
    | Outfix of string*string
    | Binder of string
  type term =
    | Id of string * location
	(** If the term is either a constant or a variable (not to
	    determine it during parsing) *)
    | Symbol of symbol * location
	(** If the term is a symbol *)
    | Abs of string * term * location
	(** If the term is a intuitionistic abstraction *)
    | LAbs of string * term * location
	(** If the term is a linear abstraction *)
    | App of term * term * location
	(** If the term is an application *)	

	
  (** The type of types as found in the signature files *)

  type type_def =
    | Type_atom of string * location * term list
	(** If the type is atomic. The third parameter is the terms to
	    which the type is applied in case of a dependent type. The
	    list is empty if the type does not depend on any type *)
    | Linear_arrow of type_def * type_def * location
	(** If the type is described with a linear abstraction *)
    | Arrow of type_def * type_def * location
	(** If the type is described with a intuitionistic abstraction
	*)
    | Dep of (string * location * type_def) * type_def * location
	(** If the type is a dependent type *)
    | Type_Abs of (string * location * type_def)  * location
	(** If the type is a dependent type build with an abstraction *)



  (** The type of kinds as found in the signature files *)
  type kind = K of type_def list

  (** The type of the signature as abstract object *)
  type t = Signature of string * sig_content (** The first string is the name of the signature *)
  and sig_content = sig_entry list
  and sig_entry = 
    | Type_decl of (string * location * kind)
	(** The first parameter ([string]) is the name of the type,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its kind *)
    | Term_decl of (string * location * type_def)
	(** The first parameter ([string]) is the name of the constat,
	    the second parameter is the place in the file where it was
	    defined and the last parameter is its type *)


  (** [empty name] returns the empty signature of name [name] *)
  val empty : string -> t
    
  (** [insert_type_decl id types loc sig] returns a signature where
      the type [id] has been added as type depending of [types] to the
      signature [sig] *)
  val insert_type_decl : string -> type_def list -> location -> t -> t
    
  (** [insert_term_decl id type loc sig] returns a signature where the
      constant [id] of type [type] has been added to the signature [sig]
  *)
  val insert_term_decl : string ->  type_def -> location -> t -> t

  (** [to_string sg] returns a string describing the signature
    [sg]. Should be parsable *)
  val to_string : t -> string
    
  (** [term_to_string t] returns a string describing the term [t]. *)
  val term_to_string : term -> string

  (** [type_def_to_string t ] returns a string describing the type definition [t] *)
  val type_def_to_string : type_def -> string

  (** [extract_term_location t] returns the location of the term [t]. *)
  val extract_term_location : term -> Token.flocation

  (** [extract_type_location t] returns the location of the type definiton [t]. *)
  val extract_type_location : type_def -> Token.flocation
end

module Abstract_lexicon :
sig


  (** The type of location in the signature files *)
  type location = Token.flocation
      
      
  (** The type of terms provided by the parser. Note their is no
      distinction between variables and constants, as we require this to
      be made by the signature *)
  type term = Abstract_sig.term

	
  (** The type of types as found in the signature files *)
  type type_def = Abstract_sig.type_def

  (** The type of the signature as abstract object *)
  type t = Lexicon of string * string * string * lex_content
    (** The first string is the name of the lexicon, the second is the
	name of the abstract signature and the last one is the name of the
	object signature *)
  and lex_content = lex_assignment Tries.t
  and lex_assignment = 
    | Type_assgt of string * location * type_def
	(** The first parameter of type [string] is the name of the
	    abstract type, the second parameter is the place where it is
	    defined, and the third parameter is its realization as type in
	    the object signature *)
    | Const_assgt of string * location * term
	(** The first parameter of type [string] is the name of the
	    abstract constant, the second parameter is the place where it
	    is defined, and the third parameter is its realization as term
	    in the object signature *)
	

  (** [empty name abs_sig obj_sig] returns the empty lexicon of name
      [name] from the abstract signature [abs_sig] to the object signature
      [obj_sig] *)
  val empty : string -> string -> string -> t
    
  (** [insert_type_assgt id type loc lex] returns a lexicon where the
      type [id] has been assigned the type [type] at place [loc] to the
      lexicon [lex] *)
  val insert_type_assgt : string -> type_def -> location -> t -> t
    

  (** [insert_cst_assgt id t loc lex] returns a lexicon where the
      constant [id] has been assigned the term [t] at place [loc] to the
      lexicon [lex] *)
  val insert_const_assgt : string ->  term -> location -> t -> t

  (** [to_string sg] returns a string describing the signature. Should
      be parsable *)
  val to_string : t -> string
end

