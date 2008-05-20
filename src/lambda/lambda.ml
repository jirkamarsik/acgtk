module Lambda =
  struct
    exception Duplicate_type_definition
    exception Duplicate_term_definition
	
    type type_of_definition =
      | Declared
      | Defined
	  
    type abs =
      | Linear
(*    | Non_linear *)
	  
    type term =
      | Var of int
	    (** If the term is variable (bound by a binder)*)
      | Const of int
	    (** If the term is a constant (not bound by a binder) *)
(*    | Abs of string * term
      (** If the term is a intuitionistic abstraction *) *)
      | LAbs of string * term
	    (** If the term is a linear abstraction *)
      | App of term * term
	    (** If the term is an application *)	
	    
    type type_def = 
      | Type_atom of int * term list
	    (** If the type is atomic. The third parameter is the terms to
		which the type is applied in case of a dependent type. The
		list is empty if the type does not depend on any type *)
      | Linear_arrow of type_def * type_def
	    (** If the type is described with a linear abstraction *)
(*    | Arrow of type_def * type_def * location
      (** If the type is described with a intuitionistic abstraction
       *)
      | Dep of (string * location * type_def) * type_def * location
      (** If the type is a dependent type *)
      | Type_Abs of (string * location * type_def)  * location
      (** If the type is a dependent type build with an abstraction *) *)
	    
    type kind = K of type_def list
	
	    
  end
