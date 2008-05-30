module Lambda =
  struct
    type term =
      | Var of int
	    (** If the term is variable (bound by a binder)*)
      | LVar of int
	    (** If the term is variable (bound by a binder)*)
      | Const of int
	    (** If the term is a constant (not bound by a binder) *)
      | DConst of int
	    (** If the term is a constant (not bound by a binder) *)
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

	    
    type kind = K of type_def list
	
	    
  end
