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

open Signature
open Abstract_syntax
open Base_grammar

module Term_grammar =
struct
  module type Term_grammar =
  sig
    
    include Base_grammar.Base_grammar
      
    type type_or_cst =
      | Type of Abstract_lexicon.type_def
      | Const of Abstract_lexicon.term
	  
    (** The entry that parses lambda terms *)
    val term : Abstract_sig.term Entry.e
    val term_as_parameter : Abstract_sig.term  Entry.e
    val entry : (Signature.t -> type_or_cst) Entry.e
    val add_binder : string -> unit
    val add_infix : string -> unit
    val add_prefix : string -> unit
    val add_outfix: (string*string) -> unit
  end
    
    
  module Make (B:Base_grammar.Base_grammar)=
  struct
    
    include B
      
    type type_or_cst =
      | Type of Abstract_lexicon.type_def
      | Const of Abstract_lexicon.term
	  
    type error =
      | Not_cst_either_type of string
      | Both_cst_and_type of string
      | Mixed of string
      | Type_arg of string
      | Not_atomic_type of string
	  
    let error_to_string err = 
      let msg = match err with
	| Not_cst_either_type s -> Printf.sprintf "\"%s\" is neither a type or a constant of the object signature" s
	| Both_cst_and_type s -> Printf.sprintf "\"%s\" is both a type and a constant" s
      | Mixed s -> Printf.sprintf "Mixed types and constants after \"%s\"" s
      | Type_arg s -> Printf.sprintf "Terms as argument of a type expected after \"%s\"" s
      | Not_atomic_type s -> Printf.sprintf "\"%s\" is a non atomic type expecting terms as arguments" s in
	msg
	  
    exception Parse_Error of string
      
    let send_with_loc loc error = Stdpp.raise_with_loc loc (Parse_Error (error_to_string error))
      
    let term = B.Entry.create "lambda-term"
    let aterm = B.Entry.create "atomic lambda-term"
    let app = B.Entry.create "application"
    let term_as_parameter = B.Entry.create "lambda-term as parameter"
    let entry = B.Entry.create "entry"
    let entryl = B.Entry.create "entry list"
    let varl = B.Entry.create "list of variables"


    let print_info e= 
      let () = Printf.printf "\"%s\" is defined as follows:\n" (B.Entry.name e) in
      B.Entry.print e
      

    let add_binder sym =
      GEXTEND B
	entry:
	[ [ $sym$ ; vl = varl ; "." ; t =term ->
	      (fun sg ->
		 Const (List.fold_right
			  (fun abs term -> Abstract_sig.App (Abstract_sig.Id(sym,loc),Abstract_sig.LAbs (abs,term,loc),loc))
			  vl
			  t)) ] ];
      term:
	[ [ $sym$ ; vl = varl ; "." ; t =term ->
	      List.fold_right
		(fun abs term -> Abstract_sig.App (Abstract_sig.Id(sym,loc),Abstract_sig.LAbs (abs,term,loc),loc))
		vl
		t ] ];
      END;;

    let add_prefix sym =
      GEXTEND B
	entry: LEVEL "simple"
	[ [ $sym$ ; t =term ->
	      (fun sg ->
		 Const (Abstract_sig.App (Abstract_sig.Id(sym,loc), t,loc))) ] ];
      aterm: LEVEL "simple"
	[ [ $sym$ ; vl = varl ; "." ; t =term ->Abstract_sig.App (Abstract_sig.Id(sym,loc),t,loc)] ];
      END;;
    
    let add_infix sym =
      let () =
	GEXTEND B
	  entry: LAST 
	  [ [ t1 = B.luident ; $sym$ ; t2 =aterm ->
		(fun sg -> Const (Abstract_sig.App (Abstract_sig.App (Abstract_sig.Symbol(Abstract_sig.Infix sym,loc),Abstract_sig.Id(t1,loc),loc),t2,loc)))
	    | t1 = aterm ; $sym$ ; t2 =aterm ->
		(fun sg -> Const (Abstract_sig.App (Abstract_sig.App (Abstract_sig.Symbol(Abstract_sig.Infix sym,loc),t1,loc),t2,loc)))] ];
	app:  LEVEL "simple" 
	  [ [ t1 = aterm ; $sym$ ; t2 = aterm ->Abstract_sig.App (Abstract_sig.App (Abstract_sig.Symbol(Abstract_sig.Infix sym,loc),t1,loc),t2,loc) ] ];
	END in
      let () = print_info entry in
      let () = print_info term in
      let () = print_info app in
      let () = print_info aterm in
	()
    
    let add_outfix (sym1,sym2) = 
      let () =
	GEXTEND B
	  entry: LEVEL "simple"
	  [ [ $sym1$ ; t = term ; $sym2$ ->
		(fun sg -> Const  (Abstract_sig.App (Abstract_sig.Symbol(Abstract_sig.Outfix (sym1,sym2),loc),t,loc)))
	    | $sym1$ ; t = term ; $sym2$  ; l=entryl->
		(fun sg -> 
		   Const (List.fold_left
			    (fun term param ->
			       match param with
				 | Const t -> Abstract_sig.App (term,t,loc)
				 | Type _ -> send_with_loc loc   (Mixed ""))
			    (Abstract_sig.Symbol (Abstract_sig.Outfix (sym1,sym2),loc))
			    (l sg)))] ];
	aterm: LEVEL "simple"
	  [ [ $sym1$ ; t = term ; $sym2$ ->
		Abstract_sig.App (Abstract_sig.Symbol(Abstract_sig.Outfix (sym1,sym2),loc),t,loc)] ];
	END in
      let () = print_info entry in
      let () = print_info term in
      let () = print_info app in
      let () = print_info aterm in
	()
  
    GEXTEND B
      GLOBAL : term aterm app term_as_parameter entry entryl varl;
    entry:
      [ "simple" LEFTA
	[ "lambda"; vl = varl; "."; t = term -> 
	     (fun sg -> 
		Const (List.fold_right
			 (fun abs term -> Abstract_sig.LAbs (abs,term,loc))
			 vl
			 t))
	 | "Lambda"; vl = varl; "."; t = term ->
	     (fun sg -> Const (List.fold_right
				 (fun abs term -> Abstract_sig.Abs (abs,term,loc))
				 vl
				 t))
	 | (x,l) = B.luident_loc                          ->
	     (fun sg -> 
		match Signature.is_a_cst x sg, Signature.is_a_type x sg with
		  | true,false -> Const (Abstract_sig.Id (x,loc))
		  | false,true -> Type (Abstract_sig.Type_atom (x,loc,[]))
		  | false,false -> send_with_loc l (Not_cst_either_type x)
		  | true,true -> send_with_loc l (Both_cst_and_type x))
	 | "(" ; e=entry ; ")" -> e
	 | (x,l_loc) = B.luident_loc; l=entryl                          -> 
	     (fun sg -> 
		match Signature.is_a_cst x sg, Signature.is_a_type x sg with
		  | true,false -> 
		      Const (List.fold_left
			       (fun term param ->
				  match param with
				    | Const t -> Abstract_sig.App (term,t,loc)
				    | Type _ -> send_with_loc loc   (Mixed x))
			       (Abstract_sig.Id (x,l_loc))
			       (l sg))
		  | false,true -> 
		      Type (Abstract_sig.Type_atom (x,loc,List.map
						      (fun e ->
							 match e with
							   | Const t -> t
							   | Type _ -> send_with_loc loc  (Type_arg x))
						      (l sg)))
		  | false,false -> send_with_loc l_loc (Not_cst_either_type x)
		| true,true -> send_with_loc l_loc (Both_cst_and_type x))
	 | t1 = atype; "->"; t2 = typ                     -> (fun sg -> Type (Abstract_sig.Linear_arrow (t1,t2,loc)))
	 | t1 = atype; "=>"; t2 = typ                     -> (fun sg -> Type (Abstract_sig.Arrow (t1,t2,loc)))
	 | "("; (x,l) = B.luident_loc; ":"; t1 = typ; ")"; t2 = typ -> (fun sg -> Type (Abstract_sig.Dep((x,l,t1),t2,loc)))
	 | "("; (x,l) = B.luident_loc; ":"; t1 = typ; ","; al = tvarl; ")"; t2 = typ ->
	     (fun sg -> Type (List.fold_right
				(fun (name,l_loc,t) typ -> Abstract_sig.Dep((name,l_loc,t),typ,loc))
				((x,l,t1)::al)
				t2))
	 | "("; (x,l) = B.luident_loc; "->"; t2 = typ; ")"            -> (fun sg -> Type (Abstract_sig.Linear_arrow(Abstract_sig.Type_atom (x,l,[]),t2,loc)))
	 | "("; (x,l) = B.luident_loc; "=>"; t2 = typ; ")"            -> (fun sg -> Type (Abstract_sig.Arrow(Abstract_sig.Type_atom (x,l,[]),t2,loc)))
	 | "("; (x,l) = B.luident_loc; al = argl; "->"; t2 = typ; ")" -> (fun sg -> Type (Abstract_sig.Linear_arrow(Abstract_sig.Type_atom (x,l,al),t2,loc)))
	 | "("; (x,l) = B.luident_loc; al = argl; "=>"; t2 = typ; ")" -> (fun sg -> Type (Abstract_sig.Arrow(Abstract_sig.Type_atom (x,l,al),t2,loc)))
	 | "("; (x,l) = B.luident_loc; al = argl; ")"                 -> (fun sg -> Type (Abstract_sig.Type_atom (x,l,al)))
	 | "(" ; e=entry ; ")" ; l=entryl ->
	     (fun sg -> 
		match e sg with
		  | Const t -> 
		      Const (List.fold_left
			       (fun term param ->
				  match param with
				    | Const t -> Abstract_sig.App (term,t,loc)
				    | Type t -> send_with_loc loc  (Mixed (Abstract_sig.type_def_to_string t)))
			       t
			       (l sg))
		  | Type (Abstract_sig.Type_atom (x,loc,args)) -> 
		      Type (Abstract_sig.Type_atom (x,loc,args@(List.map
								  (fun e ->
								     match e with
								       | Const t -> t
								       | Type _ -> send_with_loc loc  (Type_arg x))
								  (l sg))))
		  | Type t -> send_with_loc loc  (Not_atomic_type (Abstract_sig.type_def_to_string t)))
	 | "("; t1 = atype; "->"; t2 = typ; ")"           -> (fun sg -> Type (Abstract_sig.Linear_arrow (t1,t2,loc)))
	 | "("; t1 = atype; "=>"; t2 = typ; ")"           -> (fun sg -> Type (Abstract_sig.Arrow (t1,t2,loc))) ]];
      entryl:
	[ [ e= entry -> (fun sg -> [e sg])
	  | e=entry ; l=entryl -> (fun sg -> (e sg)::(l sg)) ]];
      term:
	[ "simple"
	  [ "lambda"; vl = varl; "."; t = term ->
	     List.fold_right
	       (fun abs term -> Abstract_sig.LAbs (abs,term,loc))
	       vl
	       t
	 | "Lambda"; vl = varl; "."; t = term ->
	     List.fold_right
	       (fun abs term -> Abstract_sig.Abs (abs,term,loc))
	       vl
	       t
	 | t = app                             -> t
	 ]];
      app:
	[ "simple" LEFTA
	    [
	      t = aterm                           -> t
	    | t = aterm; al = argl                -> 
		List.fold_left
		  (fun term param -> Abstract_sig.App (term,param,loc))
		  t
		  al
	    ]
	];
      aterm:
	[
	  "simple"
	    [ x = B.luident                          -> Abstract_sig.Id (x,loc)
	    | "("; t = term; ")"                 -> t
	    ]];
      argl:
	[[ t = aterm                           -> [t]
	 | t = aterm; al = argl                -> t::al
	 ]];
      varl:
	[[ x = B.luident                          -> [x]
	 | x = B.luident; vl = varl               -> x::vl
	 ]]; 
      term_as_parameter :
	[ [ x = B.luident -> Abstract_sig.Id (x,loc)
	  | "(" ; t=term ; ")" -> t
	  ] ] ;
      typ:
	[[ t1 = atype; "->"; t2 = typ                     -> Abstract_sig.Linear_arrow (t1,t2,loc)
	 | t1 = atype; "=>"; t2 = typ                     -> Abstract_sig.Arrow (t1,t2,loc)
	 | t = atype                                      -> t
	 ]];
      atype:
      [[ x = atom                                       -> x ]
      |[ "("; (x,l) = B.luident_loc; ":"; t1 = typ; ")"; t2 = atype -> Abstract_sig.Dep((x,l,t1),t2,loc)
       | "("; (x,l) = B.luident_loc; ":"; t1 = typ; ","; al = tvarl; ")"; t2 = atype ->
	   List.fold_right
	     (fun (name,l_loc,t) typ -> Abstract_sig.Dep((name,l_loc,t),typ,loc))
	     ((x,l,t1)::al)
	     t2 ]
      |[ "("; (x,l) = B.luident_loc; "->"; t2 = typ; ")"            -> Abstract_sig.Linear_arrow(Abstract_sig.Type_atom (x,l,[]),t2,loc)
       | "("; (x,l) = B.luident_loc; "=>"; t2 = typ; ")"            -> Abstract_sig.Arrow(Abstract_sig.Type_atom (x,l,[]),t2,loc)
       | "("; (x,l) = B.luident_loc; ")"                            -> Abstract_sig.Type_atom (x,l,[]) ]
      |[ "("; (x,l) = B.luident_loc; al = argl; "->"; t2 = typ; ")" -> Abstract_sig.Linear_arrow(Abstract_sig.Type_atom (x,l,al),t2,loc)
	 | "("; (x,l) = B.luident_loc; al = argl; "=>"; t2 = typ; ")" -> Abstract_sig.Arrow(Abstract_sig.Type_atom (x,l,al),t2,loc)
	 | "("; x = B.luident; al = argl; ")"                 -> Abstract_sig.Type_atom (x,loc,al) ]
      |[ "("; t1 = atype; "->"; t2 = typ; ")"           -> Abstract_sig.Linear_arrow (t1,t2,loc)
	 | "("; t1 = atype; "=>"; t2 = typ; ")"           -> Abstract_sig.Arrow (t1,t2,loc)
	 | "("; t = atype; ")"                            -> t]
      ];
    atom:
      [[ x = B.luident                                      -> Abstract_sig.Type_atom (x,loc,[])
       | x = B.luident; al = argl                           -> Abstract_sig.Type_atom (x,loc,al)
       ]];
    tvarl:
      [[ (x,l) = B.luident_loc; ":"; t = typ                        -> [x,l,t]
       | (x,l) = B.luident_loc; ":"; t = typ; ","; al = tvarl       -> (x,l,t)::al
       ]]; 
    END
    
  end
end
