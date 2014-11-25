open Abstract_syntax
open Cairo
open Diagram
open Environment
open Lambda.Lambda

module Make (E : Environment_sig) = struct

  type signature = E.Signature1.t
  type lexicon = E.Lexicon.t
  type term = E.Signature1.term
  type stype = Lambda.Lambda.stype
  type 'a tree = 'a Tree.t
  type id_to_sym_t = int -> Abstract_syntax.syntactic_behavior * string

  let family = "DejaVu Sans"
  let size = 15.
  let font_extents = get_font_extents (font family) size
  let textC ~face ~size contents =
    text_ ~face ~size contents
    |> translateY (-. font_extents.descent)
    |> translateY ((font_extents.ascent +. font_extents.descent) /. 2.)
  
  let n = textC ~face:(font family) ~size
  let b = textC ~face:(font family ~weight:Cairo.Bold) ~size
  let i = textC ~face:(font family ~slant:Cairo.Italic) ~size

  

  let color_from_byte_rgb (r : int) (g : int) (b : int) =
    (float r /. 255., float g /. 255., float b /. 255., 1.)

  let solarized_base03    = color_from_byte_rgb   0  43  54
  let solarized_base02    = color_from_byte_rgb   7  54  66
  let solarized_base01    = color_from_byte_rgb  88 110 117
  let solarized_base00    = color_from_byte_rgb 101 123 131
  let solarized_base0     = color_from_byte_rgb 131 148 150
  let solarized_base1     = color_from_byte_rgb 147 161 161
  let solarized_base2     = color_from_byte_rgb 238 232 213
  let solarized_base3     = color_from_byte_rgb 253 246 227
  let solarized_yellow    = color_from_byte_rgb 181 137   0
  let solarized_orange    = color_from_byte_rgb 203  75  22
  let solarized_red       = color_from_byte_rgb 220  50  47
  let solarized_magenta   = color_from_byte_rgb 211  54 130
  let solarized_violet    = color_from_byte_rgb 108 113 196
  let solarized_blue      = color_from_byte_rgb  38 139 210
  let solarized_cyan      = color_from_byte_rgb  42 161 152
  let solarized_green     = color_from_byte_rgb 133 153   0

  let replace_with_dict : (string * string) list -> string -> string =
    List.fold_right (fun (ugly, pretty) ->
                       Str.global_replace (Str.regexp_string ugly) pretty)

  let prettify : string -> string =
    replace_with_dict [ ("Ex", "∃");
                        ("All", "∀");
                        ("&", "∧");
                        (">", "⇒") ]

  (* Copy of lambda.ml for diagrams... *)

  let parenthesize_d ((d, b) : diagram * bool) : diagram =
    match b with
    | true -> d
    | false -> hcat [n "(";
                     d;
                     n ")" ]

  let rec fix (f : ('a -> 'b) -> ('a -> 'b)) : 'a -> 'b =
    fun x -> f (fix f) x

  let term_to_diagram_open
      (recur_fn : term -> int -> int -> env * env -> id_to_sym_t -> diagram * bool)
      (t : term)
      (l_level : int)
      (level : int)
      ((l_env, env) : env * env)
      (id_to_sym : id_to_sym_t)
      : diagram * bool =
    let recurse t l_level level (l_env,env) =
      recur_fn t l_level level (l_env,env) id_to_sym in
    match t with
    | Var i -> n @@ List.assoc (level - 1 - i) env, true
    | LVar i -> n @@ List.assoc (l_level - 1 - i) l_env, true
    | Const id | DConst id -> n @@ prettify @@ snd @@ id_to_sym id, true
    | Abs (x, t) ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l,u = unfold_abs [(level, x')] (level + 1)
                                  (l_env, (level, x') :: env) t in
          hcat [ n "λ";
                 n @@ Utils.string_of_list " " snd @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_level l (l_env, vars @ env) ],
          false
    | LAbs (x, t) ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l,u = unfold_labs [(l_level, x')] (l_level + 1)
                                   ((l_level, x') :: l_env, env) t in
          hcat [ n "λᵒ";
                 n @@ Utils.string_of_list " " snd @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l level (vars @ l_env, env) ],
          false
    | App ((Const id | DConst id) as binder, Abs(x, u))
      when is_binder id id_to_sym ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l_l,l,u = unfold_binder id l_level (level + 1) id_to_sym
                                         [level,(x',Abstract_syntax.Non_linear)]
                                         (l_env, (level, x') :: env) u in
        let new_env = List.fold_right
                        (fun (l,(x,abs)) (l_acc, acc) ->
                           match abs with
                           | Abstract_syntax.Non_linear -> l_acc, (l, x) :: acc
                           | Abstract_syntax.Linear -> (l, x) :: l_acc, acc)
                        vars (l_env, env) in
          hcat [ parenthesize_d @@ recurse binder l_l l new_env;
                 n " ";
                 n @@ Utils.string_of_list " " (snd >> fst) @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_l l new_env ],
          false
    | App ((Const id | DConst id) as binder, LAbs(x, u))
      when is_binder id id_to_sym ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l_l,l,u = unfold_binder id (l_level + 1) level id_to_sym
                           [l_level,(x',Abstract_syntax.Linear)]
                           ((l_level, x') :: l_env, env) u in
        let new_env = List.fold_right
                        (fun (l,(x,abs)) (l_acc,acc) ->
                           match abs with
                           | Abstract_syntax.Non_linear -> l_acc, (l, x) :: acc
                           | Abstract_syntax.Linear -> (l, x) :: l_acc, acc)
                        vars (l_env, env) in
          hcat [ parenthesize_d @@ recurse binder l_l l new_env;
                 n " ";
                 n @@ Utils.string_of_list " " (snd >> fst) @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_l l new_env ],
          false
    | App(App((Const id | DConst id) as op, t1), t2)
      when is_infix id id_to_sym ->
        hcat [ parenthesize_d @@ recurse t1 l_level level (l_env, env);
               n " ";
               parenthesize_d @@ recurse op l_level level (l_env, env);
               n " ";
               parenthesize_d @@ recurse t2 l_level level (l_env, env) ],
        false
    | App(t1, t2) ->
        let args,fn = unfold_app [t2] t1 in
          hcat @@ [ parenthesize_d @@ recurse fn l_level level (l_env, env);
                    n " "; ]
                  @ Utils.intersperse (n " ") @@
                      List.map (fun x -> parenthesize_d @@
                                 recurse x l_level level (l_env, env)) args,
          false
    | _ -> failwith "Not yet implemented"

  let term_to_diagram (t : term) (id_to_sym : id_to_sym_t) : diagram =
    fst @@ fix term_to_diagram_open t 0 0 ([], []) id_to_sym


  let abstract_sig (lex : lexicon) : signature =
    fst @@ E.Lexicon.get_sig lex

  let object_sig (lex : lexicon) : signature =
    snd @@ E.Lexicon.get_sig lex

  let sig_name (sg : signature) : string =
    fst @@ E.Signature1.name sg

  let interpret_term (t : term) (lex : lexicon) : term =
    E.Lexicon.interpret_term t lex
    |> normalize
       ~id_to_term:(fun i ->
                      E.Signature1.unfold_term_definition i @@ object_sig lex)

  let rec term_to_graph (t : term) : term tree =
    match t with
    | Var _ | LVar _ | Const _ | DConst _ ->
        Tree.T (t, [])
    | Abs (_, body) | LAbs (_, body) ->
        Tree.T (t, [ term_to_graph body ])
    | App (fn, arg) ->
        Tree.T (t, [ term_to_graph fn; term_to_graph arg ])
    | _ -> failwith "Not yet implemented"

  let rec realize_graph (lexs : lexicon list) (tree : term tree) : (term list) tree =
    match tree with
    | Tree.T (term, children) ->
        Tree.T (term :: List.map (interpret_term term) lexs,
                List.map (realize_graph lexs) children)

  let rec unfold_cats (cat_id : int) (t : term) : term list =
    match t with
    | App(App((Const id | DConst id), t1), t2)
      when id = cat_id ->
        unfold_cats cat_id t1 @ unfold_cats cat_id t2
    | _ -> [ t ]

  let simplify_cats default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let recurse t l_level level (l_env, env) =
      recur_fn t l_level level (l_env, env) id_to_sym in
    match t with
    | App(App((Const id | DConst id) as op, t1), t2)
      when is_infix id id_to_sym && ("+" = snd @@ id_to_sym id)->
        let args = unfold_cats id t1 @ unfold_cats id t2 in
        let sep = hcat [ n " ";
                         parenthesize_d @@ recurse op l_level level (l_env, env);
                         n " " ] in
          (args |> List.map (fun arg -> parenthesize_d @@
                               recurse arg l_level level (l_env, env))
                |> Utils.intersperse sep
                |> hcat), false
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym

  let render_constants_with render_fn default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    match t with
    | Const id | DConst id -> render_fn @@ prettify @@ snd @@ id_to_sym id, true
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym

  let logic_const name =
    match name with
    | "∃" | "∀" -> n name
                   |> reframe (fun exts ->
                                { exts with w = exts.w -. (extents (n " ")).w })
    | "∧" | "⇒" -> n name
    | _ -> b name

  let render_with_color c default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let d, b = default_fn recur_fn t l_level level (l_env, env) id_to_sym in
    color c d, b

  let big_parens (d : diagram) : diagram =
    let paren_height = (extents @@ tighten_text @@ n "(").h in
    let d_height = (extents d).h in
    let y_scale = d_height /. paren_height in
    let x_scale = y_scale ** 0.125 in
    let scale_paren p =
      n p |> tighten_text
          |> centerY
          |> scale (x_scale, y_scale)
          |> pad_abs ~left:2. in
    hcat [ scale_paren "(";
           d;
           scale_paren ")"; ]
  
  let tag_style default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let recurse t l_level level (l_env, env) =
      recur_fn t l_level level (l_env, env) id_to_sym in
    match t with
    | App(t1, t2) ->
        let args,fn = unfold_app [t2] t1 in
        let arg_diagrams = List.map (fun x -> fst @@
                                       recurse x l_level level (l_env, env))
                                    args in
          (match fn with
          | Const _ | DConst _ ->
              Tree.T (centerX @@ parenthesize_d @@
                        recurse fn l_level level (l_env, env),
                      List.map (centerX >> Tree.singleton) arg_diagrams)
              |> Tree.to_diagram ~vgap:10.
              |> center
              |> setup (fun cr -> set_line_width cr 1.),
              true
          | Var _ | LVar _ ->
              hcat [ parenthesize_d @@ recurse fn l_level level (l_env, env);
                     big_parens @@ hcat @@ Utils.intersperse (n ", ") arg_diagrams; ],
              false
          | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym)
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym
  

  let rec diagramify_graph (l_level : int) (level : int)
                           (l_env, env : env * env)
                           (sgs : signature list)
                           (tree : (term list) tree) : (diagram list) tree =
    let ttdo sg = term_to_diagram_open
                  |> (match sig_name sg with
                      | "Strings"
                      | "strings" -> simplify_cats >>
                                     render_constants_with i
                      | "logic"
                      | "semantics" -> render_constants_with logic_const
                      (* | "Derivation_trees"
                         | "derivation_trees" *)
                      | "Derived_trees"
                      | "derived_trees" -> tag_style
                      | _ -> fun x -> x)
                  |> fix in
    match tree with
    | Tree.T (((Abs (x, _) :: _) as terms), children) ->
        let x' = generate_var_name x (l_env, env) in
        let env' = (level, x') :: env in
        let level' = level + 1 in
          Tree.T (List.map2 (fun term sg ->
                               fst @@ ttdo sg term l_level level (l_env, env)
                                              (E.Signature1.id_to_string sg))
                            terms sgs,
                  List.map (diagramify_graph l_level level' (l_env, env') sgs)
                           children)
    | Tree.T (((LAbs (x, _) :: _) as terms), children) ->
        let x' = generate_var_name x (l_env, env) in
        let l_env' = (l_level, x') :: l_env in
        let l_level' = l_level + 1 in
          Tree.T (List.map2 (fun term sg ->
                               fst @@ ttdo sg term l_level level (l_env, env)
                                              (E.Signature1.id_to_string sg))
                        terms sgs,
                  List.map (diagramify_graph l_level' level (l_env', env) sgs)
                           children)
    | Tree.T (terms, children) ->
        Tree.T (List.map2 (fun term sg ->
                             fst @@ ttdo sg term l_level level (l_env, env)
                                            (E.Signature1.id_to_string sg))
                          terms sgs,
                List.map (diagramify_graph l_level level (l_env, env) sgs)
                         children)


  let node_to_diagram (lines : diagram list) : diagram =
    lines
    |> List.map (pad_rel ~all:0.05)
    |> List.map2 color @@ Utils.cycle (List.length lines) [ solarized_blue;
                                                            solarized_green;
                                                            solarized_red;
                                                            solarized_violet ]
    |> List.map centerX
    |> vcat
    |> bg_color solarized_base02

  let type_to_diagram (sg : signature) (ty : stype) : diagram =
    type_to_string ty (E.Signature1.id_to_string sg) 
    |> replace_with_dict [ ("->", "⊸");
                           ("=>", "→") ]
    |> n

  let realize_diagram (abs_term : term) (lexs : lexicon list) : diagram =
    let abs_sig = abstract_sig (List.hd lexs) in
    let obj_sigs = List.map object_sig lexs in
    let sigs = abs_sig :: obj_sigs in
    let term_graph = term_to_graph abs_term in
    (* let types = term_graph
                |> Tree.map (TypeInference.Type.inference >> fst)
                |> Tree.map (type_to_diagram abs_sig) in *)
    let obj_terms = term_graph
                    |> realize_graph lexs
                    |> diagramify_graph 0 0 ([], []) sigs
                    |> Tree.map node_to_diagram in
    (* Tree.map2 (===) types obj_terms *)
    obj_terms
    |> Tree.to_diagram
    |> color solarized_magenta
    |> bg_color solarized_base03

end
