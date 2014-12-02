open Cairo
open Diagram
open Lambda.Lambda
open Show_exts


module Make (T : Show_text_sig) : Show_embellish_sig = struct

  open T

  module L = Show.Lambda_show(T)
  open L

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
    | Const id | DConst id -> render_fn @@ snd @@ id_to_sym id, true
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym

  let logic_const name =
    let name = match name with
               | "Ex" -> "∃"
               | "All" -> "∀"
               | "The" -> "ι"
               | "&" -> "∧"
               | ">" -> "⇒"
               | _ -> name in
    match name with
    | "∃"
    | "∀"
    | "ι" -> n name
             |> reframe (fun exts ->
                           { exts with w = exts.w -. (extents (n " ")).w })
    | "∧"
    | "⇒" -> n name
    | _ -> b name

  let string_const = function
    | "e" -> n "ε"
    | name -> i name

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
              Tree.T (parenthesize_d @@
                        recurse fn l_level level (l_env, env),
                      List.map Tree.singleton arg_diagrams)
              |> Tree.to_diagram ~vgap:10.
              |> centerY
              |> setup (fun cr -> set_line_width cr 1.),
              true
          | Var _ | LVar _ ->
              hcat [ parenthesize_d @@ recurse fn l_level level (l_env, env);
                     big_parens @@ hcat @@ Utils.intersperse (n ", ") arg_diagrams; ]
              |> centerX,
              false
          | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym)
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym
 


  let embellishments = function
    | "Strings"
    | "strings"
    | "anglais"
    | "francais" -> simplify_cats >> render_constants_with string_const
    | "logic"
    | "logique"
    | "semantics" -> render_constants_with logic_const
    | "Derivation_trees"
    | "derivation_trees"
    | "Derived_trees"
    | "derived_trees" -> tag_style
    | _ -> fun x -> x

end
