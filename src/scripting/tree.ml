open Cairo
open Diagram

type 'a t = T of 'a * ('a t) list

let rec map (f : 'a -> 'b) : 'a t -> 'b t = function
  | T (node, children) -> T (f node, List.map (map f) children)

let rec map2 (f : 'a -> 'b -> 'c) (t1 : 'a t) (t2 : 'b t) : 'c t =
  match (t1, t2) with
  | (T (root1, children1), T (root2, children2)) ->
      T (f root1 root2, List.map2 (map2 f) children1 children2)

let singleton (root : 'a) : 'a t =
  T (root, [])

let rec to_diagram ?(hgap = 10.) ?(vgap = 20.) (t : diagram t) : diagram =
  match t with
  | T (node, []) -> node
  | T (node, children) ->

      let child_diagrams = List.map (to_diagram >> alignT) children in

      let child_exts = List.map extents child_diagrams in

      let rec hcat_centers (first : rectangle) (more : rectangle list) : float list =
        match more with
        | [] -> []
        | next :: rest ->
            let offset = first.x +. first.w +. hgap -. next.x in
            offset :: hcat_centers { first with w = first.w +. hgap +. next.w }
                                   rest in

      let child_centers = 0. :: hcat_centers (List.hd child_exts)
                                             (List.tl child_exts) in
 
      let n_children = List.length children in

      let child_row_offset =
        if n_children mod 2 = 0 then
          (  List.nth child_centers (n_children / 2)
          +. List.nth child_centers (n_children / 2 - 1))
          /. 2.
        else
          List.nth child_centers (n_children / 2) in

      let child_row = child_diagrams
                      |> Utils.intersperse (hspace hgap)
                      |> hcat
                      |> translateX (-. child_row_offset) in

      let child_centers = List.map (fun c -> c -. child_row_offset) child_centers in

      let antlers = blend (List.map (fun c -> line (0., 0.) (c, vgap)) child_centers) in

      vcat [ node;
             antlers;
             child_row ]
